-- | A high-level (no raw pointer types) interface to PipeWire.
module PipeWire
  ( SpaDictMap (..),
    RegistryObject (..),
    PipeWireCallbacks (..),
    PipeWireClient,
    run,
    quit,
    waitForQuit,
  )
where

import Bindings.PipeWire (DictPtr, MainLoop, Properties (..), RegistryEvents (..))
import Bindings.PipeWire qualified as B
import Bindings.Util qualified as B
import Control.Concurrent (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, tryTakeMVar)
import Control.Concurrent.Async (Async, asyncBound, asyncThreadId, wait)
import Control.Exception (bracket)
import Control.Monad (forM, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Foreign (allocaBytes, freeHaskellFunPtr, nullPtr, peek, peekArray)
import System.Posix.Types (Fd (..))
import Unsafe.Coerce (unsafeCoerce)

data SpaDictMap = SpaDictMap
  { flags :: Word,
    items :: Map Text Text
  }
  deriving (Eq, Show)

data RegistryObject = RegistryObject
  { id :: Word,
    permissions :: Word,
    typ :: Text,
    version :: Word,
    props :: SpaDictMap
  }
  deriving (Eq, Show)

data PipeWireCallbacks = PipeWireCallbacks
  { cameraAdded :: Map Word RegistryObject -> RegistryObject -> IO (),
    cameraRemoved :: Map Word RegistryObject -> Word -> IO ()
  }

data PipeWireClient = PipeWireClient
  { async :: Async (),
    mainLoop :: MVar MainLoop
  }

instance Eq PipeWireClient where
  p1 == p2 = p1.async == p2.async

instance Show PipeWireClient where
  show p = "PipeWireClient<" <> show (asyncThreadId p.async) <> ">"

run :: Fd -> PipeWireCallbacks -> IO PipeWireClient
run (Fd fd) cbs = do
  loopVar <- newEmptyMVar
  camerasVar <- newMVar Map.empty

  async <- asyncBound $ do
    bracket (B.registryEventsGlobalCallback (addCb camerasVar)) freeHaskellFunPtr $ \globalCallback -> do
      bracket (B.registryEventsGlobalRemoveCallback (removeCb camerasVar)) freeHaskellFunPtr $ \globalRemoveCallback -> do
        let events =
              RegistryEvents
                { version = B.versionRegistryEvents,
                  globalCallback,
                  globalRemoveCallback
                }

        B.init nullPtr nullPtr
        mainLoop <- B.mainLoopNew nullPtr
        putMVar loopVar mainLoop
        loop <- B.mainLoopGetLoop mainLoop
        context <- B.contextNew loop (Properties nullPtr) 0
        core <- B.contextConnectFd context fd (Properties nullPtr) 0

        allocaBytes B.spaHookSize $ \listenerHandle -> do
          registry <- B.coreGetRegistry core B.versionRegistry 0
          B.registryAddListener registry listenerHandle events
          void (B.mainLoopRun mainLoop)
          B.proxyDestroy (unsafeCoerce registry)

        void (B.coreDisconnect core)
        B.contextDestroy context
        B.mainLoopDestroy mainLoop
        B.deinit
  void (readMVar loopVar) -- don't return until we can call quit safely...
  pure PipeWireClient {mainLoop = loopVar, async}
  where
    addCb camerasVar _data camId permissions typPtr version propsPtr = do
      typ <- B.cStringToText typPtr
      props <- decodeDict propsPtr
      let registryObj =
            RegistryObject
              { id = fromIntegral camId,
                permissions = fromIntegral permissions,
                typ,
                version = fromIntegral version,
                props
              }
      when (isCamera registryObj) $ do
        modifyMVar_ camerasVar $ \cameras -> do
          let newCameras = Map.insert registryObj.id registryObj cameras
          cbs.cameraAdded newCameras registryObj
          pure newCameras

    removeCb camerasVar _data camId = do
      let wordId = fromIntegral camId
      modifyMVar_ camerasVar $ \cameras -> do
        case Map.lookup wordId cameras of
          Just registryObj | isCamera registryObj -> do
            let newCameras = Map.delete wordId cameras
            cbs.cameraRemoved newCameras wordId
            pure newCameras
          _ -> do
            pure cameras

quit :: PipeWireClient -> IO ()
quit client = do
  tryTakeMVar client.mainLoop >>= \case
    Nothing -> pure () -- we already quit
    Just ml -> void (B.mainLoopQuit ml)

waitForQuit :: PipeWireClient -> IO ()
waitForQuit client = do
  wait client.async

isCamera :: RegistryObject -> Bool
isCamera obj =
  Map.lookup "media.class" obj.props.items == Just "Video/Source"
    && Map.lookup "media.role" obj.props.items == Just "Camera"

decodeDict :: DictPtr -> IO SpaDictMap
decodeDict dictPtr = do
  dict <- peek dictPtr
  items <- peekArray (fromIntegral dict.nItems) dict.items
  itemPairs <- forM items $ \item -> do
    (,)
      <$> B.cStringToText item.key
      <*> B.cStringToText item.value
  pure
    SpaDictMap
      { flags = fromIntegral dict.flags,
        items = Map.fromList itemPairs
      }
