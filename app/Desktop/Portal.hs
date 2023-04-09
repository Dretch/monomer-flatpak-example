module Desktop.Portal
  ( Request,
    GetUserInformationResponse (..),
    OpenFileOptions (..),
    getUserInformation,
    openFile,
    await,
    cancel,
  )
where

import Control.Concurrent (MVar, readMVar, tryPutMVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Exception (onException, throwIO)
import Control.Monad (when)
import DBus (BusName, InterfaceName, MemberName, MethodCall, ObjectPath)
import DBus qualified
import DBus.Client (Client, MatchRule (..))
import DBus.Client qualified as DBus
import DBus.Internal.Message (Signal (..))
import DBus.Internal.Types (Variant)
import Data.Default.Class (Default (def))
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Word (Word32, Word64)
import System.Random.Stateful qualified as R

data GetUserInformationResponse = GetUserInformationResponse
  { userId :: Text,
    userName :: Text,
    userImage :: Text
  }
  deriving (Eq, Show)

data OpenFileOptions = OpenFileOptions
  {title :: Maybe Text}
  deriving (Eq, Show)

instance Default OpenFileOptions where
  def = OpenFileOptions {title = Nothing}

-- | A request that may be in-progress, finished, or cancelled.
data Request a = Request
  { client :: Client,
    methodCall :: MethodCall,
    result :: MVar (Maybe a)
  }

instance Eq (Request a) where
  a == b = a.result == b.result

instance Show (Request a) where
  show request =
    "Request{client=<"
      <> show request.client.clientThreadID
      <> ">, methodCall="
      <> show request.methodCall
      <> ", result=<MVar>}"

-- | Wait for a request to be finished, and return the result if it succeeded.
await :: Request a -> IO (Maybe a)
await request = do
  readMVar request.result

-- | Cancel a request. This will cause any threads blocked on 'await' to receive 'Nothing'.
cancel :: Request a -> IO ()
cancel request = do
  putSucceeded <- tryPutMVar request.result Nothing
  when putSucceeded $ do
    -- Otherwise the request was already finished/cancelled. Don't bother calling
    -- the Close method on the request because disconnection has the same effect.
    DBus.disconnect request.client

getUserInformation :: Text -> IO (Request GetUserInformationResponse)
getUserInformation reason =
  sendRequest "org.freedesktop.portal.Account" "GetUserInformation" [windowHandle] options parseResponse
  where
    windowHandle = DBus.toVariant ("" :: Text)
    options = Map.singleton "reason" (DBus.toVariant reason)

    parseResponse = \case
      resMap
        | Just userId <- DBus.fromVariant =<< Map.lookup "id" resMap,
          Just userName <- DBus.fromVariant =<< Map.lookup "name" resMap,
          Just userImage <- DBus.fromVariant =<< Map.lookup "image" resMap ->
            pure GetUserInformationResponse {..}
      _ ->
        throwIO (DBus.clientError "getUserInformation: could not parse response")

openFile :: OpenFileOptions -> IO (Request [Text])
openFile ofOptions =
  sendRequest "org.freedesktop.portal.FileChooser" "OpenFile" [windowHandle, DBus.toVariant title] mempty parseResponse
  where
    windowHandle = DBus.toVariant ("" :: Text)
    title = fromMaybe "" ofOptions.title

    parseResponse = \case
      resMap
        | Just (uris :: [Text]) <- DBus.fromVariant =<< Map.lookup "uris" resMap ->
            pure uris
      _ ->
        throwIO (DBus.clientError "openFile: could not parse response")

sendRequest ::
  InterfaceName ->
  MemberName ->
  [Variant] ->
  Map Text Variant ->
  (Map Text Variant -> IO a) ->
  IO (Request a)
sendRequest interface memberName parameters options parseResponse = do
  (client, clientName) <- connect
  onException (sendRequest' client clientName) (DBus.disconnect client)
  where
    sendRequest' client clientName = do
      (handle, token) <- requestHandle clientName
      resultVar <- newEmptyMVar

      -- listen before sending the request, to avoid a race condition where the
      -- response happens before we get a chance to register the listener for it
      _ <-
        DBus.addMatch
          client
          DBus.matchAny
            { matchPath = Just handle,
              matchInterface = Just "org.freedesktop.portal.Request",
              matchMember = Just "Response"
            }
          ( \Signal {signalBody} -> do
              val <- case signalBody of
                [code, result]
                  | Just (0 :: Word32) <- DBus.fromVariant code,
                    Just (resMap :: Map Text Variant) <- DBus.fromVariant result -> do
                      Just <$> parseResponse resMap
                _ -> do
                  pure Nothing
              -- need to try because cancel might have been called and populated the mvar with Nothing
              putSucceeded <- tryPutMVar resultVar val
              when putSucceeded $
                DBus.disconnect client -- we only use the connection for a single request, which is now done
          )

      let methodCall =
            (DBus.methodCall "/org/freedesktop/portal/desktop" interface memberName)
              { DBus.methodCallDestination = Just "org.freedesktop.portal.Desktop",
                DBus.methodCallBody =
                  parameters <> [DBus.toVariant (Map.insert "handle_token" (DBus.toVariant token) options)]
              }

      reply <- DBus.call_ client methodCall

      case DBus.methodReturnBody reply of
        [x]
          | Just (objX :: ObjectPath) <- DBus.fromVariant x ->
              if objX == handle
                then pure (Request client methodCall resultVar)
                else
                  let msg = "Unexpected handle: " <> show objX <> " should be " <> show handle <> ". Probably xdg-desktop-portal is too old."
                   in throwIO (DBus.clientError msg)
        _ ->
          throwIO (DBus.clientError ("Request reply in unexpected format: " <> show reply))

connect :: IO (Client, BusName)
connect = do
  env <- DBus.getSessionAddress
  case env of
    Nothing -> throwIO (DBus.clientError "connect: session address not found.")
    Just addr -> DBus.connectWithName DBus.defaultClientOptions addr

requestToken :: IO Text
requestToken = do
  (rnd :: Word64) <- R.uniformM R.globalStdGen
  pure ("monomer_flatpak_example_" <> pack (show rnd))

requestHandle :: BusName -> IO (ObjectPath, Text)
requestHandle clientName = do
  token <- requestToken
  pure (DBus.objectPath_ ("/org/freedesktop/portal/desktop/request/" <> escapeClientName clientName <> "/" <> unpack token), token)
  where
    escapeClientName =
      map (\case '.' -> '_'; c -> c) . drop 1 . DBus.formatBusName
