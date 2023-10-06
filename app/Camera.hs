module Camera (camera) where

import Control.Exception (SomeException, catch)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, intercalate, pack)
import Desktop.Portal (Client)
import Desktop.Portal qualified as Portal
import Monomer
import PipeWire (PipeWireCallbacks (..), PipeWireClient, RegistryObject (..))
import PipeWire qualified
import System.Posix.Types (Fd)

data CameraModel = CameraModel
  { portalClient :: Client,
    state :: CameraState
  }
  deriving (Eq, Show)

data CameraState
  = StateCheckingPresent
  | StateNotFound
  | StateAccessing
  | StateAccessDenied
  | StateOpeningPipeWire
  | StateOpenedPipeWire Fd
  | StateConnectedToPipewire PipeWireClient (Map Word RegistryObject)
  deriving (Eq, Show)

data CameraEvent
  = Init
  | Dispose
  | SetState CameraState
  | ShowAlert {title :: Text, body :: Text}
  | CamerasChanged (Map Word RegistryObject)

camera :: (CompParentModel s, CompositeEvent e) => Client -> (Text -> Text -> e) -> WidgetNode s e
camera portalClient parentAlert =
  compositeD_ "MonomerFlatpakExample.Camera" (WidgetValue initialModel) buildUI (handleEvent parentAlert) cfg
  where
    cfg = [onInit Init, onDispose Dispose]
    initialModel = CameraModel {portalClient, state = StateCheckingPresent}

buildUI :: UIBuilder CameraModel CameraEvent
buildUI _wenv model =
  label_ (lbl model.state) [multiline] `styleBasic` [padding 40]
  where
    lbl = \case
      StateCheckingPresent ->
        "Checking for camera..."
      StateAccessing ->
        "Requesting access to camera..."
      StateNotFound ->
        "Camera not found."
      StateAccessDenied ->
        "Camera access denied."
      StateOpeningPipeWire ->
        "Opening pipewire..."
      StateOpenedPipeWire fd ->
        "Opened pipewire: " <> pack (show fd)
      StateConnectedToPipewire _client cameras ->
        "Found cameras:\n\n" <> formatCameras cameras

formatCameras :: Map Word RegistryObject -> Text
formatCameras cameras =
  (formatCamera <$> Map.elems cameras)
    & intercalate "\n\n"
  where
    formatCamera c =
      Map.assocs c.props.items
        & fmap (\(key, value) -> key <> ": " <> value)
        & intercalate "\n"

handleEvent :: (Text -> Text -> ep) -> EventHandler CameraModel CameraEvent sp ep
handleEvent parentAlert _env _node model = \case
  Init ->
    [ Producer $ \emit -> do
        catchErrors "Checking for camera failed" emit $ do
          Portal.isCameraPresent model.portalClient >>= \case
            False -> emit (SetState StateNotFound)
            True -> do
              emit (SetState StateAccessing)
              Portal.accessCamera model.portalClient >>= Portal.await >>= \case
                Nothing -> emit (SetState StateAccessDenied)
                Just () -> do
                  emit (SetState StateOpeningPipeWire)
                  fd <- Portal.openPipeWireRemote model.portalClient
                  emit (SetState (StateOpenedPipeWire fd))
                  let cbs =
                        PipeWireCallbacks
                          { cameraAdded = \cameras _ -> emit (CamerasChanged cameras),
                            cameraRemoved = \cameras _ -> emit (CamerasChanged cameras)
                          }
                  pwClient <- PipeWire.run fd cbs
                  emit (SetState (StateConnectedToPipewire pwClient mempty))
                  -- need to keep thread alive so monomer keeps accepting our events!
                  PipeWire.waitForQuit pwClient
    ]
  Dispose ->
    [ Producer $ \_emit -> do
        case model.state of
          StateConnectedToPipewire pwClient _ ->
            PipeWire.quit pwClient
          _ ->
            pure ()
    ]
  SetState state ->
    [Model model {state}]
  ShowAlert {title, body} ->
    [Report (parentAlert title body)]
  CamerasChanged cameras ->
    case model.state of
      StateConnectedToPipewire client _cameras ->
        [Model model {state = StateConnectedToPipewire client cameras}]
      _ -> []

catchErrors :: Text -> (CameraEvent -> IO ()) -> IO () -> IO ()
catchErrors title emit cmd = catch cmd handler
  where
    handler (e :: SomeException) =
      emit (ShowAlert title (pack (show e)))
