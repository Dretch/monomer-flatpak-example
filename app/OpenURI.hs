{-# LANGUAGE QuasiQuotes #-}

module OpenURI (openURI) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.Text (Text, pack)
import Desktop.Portal (Client, FileSpec (..))
import Desktop.Portal qualified as Portal
import Desktop.Portal.OpenURI (openURIOptions)
import Desktop.Portal.OpenURI qualified as OpenURI
import Monomer
import System.Directory (createDirectoryIfMissing)
import Text.URI (URI, render)
import Text.URI.QQ (uri)

newtype OpenURIModel = OpenURIModel
  { portalClient :: Client
  }
  deriving (Eq, Show)

data OpenURIEvent
  = OpenURI
  | OpenFile
  | OpenDirectory
  | ShowAlert {title :: Text, body :: Text}

weatherURI :: URI
weatherURI = [uri|https://www.bbc.co.uk/weather/ox1|]

openURI :: (CompParentModel s, CompositeEvent e) => Client -> (Text -> Text -> e) -> WidgetNode s e
openURI portalClient parentAlert =
  compositeD_ "MonomerFlatpakExample.OpenURI" (WidgetValue initialModel) buildUI (handleEvent parentAlert) []
  where
    initialModel = OpenURIModel {portalClient}

buildUI :: UIBuilder OpenURIModel OpenURIEvent
buildUI _wenv _model =
  vstack_
    [childSpacing]
    [ label ("Open URI: " <> render weatherURI),
      button "Open URI" OpenURI,
      spacer,
      label "Open new text file with other app.",
      button "Open File" OpenFile,
      spacer,
      label "Open new directory with file browser.",
      button "Open Directory" OpenDirectory
    ]

handleEvent :: (Text -> Text -> ep) -> EventHandler OpenURIModel OpenURIEvent sp ep
handleEvent parentAlert _env _node model = \case
  OpenURI ->
    [ Producer $ \emit ->
        catchErrors "Open URI Failed" emit $ do
          void (Portal.openURI model.portalClient (openURIOptions weatherURI))
    ]
  OpenFile ->
    [ Producer $ \emit -> do
        catchErrors "Open File Failed" emit $ do
          filePath <- (<> "/hello.txt") <$> Portal.getXdgDataHome
          writeFile filePath "Hello!"
          void $ OpenURI.openFile model.portalClient (OpenURI.openFileOptions (FileSpecPath filePath))
    ]
  OpenDirectory ->
    [ Producer $ \emit -> do
        catchErrors "Open Directory Failed" emit $ do
          dirPath <- (<> "/hello-directory") <$> Portal.getXdgDataHome
          let filePath = dirPath <> "/hello.txt"
          createDirectoryIfMissing True dirPath
          writeFile filePath "Hello!"
          void $ OpenURI.openDirectory model.portalClient (OpenURI.openDirectoryOptions (FileSpecPath dirPath))
    ]
  ShowAlert {title, body} ->
    [Report (parentAlert title body)]

catchErrors :: Text -> (OpenURIEvent -> IO ()) -> IO () -> IO ()
catchErrors title emit cmd = catch cmd handler
  where
    handler (e :: SomeException) =
      emit (ShowAlert title (pack (show e)))
