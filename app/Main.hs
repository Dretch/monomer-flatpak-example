module Main (main) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import Monomer
import Monomer.Hagrid
import Paths_monomer_flatpak_example (getDataFileName)
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import System.Environment (getEnvironment)
import Prelude hiding (unwords)

data AppModel = AppModel
  { fileSystem :: Seq EnvironmentInfo,
    environmentVariables :: Seq EnvironmentInfo
  }
  deriving (Eq, Show)

data EnvironmentInfo = EnvironmentInfo
  { key :: Text,
    value :: Text
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInitFinish (Seq EnvironmentInfo) (Seq EnvironmentInfo)

main :: IO ()
main = do
  regularFontPath <- pack <$> getDataFileName "/fonts/Cantarell/Cantarell-Regular.ttf"
  iconPath <- pack <$> getDataFileName "/io.github.Dretch.MonomerFlatpakExample.png"
  startApp initialModel handleEvent buildUI (config regularFontPath iconPath)
  where
    initialModel = AppModel {fileSystem = mempty, environmentVariables = mempty}
    config regularFontPath iconPath =
      [ appTheme darkTheme,
        appWindowTitle "Monomer Flatpak Example",
        appWindowIcon iconPath,
        appWindowState (MainWindowNormal (1000, 800)),
        appFontDef "Regular" regularFontPath,
        appDisableAutoScale True,
        appInitEvent AppInit
      ]

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model =
  vstack_
    [childSpacing_ 5]
    [ label "Monomer Flatpak Example"
        `styleBasic` [textSize 40, paddingL 5, paddingT 10, paddingR 10],
      label "This is a demo of the monomer framework running inside the Flatpak sandbox."
        `styleBasic` [padding 5],
      hagrid
        [ (textColumn "File System Info" (.key)) {initialWidth = 300},
          (widgetColumn "" valueColumn) {initialWidth = 700}
        ]
        model.fileSystem,
      hagrid
        [ (textColumn "Environment Variable" (.key)) {initialWidth = 300},
          (widgetColumn "" valueColumn) {initialWidth = 700}
        ]
        model.environmentVariables
    ]

valueColumn :: Int -> EnvironmentInfo -> WidgetNode s e
valueColumn _ix model =
  label_ model.value [multiline, ellipsis]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  AppInit ->
    [ Task $ do
        curDir <- pack <$> getCurrentDirectory
        homeDir <- pack <$> getHomeDirectory
        lsRoot <- pack . unlines <$> listDirectory "/"
        lsCurDir <- pack . unlines <$> listDirectory (unpack curDir)
        lsHomeDir <- pack . unlines <$> listDirectory (unpack homeDir)
        envVars <- Seq.fromList . map (\(k, v) -> EnvironmentInfo (pack k) (pack v)) <$> getEnvironment
        pure $
          AppInitFinish
            ( Seq.fromList
                [ EnvironmentInfo "Current Directory" curDir,
                  EnvironmentInfo "Home Directory" homeDir,
                  EnvironmentInfo "ls /" lsRoot,
                  EnvironmentInfo ("ls " <> curDir) lsCurDir,
                  EnvironmentInfo ("ls " <> homeDir) lsHomeDir
                ]
            )
            envVars
    ]
  AppInitFinish fileSystem environmentVariables ->
    [Model model {fileSystem, environmentVariables}]
