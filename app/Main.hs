module Main (main) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack, unwords)
import Monomer
import Monomer.Hagrid
import System.Directory (getCurrentDirectory, getHomeDirectory, listDirectory)
import Prelude hiding (unwords)

data AppModel = AppModel
  { environmentInfos :: Seq EnvironmentInfo
  }
  deriving (Eq, Show)

data EnvironmentInfo = EnvironmentInfo
  { key :: Text,
    value :: Text
  }
  deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppInitFinish (Seq EnvironmentInfo)

main :: IO ()
main = do
  startApp initialModel handleEvent buildUI config
  where
    initialModel = AppModel {environmentInfos = mempty}
    config =
      [ appTheme darkTheme,
        appWindowTitle "Monomer Flatpak Example",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appFontDef "Bold" "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appDisableAutoScale True,
        appInitEvent AppInit
      ]

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model =
  vstack
    [ label "This is a demo of the monomer framework running inside the Flatpak sandbox."
        `styleBasic` [padding 10],
      hagrid
        [ (textColumn "Environment Property" (.key)) {initialWidth = 200},
          (textColumn "Value" (.value)) {initialWidth = 600}
        ]
        model.environmentInfos
    ]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  AppInit ->
    [ Task $ do
        curDir <- pack <$> getCurrentDirectory
        homeDir <- pack <$> getHomeDirectory
        lsRoot <- unwords . fmap pack <$> listDirectory "/"
        lsCurDir <- unwords . fmap pack <$> listDirectory (unpack curDir)
        lsHomeDir <- unwords . fmap pack <$> listDirectory (unpack homeDir)
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
    ]
  AppInitFinish environmentInfos ->
    [Model model {environmentInfos}]
