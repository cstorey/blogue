import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable

-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = "build" </> dropDirectory1 path

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    "yarn-install" ~> do
      need ["package.json", "yarn.lock"]
      cmd_  "yarn" ["install"]

    "yarn-build" ~> do
      need ["out/manifest.json"]

    "out/manifest.json" %> \_ -> do
      need ["yarn-install"]
      cssFiles <- getDirectoryFiles "." ["css/*.css"]
      need $ jsConfFiles ++ cssFiles
      cmd_ "yarn" ["run", "build"]

    "site-rebuild" ~> do
      cmd_ ["stack", "exec", "site", "rebuild"]
    "site-build" ~> do
      cmd_ ["stack", "exec", "site", "build"]

jsConfFiles :: [String]
jsConfFiles =
  ["postcss.config.js"
  ,"webpack.config.js"]