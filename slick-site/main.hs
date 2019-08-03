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

    ["out/*"] &%> \_ -> do
      need ["yarn-install"]
      cssFiles <- getDirectoryFiles "." ["css/*.css"]
      need $ jsConfFiles ++ cssFiles
      cmd_ "yarn" ["run", "build"]

    venv </> "bin/python" %> \_ -> do
      cmd_ "virtualenv -p python2" venv

    "py-install" ~> do
      let reqs = "requirements.txt"
      need [venv </> "bin/python", reqs]
      cmd_ (venv </> "bin/pip") "install -r" reqs

    "images/*/*.svg" %> \out -> do
      need ["py-install"]
      let script = out <.> ".py"
      need [out <.> ".py"]
      Stdout content <- cmd [".venv/bin/python", script]
      writeFile' out content

    "site-rebuild" ~> do
      svgs <- pySvgs
      need svgs
      need ["out/manifest.json"]
      cmd_ ["stack", "exec", "site", "rebuild"]
    "site-build" ~> do
      svgs <- pySvgs
      need svgs
      need ["out/manifest.json"]
      cmd_ ["stack", "exec", "site", "build"]
  where
  pySvgs :: Action [FilePath]
  -- These should really be derived from the links in the post.
  pySvgs = do
    pys <- getDirectoryFiles "." ["images//*.svg.py"]
    return $ map dropExtension pys
  venv = ".venv"

jsConfFiles :: [String]
jsConfFiles =
  ["postcss.config.js"
  ,"webpack.config.js"]