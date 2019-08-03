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
    want ["site-build"]

    [nodeBin </> "*"] &%> \outf -> do
      need ["package.json", "yarn.lock"]
      cmd_ "yarn" ["install"]

    ["out/*"] &%> \_ -> do
      need [webpackExe]
      cssFiles <- getDirectoryFiles "." ["css/*.css"]
      need $ jsConfFiles ++ cssFiles
      cmd_ webpackExe

    venv </> "bin/python" %> \_ -> do
      cmd_ "virtualenv -p python2" venv

    pyDepsInstalled %> \out -> do
      let reqs = "requirements.txt"
      need [venv </> "bin/python", reqs]
      cmd_ (venv </> "bin/pip") "install -r" reqs
      writeFile' out ""

    "images/*/*.svg" %> \out -> do
      need [pyDepsInstalled]
      let script = out <.> ".py"
      need [out <.> ".py"]
      Stdout content <- cmd [".venv/bin/python", script]
      writeFile' out content

    "yarn-build" ~> do
      need ["out/manifest.json"]

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
    "prettier" ~> do
      js <- getDirectoryFiles "." ["*.js"]
      css <- getDirectoryFiles "." ["css/*.js"]
      html <- getDirectoryFiles "." ["templates/*.html"]
      cmd_ (nodeBin </> "prettier") "--write" js css html

    "clean" ~> do
      svgs <- pySvgs
      liftIO $ removeFiles "." $ svgs ++ [pyDepsInstalled]
      liftIO $ removeFiles "out" ["*"]
      liftIO $ removeFiles "_site" ["*"]

  where
  venv = ".venv"

  pySvgs :: Action [FilePath]
  -- These should really be derived from the links in the post.
  pySvgs = do
    pys <- getDirectoryFiles "." ["images//*.svg.py"]
    return $ map dropExtension pys

  nodeBin :: FilePath
  nodeBin = "node_modules/.bin"

  webpackExe :: FilePath
  webpackExe = nodeBin </> "webpack"

  jsConfFiles :: [String]
  jsConfFiles =
    ["postcss.config.js"
    ,"webpack.config.js"]

  pyDepsInstalled :: FilePath
  pyDepsInstalled = venv </> ".installed"
