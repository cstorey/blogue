import System.Directory

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
    want [distDir]

    [nodeBin </> "*"] &%> \outf -> do
      need ["package.json", "yarn.lock"]
      cmd_ "yarn" ["install"]

    ["out/*"] &%> \_ -> do
      need [webpackExe]
      cssFiles <- getDirectoryFiles "." ["css/*.css"]
      need $ jsConfFiles ++ cssFiles
      cmd_ webpackExe

    venv </> "bin/python" %> \_ -> do
      cmd_ "virtualenv -p python3" venv

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

    [hakyllOut </> "*"] &%> \_ -> do
      runHakyll "build"

    distDir ~> do
      need [hakyllOut </> "index.html"]
      files <- getDirectoryFiles hakyllOut ["**"]
      -- liftIO $ putStrLn $ show ("files", hakyllOut, files)
      need $ (hakyllOut </>) <$> files
      forM_ files $ \file -> do
        liftIO $ createDirectoryIfMissing True $ takeDirectory file
        -- liftIO $ putStrLn $ show ("copyFileChanged", (hakyllOut </> file), (distDir </> file))
        copyFileChanged (hakyllOut </> file) (distDir </> file)

    "yarn-build" ~> do
      need ["out/manifest.json"]

    "site-rebuild" ~> do
      runHakyll "rebuild"
    "site-build" ~> do
      runHakyll "build"
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
      liftIO $ removeFiles "_cache" ["*"]
      liftIO $ removeFiles distDir ["*"]

  where
  venv = ".venv"

  pySvgs :: Action [FilePath]
  -- These should really be derived from the links in the post.
  pySvgs = do
    pys <- getDirectoryFiles "." ["images//*.svg.py"]
    return $ map dropExtension pys

  runHakyll :: String -> Action ()
  runHakyll cmd = do
      svgs <- pySvgs
      hakyllSrc <- getDirectoryFiles "." ["posts/**", "templates/*"]
      need $ svgs
        <> ["out/manifest.json"]
        <> hakyllSrc
      cmd_ ["stack", "exec", "site", cmd]

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

  distDir :: FilePath
  distDir = "dist"
  hakyllOut :: FilePath
  hakyllOut = "_site"
