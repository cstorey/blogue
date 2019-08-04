import System.Directory

import qualified Data.Text as T
import qualified Data.Map as M
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Posix.Files (createSymbolicLink)
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable
import Slick

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

    wpOut </> "manifest.json" %> \_ -> do
      need [webpackExe]
      cssFiles <- getDirectoryFiles "." ["css/*.css"]
      need $ jsConfFiles ++ cssFiles
      cmd_ webpackExe
    wpOut </> "*" %> \outf -> do
      need ["out/manifest.json"]

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

    distDir </> wpOut </> "*" %> \out -> do
        let file = dropDirectory1 out
        liftIO $ createDirectoryIfMissing True distDir
        -- liftIO $ putStrLn $ show ("copyFileChanged/out", file, (distDir </> file))
        copyFileChanged file (distDir </> file)

    ["dist//*.html", "dist/*.xml", "dist//*.svg"] |%> \out -> do
        let file = dropDirectory1 out
        liftIO $ createDirectoryIfMissing True distDir
        -- liftIO $ putStrLn $ show ("copyFileChanged/_site", (hakyllOut </> file), (distDir </> file))
        copyFileChanged (hakyllOut </> file) (distDir </> file)

    distDir </> "out/main.css" %> \out -> do
        let manifestPath = webpackOut </>  "manifest.json"
        need [manifestPath]
        manifestData <- liftIO $ B.readFile manifestPath
        let manifest = (maybe M.empty id $ decode manifestData) :: M.Map String String
        liftIO $ putStrLn $ show manifest
        let stem = dropDirectory1 $ dropDirectory1 out
        src <- maybe
          (fail ("Missing item in manifest.json: " ++ stem)) return $
          M.lookup stem manifest
        need [distDir </> webpackOut </> src]
        liftIO $ putStrLn $ show $ src
        liftIO $ putStrLn $ show $ ("need",distDir </> webpackOut </> src)
        liftIO $ putStrLn $ show $ ("createSymbolicLink", src, out)
        liftIO $ removeFiles "." [out]
        liftIO $ createSymbolicLink src out

    distDir </> "about.html" %> \out -> do
--      error $ "about:" ++ out
      let srcPath = (dropDirectory1 out) -<.> "md"
      need [srcPath]
      fileContents <- readFile' srcPath
      -- Load a markdown source file into an Aeson Value
      -- The 'content' key contains an html-rendered string
      -- Any metadata from a yaml block is loaded into the appropriate keys in the Aeson object
      -- e.g. author, date, tags, etc.
      postData <- markdownToHTML . T.pack $ fileContents
      -- Load a mustache template using using cache if available
      need ["templates/page.html"]
      pageT <- compileTemplate' "templates/page.html"
      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute pageT postData

    distDir ~> do
      need ["copy-hakyll", "copy-webpack",
            distDir </> "out/main.css",
            distDir </> "about.html"]

    "copy-hakyll" ~> do
      need [hakyllOut </> "index.html"]
      files <- getDirectoryFiles hakyllOut ["//*.html", "/*.xml", "//*.svg"]
      need $ (hakyllOut </>) <$> files
      need $ (distDir </>) <$> files

    "copy-webpack" ~> do
      need ["out/manifest.json"]
      files <- getDirectoryFiles "." $
          (wpOut </>) <$> ["*.css", "*.woff2", "*.woff", "*.otf", "*.ttf"]
      need $ files
      need $ (distDir </>) <$> files


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
  webpackOut :: FilePath
  webpackOut = "out"

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
  wpOut :: FilePath
  wpOut = "out"
