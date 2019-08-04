{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

import System.Directory

import qualified Data.Text as T
import qualified Data.Map as M
import           Data.Aeson
import Control.Lens (at, (?~))
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import System.Posix.Files (createSymbolicLink)
import Development.Shake
import Development.Shake.FilePath
import Data.Foldable
import Slick
import           Text.Pandoc.Options
import           Text.Pandoc
import GHC.Generics (Generic)
import Data.List

-- convert a source filepath to a build filepath
-- e.g. site/css/style.css -> build/css/style.css
srcToBuild :: FilePath -> FilePath
srcToBuild path = "build" </> dropDirectory1 path


data Post = Post
  { title :: String
  , content :: String
  , date :: String
  , description :: String
  , url :: String
  } deriving (Generic, Eq, Ord, Show)
instance FromJSON Post
instance ToJSON Post

data Index = Index
  { title :: String
  , posts :: [Post]
  } deriving (Generic, Eq, Ord, Show)
instance FromJSON Index
instance ToJSON Index

main :: IO ()
main =
  shakeArgs shakeOptions $ do
    want [distDir]

    [nodeBin </> "*"] &%> \outf -> do
      need ["package.json", "yarn.lock"]
      cmd_ "yarn" ["install"]

    wpOut </> "manifest.json" %> \_ -> do
      need [webpackExe]
      deps <- getDirectoryFiles "." ["css/*.css", "js/*.js"]
      need $ jsConfFiles ++ deps
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
        -- putLoud $ show ("copyFileChanged/out", file, (distDir </> file))
        copyFileChanged file (distDir </> file)

    ["dist/posts/*.html", "dist/*.xml", "dist//*.svg"] |%> \out -> do
        let file = dropDirectory1 out
        liftIO $ createDirectoryIfMissing True distDir
        -- putLoud $ show ("copyFileChanged/_site", (hakyllOut </> file), (distDir </> file))
        copyFileChanged (hakyllOut </> file) (distDir </> file)

    [distDir </> "out/main.css", distDir </> "out/main.js"] |%> \out -> do
        let manifestPath = webpackOut </>  "manifest.json"
        need [manifestPath]
        manifestData <- liftIO $ B.readFile manifestPath
        let manifest = (maybe M.empty id $ decode manifestData) :: M.Map String String
        putLoud $ show manifest
        let stem = dropDirectory1 $ dropDirectory1 out
        src <- maybe
          (fail ("Missing item in manifest.json: " ++ stem)) return $
          M.lookup stem manifest
        need [distDir </> webpackOut </> src]
        putLoud $ show $ src
        putLoud $ show $ ("need",distDir </> webpackOut </> src)
        putLoud $ show $ ("createSymbolicLink", src, out)
        liftIO $ removeFiles "." [out]
        traced "symlink" $ createSymbolicLink src out

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
      writeFile' out . T.unpack $ substitute pageT $ toJSON postData

    distDir </> "tmp" </> "posts" </> "*.html" %> \out -> do
      let srcPath = dropDirectory1 (dropDirectory1 out) -<.> "md"
      postData <- loadPost srcPath
      -- Load a mustache template using using cache if available
      need ["templates/page.html"]
      pageT <- compileTemplate' "templates/page.html"
      writeFile' out . T.unpack $ substitute pageT $ toJSON postData

    distDir </> "index.html" %> \out -> do
      postns <- postNames
      allPosts <- postNames >>= traverse loadPost
      let posts = take 5 $ reverse $ sortOn date allPosts
      let title = "Home"
      let idx = Index {title,posts}

      need ["templates/home.html"]
      pageT <- compileTemplate' "templates/home.html"
      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute pageT $ toJSON idx

    distDir </> "archive.html" %> \out -> do
      postns <- postNames
      allPosts <- postNames >>= traverse loadPost
      let posts = reverse $ sortOn date allPosts
      let title = "Archive"
      let idx = Index {title,posts}

      need ["templates/archive.html"]
      pageT <- compileTemplate' "templates/archive.html"
      -- Fill in the template using the post metadata/content
      writeFile' out . T.unpack $ substitute pageT $ toJSON idx

    distDir ~> do
      posts <- postHtmls
      need $ ["copy-hakyll", "copy-webpack",
            distDir </> "out/main.css",
            distDir </> "out/main.js",
            distDir </> "about.html",
            distDir </> "index.html",
            distDir </> "archive.html"
           ] <> posts

    "copy-hakyll" ~> do
      need [hakyllOut </> "atom.xml"]
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

  postNames :: Action [FilePath]
  postNames =  getDirectoryFiles "." ["posts/*.md"]

  postHtmls :: Action [FilePath]
  postHtmls = do
    names <- postNames
    return $ (\f -> distDir </> "tmp" </> f -<.> ".html") <$> names

  loadPost :: FilePath -> Action Post
  loadPost srcPath = do
    need [srcPath]
    let url = T.pack $ srcPath -<.> "html"
    postData <- readFile' srcPath >>= markdownToHTML . T.pack
    let withURL = _Object . at (T.pack "url") ?~ String url
    convert . withURL $ postData

  renderMarkdown :: T.Text -> Action Post
  renderMarkdown t = do
    p <- loadUsing (readMarkdown readerOptions) (writeHtml5String writerOptions) t
    convert p
    where
      mathExtensions = extensionsFromList [
                        Ext_tex_math_dollars,
                        Ext_tex_math_double_backslash,
                        Ext_latex_macros]
      writerOptions = html5Options {
                        writerExtensions = writerExtensions html5Options <> mathExtensions
                        , writerHTMLMathMethod =  KaTeX  "https://nonexistent.example/"
                      }
      readerOptions = markdownOptions {
        readerExtensions = readerExtensions markdownOptions `mappend` mathExtensions
      }
