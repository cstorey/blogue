--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Hakyll
import           Hakyll.Core.Compiler
import           Data.Aeson
import           System.Process (readProcess)
import qualified Data.ByteString.Lazy as B
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Set as S
import           Text.Pandoc.Options
import           Text.Pandoc
import           Text.Pandoc.Walk (walkM)
import qualified Data.Text as T
import           System.Directory
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import           System.AtomicWrite.Writer.String

--------------------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Ceri Storey"
    , feedDescription = "An Blog"
    , feedAuthorName  = "Ceri Storey"
    , feedAuthorEmail = "atom@ceri.storey.name"
    , feedRoot        = "http://ceri.storey.name"
    }

bundlePrefix = "out/"

mapUrl :: M.Map String String -> String -> String
mapUrl m k = res
  where
   addPrefix k = "/" ++ bundlePrefix ++ k
   valp = M.lookup k m
   res = maybe k addPrefix $ valp

updateFromManifest :: Item String -> Compiler (Item String)
updateFromManifest item = do
    manifestData <- unsafeCompiler $ B.readFile $ bundlePrefix ++ "manifest.json"
    let manifest = (maybe M.empty id $ decode manifestData) :: M.Map String String
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls $ mapUrl manifest) item

main :: IO ()
main = do
  thePandocCompiler <- makePandocCompiler
  hakyll $ do
    match "images/**/*.dot" $ do
        route $ setExtension "svg"
        compile $ getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    manifestDep <- makePatternDependency "out/manifest.json"
    rulesExtraDependencies [manifestDep] $ do
      match (fromList ["about.md"]) $ do
          route   $ setExtension "html"
          compile $ thePandocCompiler
              >>= loadAndApplyTemplate "templates/page.hakyll.html"    postCtx
              >>= loadAndApplyTemplate "templates/default.hakyll.html" mainContext
              >>= updateFromManifest
              >>= relativizeUrls

      match "posts/*" $ do
          route $ setExtension "html"
          compile $ thePandocCompiler
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/post.hakyll.html"    postCtx
              >>= loadAndApplyTemplate "templates/default.hakyll.html" postCtx
              >>= updateFromManifest
              >>= relativizeUrls

      create ["archive.html"] $ do
          route idRoute
          compile $ do
              posts <- recentFirst =<< loadAll "posts/*"
              let archiveCtx =
                      listField "posts" postCtx (return posts) <>
                      constField "title" "Archives"            <>
                      mainContext

              makeItem ""
                  >>= loadAndApplyTemplate "templates/archive.hakyll.html" archiveCtx
                  >>= loadAndApplyTemplate "templates/page.hakyll.html"    postCtx
                  >>= loadAndApplyTemplate "templates/default.hakyll.html" archiveCtx
                  >>= updateFromManifest
                  >>= relativizeUrls

      create ["index.html"] $ do
          route idRoute
          compile $ do
              posts <- fmap (take 5) $ recentFirst =<< loadAll "posts/*"
              let archiveCtx =
                      listField "posts" indexCtx (return posts) <>
                      constField "title" "Home"            <>
                      mainContext

              makeItem ""
                  >>= loadAndApplyTemplate "templates/home.hakyll.html" archiveCtx
                  >>= loadAndApplyTemplate "templates/default.hakyll.html" archiveCtx
                  >>= updateFromManifest
                  >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <>
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom myFeedConfiguration feedCtx posts

-- https://stackoverflow.com/questions/29868096/how-to-use-pandoc-filter-within-hakyll
transformer
  :: ReaderOptions
  -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
transformer reader_opts writer_opts pandoc = walkM katexify pandoc
  where
    katexify :: Inline -> Compiler Inline
    katexify chunk@(Math mode body) = do
      cachedp <- cacheGet chunk
      markup <- case cachedp of
                  Just markup -> return markup
                  Nothing -> do
                    markup <- unixFilter "./node_modules/.bin/katex" (opts mode) $ body
                    cachePut chunk markup
                    return markup
      return $ RawInline (Format "html") markup
    katexify other = return other
    opts DisplayMath = ["-d"]
    opts InlineMath = []

    cacheGet :: Inline -> Compiler (Maybe String)
    cacheGet chunk = unsafeCompiler $ do
      let fn = pathFor chunk
      existsp <- doesFileExist fn
      if existsp
        then Just <$> readFile fn
        else return Nothing

    cachePut :: Inline -> String -> Compiler ()
    cachePut chunk markup = unsafeCompiler $ atomicWriteFile (pathFor chunk) markup
    pathFor chunk = "/tmp/.katex-cache-" ++ digest
      where
      digest = C.unpack $ B16.encode $ SHA256.hash $ C.pack $ show chunk

makePandocCompiler = do
    return $ pandocCompilerWithTransformM readerOptions writerOptions katexFilter
    where
        mathExtensions = extensionsFromList [
                          Ext_tex_math_dollars,
                          Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = defaultExtensions `mappend` mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions
                          , writerHTMLMathMethod =  KaTeX  "https://nonexistent.example/"
                        }
        readerOptions = defaultHakyllReaderOptions { readerExtensions = newExtensions }
        katexFilter = transformer readerOptions writerOptions

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    mainContext

indexCtx :: Context String
indexCtx =
    teaserField "teaser" "content" <>
    postCtx

mainContext :: Context String
mainContext = defaultContext
