--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import qualified Data.Map as M
import           Hakyll
import           Data.Aeson
import           Hakyll.Contrib.Hyphenation (hyphenateHtml, english_GB)
import           System.Process (readProcess)
import qualified Data.ByteString.Lazy as B
import           System.IO.Unsafe (unsafePerformIO)
import qualified Text.Pandoc.SideNote as SideNote

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

updateFromManifest :: M.Map String String -> Item String -> Compiler (Item String)
updateFromManifest manifest item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (withUrls $ mapUrl manifest) item

main :: IO ()
main = do
  manifestData <- B.readFile $ bundlePrefix ++ "manifest.json"
  let manifest = (maybe M.empty id $ decode manifestData) :: M.Map String String
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

    match (fromGlob $ bundlePrefix ++ "*") $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompilerWithSideNotes
            >>= hyphenateHtml english_GB
            >>= loadAndApplyTemplate "templates/page.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" mainContext
            >>= updateFromManifest manifest
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWithSideNotes
            >>= hyphenateHtml english_GB
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= updateFromManifest manifest
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
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= updateFromManifest manifest
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
                >>= loadAndApplyTemplate "templates/home.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= updateFromManifest manifest
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <>
                    constField "description" "This is the post description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
            renderAtom myFeedConfiguration feedCtx posts



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

pandocCompilerWithSideNotes :: Compiler (Item String)
pandocCompilerWithSideNotes = pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions SideNote.usingSideNotes
