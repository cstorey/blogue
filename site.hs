--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import		 Hakyll.Contrib.Hyphenation (hyphenateHtml, english_GB)
import		 Hakyll.Web.Sass (sassCompiler)
import		 System.Process (readProcess)


--------------------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Ceri Storey"
    , feedDescription = "An Blog"
    , feedAuthorName  = "Ceri Storey"
    , feedAuthorEmail = "atom@ceri.storey.name"
    , feedRoot        = "http://ceri.storey.name"
    }


main :: IO ()
main = hakyll $ do

    match "images/**/*.dot" $ do
	route $ setExtension "svg"
	compile $ getResourceString >>= withItemBody (unixFilter "dot" ["-Tsvg"])

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.scss" $ do
	route $ setExtension "css"
	let compressCssItem = fmap compressCss
	compile (compressCssItem <$> sassCompiler)


    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= hyphenateHtml english_GB
            >>= loadAndApplyTemplate "templates/default.html" mainContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= hyphenateHtml english_GB
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    mainContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
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

mainContext :: Context String
mainContext = 
    (field "gitversion" $ \_ -> gitVersion) <>
    defaultContext

gitVersion :: Compiler String
gitVersion = unsafeCompiler $ do
  fmap (unwords . words) $ readProcess "git" ["describe"] ""
