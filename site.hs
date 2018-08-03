--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat,(<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    tags <- buildTags postsP tagIdentifier
    let postCtx' = postCtx tags
        
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/posts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/posts/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/custom.html"   defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html" 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx'
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx'
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx' = indexCtx tags posts
            getResourceBody
                >>= applyAsTemplate indexCtx'
                >>= loadAndApplyTemplate "templates/default.html" indexCtx'
                >>= relativizeUrls

    tagsRules tags $ genTagRules tags

    match "templates/*" $ compile templateCompiler
    
    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx' `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< 
                 loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------
simpleCtx :: Tags -> Context String
simpleCtx tags = 
  tagsField "tags" tags `mappend` 
  defaultContext

postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%Y-%m-%d" `mappend`
    tagsField "tags" tags `mappend`
    defaultContext

indexCtx tags posts =
  listField "posts" (postCtx tags) (return posts) `mappend`
  tagsField "tags" tags `mappend`
  constField "title" "Andre's Blog"                `mappend`
  tagCloudField "tagcloud" 100 200 tags `mappend`
  defaultContext

tagsCtx tags tag posts = 
  listField "posts" (taggedPostCtx tags) (return posts) <>
  bodyField "body" <>
  constField "title" tag <>
  tagCloudField "tagCloud" 65 135 tags <>
  constField "tag" tag
                          
-------------------------------------------------------------------------------- 

myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Andre's Blog"
    , feedDescription = "Functional Programming, experience"
    , feedAuthorName  = "Andrey Prokopenko"
    , feedAuthorEmail = "persiantiger@yandex.ru"
    , feedRoot        = "http://an-pro.com/andre"
    }

--------------------------------------------------------------------------------    

tagIdentifier :: String -> Identifier
tagIdentifier = fromCapture "tags/*.html"

--------------------------------------------------------------------------------

postsP :: Pattern
postsP = fromGlob "posts/*"

--------------------------------------------------------------------------------

genTagRules :: Tags -> String -> Pattern -> Rules ()
genTagRules tags tag pattern = do
  route   $ gsubRoute " " (const "-")
  compile $ do
    posts <- recentFirst =<< loadAllSnapshots pattern "content"
    let tagPageCtx = tagsCtx tags tag posts 
    makeItem ""
      >>= loadAndApplyTemplate "templates/post-list.html" tagPageCtx
      >>= loadAndApplyTemplate "templates/custom.html" tagPageCtx
      >>= loadAndApplyTemplate "templates/default.html" tagPageCtx
      >>= relativizeUrls

                     
taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags <> postCtx tags
