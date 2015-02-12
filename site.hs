--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat, mappend)
import Hakyll
import Data.Default (def)
import System.Environment (getArgs)
import Text.Pandoc.Options (writerHtml5, writerSectionDivs)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    (action:_) <- getArgs
    let previewMode = action == "watch"
        config = if previewMode
            then def {
                destinationDirectory = "_preview_site"
              , storeDirectory = "_preview_cache"
              , tmpDirectory = "_preview_cache/tmp"
              }
            else def
        postsPattern = if previewMode
            then "posts/*" .||. "drafts/*"
            else "posts/*"
        pandocCompiler = pandocCompilerWith
            defaultHakyllReaderOptions
            defaultHakyllWriterOptions {
                writerHtml5 = True,
                writerSectionDivs = True
                }

        compilePostListCtx = do
            posts <- recentFirst =<< loadAllSnapshots postsPattern "content"
            return $ listField "posts" postCtx (return posts)

    hakyllWith config $ do

        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match postsPattern $ do
            route $ setExtension "html"
            compile $ do
                posts <- pandocCompiler >>= saveSnapshot "content"
                postListCtx <- compilePostListCtx
                let context = (postCtx `mappend` postListCtx)
                loadAndApplyTemplate "templates/post.html" context posts
                    >>= loadAndApplyTemplate "templates/default.html" context
                    >>= relativizeUrls

        match "index.html" $ do
            route idRoute
            compile $ do
                postListCtx <- compilePostListCtx
                let indexCtx = mconcat
                        [ constField "title" "Home"
                        , constField "home" "True"
                        , postListCtx
                        , defaultContext
                        ]

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , constField "archive" "True"
    , teaserField "teaser" "content"
    , defaultContext
    ]
