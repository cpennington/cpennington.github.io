--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mconcat)
import Hakyll
import Data.Default (def)
import System.Environment (getArgs)


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

    hakyllWith config $ do
        match "images/*" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            route   idRoute
            compile compressCssCompiler

        match (fromList ["about.rst", "contact.markdown"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match postsPattern $ do
            route $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let archiveCtx = mconcat
                        [ listField "posts" postCtx (return posts)
                        , constField "title" "Archives"
                        , constField "archive" "True"
                        , defaultContext
                        ]

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let indexCtx = mconcat
                        [ listField "posts" postCtx (return posts)
                        , constField "title" "Home"
                        , constField "home" "True"
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
    , defaultContext
    ]
