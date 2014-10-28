--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mconcat)
import           Hakyll
import           System.FilePath     (dropExtension, joinPath, splitPath)
import           System.Process      (rawSystem)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    ----------------------------------------------------------------------------
    match "js/*.js" $ do
        route   idRoute
        compile copyFileCompiler


    ----------------------------------------------------------------------------
    -- Regular images
    match "images/**.png" $ do
        route   idRoute
        compile copyFileCompiler


    ----------------------------------------------------------------------------
    -- SVG images
    match "images/*.svg" $ do
        route   $ setExtension "png"
        compile $ do
            i         <- toFilePath <$> getUnderlying
            TmpFile o <- newTmpFile "out.png"
            _         <- unsafeCompiler $
                            rawSystem "inkscape" ["-z", "-e", o, i]
            makeItem $ TmpFile o


    ----------------------------------------------------------------------------
    -- .less files
    allLessFiles <- makePatternDependency "style/*.less"
    match "style/main.less" $ rulesExtraDependencies [allLessFiles] $ do
        route   $ setExtension "css"
        compile $ do
            i         <- toFilePath <$> getUnderlying
            TmpFile o <- newTmpFile "out.css"
            _         <- unsafeCompiler $ rawSystem "lessc" [i, o]
            makeItem $ TmpFile o
    match "style/*.less" $ compile $ makeItem ()


    ----------------------------------------------------------------------------
    -- Page per event
    match "events/*.html" $ do
        -- No output yet
        compile getResourceBody


    ----------------------------------------------------------------------------
    -- Events page
    match "events.html" $ do
        route   $ indexRoute
        compile $
            getResourceBody                                         >>=
            applyAsTemplate eventsCtx                               >>=
            loadAndApplyTemplate "templates/default.html" eventsCtx >>=
            prettifyIndexRoutes                                     >>=
            relativizeUrls


    ----------------------------------------------------------------------------
    -- Pages
    match "*.html" $ do
        route   $ indexRoute
        compile $
            getResourceBody                                       >>=
            loadAndApplyTemplate "templates/default.html" pageCtx >>=
            prettifyIndexRoutes                                   >>=
            relativizeUrls


    ----------------------------------------------------------------------------
    -- Templates
    match "templates/*.html" $ compile templateCompiler


--------------------------------------------------------------------------------
indexRoute :: Routes
indexRoute = customRoute $ \identifier ->
    let path  = splitPath (toFilePath identifier)
        names = case dropExtension (last path) of
            "index" -> ["index.html"]
            n       -> [n, "index.html"]
    in joinPath (init path ++ names)


--------------------------------------------------------------------------------
prettifyIndexRoutes :: Item String -> Compiler (Item String)
prettifyIndexRoutes = return . fmap (withUrls prettify)
  where
    prettify []               = []
    prettify url@(x : xs)
        | "index.html" == url = []
        | otherwise           = x : prettify xs


--------------------------------------------------------------------------------
eventsCtx :: Context String
eventsCtx = mconcat
    [ listField "events" eventCtx $ do
        loadAll "events/*.html"
    , pageCtx
    ]


--------------------------------------------------------------------------------
eventCtx :: Context String
eventCtx = defaultContext


--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
    [ functionField "activeClass" $ \[p] _ -> do
        underlying <- getUnderlying
        return $ if fromFilePath p == underlying then "active" else "inactive"
    , defaultContext
    ]
