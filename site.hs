--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (empty, (<$>))
import           Control.Monad       (filterM)
import           Data.List           (nub, partition, sort)
import           Data.Monoid         (mconcat, (<>))
import           Data.Time           (UTCTime, defaultTimeLocale, toGregorian,
                                      utctDay)
import           Hakyll
import           System.FilePath     (dropExtension, joinPath, splitDirectories,
                                      splitPath, takeBaseName, takeExtension)
import           System.Process      (rawSystem)


--------------------------------------------------------------------------------
eventsP :: Pattern
eventsP = "events/*" .&&. complement "events/archive.html"


--------------------------------------------------------------------------------
pagesP :: Pattern
pagesP = "*.html" .||. "events/archive.html"


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    ----------------------------------------------------------------------------
    match "js/*.js" $ do
        route   idRoute
        compile copyFileCompiler


    ----------------------------------------------------------------------------
    -- Flyers (big)
    match "images/flyers/*" $ do
        route   idRoute
        compile copyFileCompiler


    ----------------------------------------------------------------------------
    -- Flyers (small)
    match "images/flyers/*" $ version "small" $ do
        route   $ customRoute $ \identifier ->
            moveDownIntoDirectory "small" (toFilePath identifier)
        compile $ do
            i         <- toFilePath <$> getUnderlying
            TmpFile o <- newTmpFile $ "out" <> takeExtension i
            _         <- unsafeCompiler $ rawSystem "convert"
                    ["-resize", "300x", "-filter", "Sinc", i, o]
            makeItem $ TmpFile o


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
    match eventsP $ do
        -- No output yet
        route   $ indexRoute
        compile $
            getResourceBody                                        >>=
            return . fmap demoteHeaders                            >>=
            applyAsTemplate eventCtx                               >>=
            loadAndApplyTemplate "templates/event.html"   eventCtx >>=
            loadAndApplyTemplate "templates/default.html" eventCtx >>=
            prettifyIndexRoutes                                    >>=
            relativizeUrls


    ----------------------------------------------------------------------------
    -- Pages
    match pagesP $ do
        route   $ indexRoute
        compile $
            getResourceBody                                       >>=
            applyAsTemplate pageCtx                               >>=
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
eventCtx :: Context String
eventCtx = mconcat
    [ field "year" $
        fmap (show . getYear) . getItemUTC defaultTimeLocale . itemIdentifier
    , functionField "flyer" $ \args item -> do
        let bn = takeBaseName $ toFilePath $ itemIdentifier item
            pt = fromGlob ("images/flyers/" ++ bn ++ ".*")
            vp = case args of
                [v] -> hasVersion v
                _   -> hasNoVersion
        identifiers <- getMatches (pt .&&. vp)
        case identifiers of
            [identifier] -> maybe empty toUrl <$> getRoute identifier
            _            -> empty
    , functionField "flickr" $ \args _ -> case args of
        [album] -> return $
            "<iframe src=\"" ++ album ++ "/player\"                      \
            \   width=\"800\" height=\"600\" frameborder=\"0\"           \
            \   allowfullscreen webkitallowfullscreen mozallowfullscreen \
            \   oallowfullscreen msallowfullscreen></iframe>"
        _ -> fail "invalid args for flickr"
    , activeClassField
    , defaultContext
    ]


--------------------------------------------------------------------------------
activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
    path <- toFilePath <$> getUnderlying
    return $ if dropExtension (head (splitDirectories path)) == p
        then "active"
        else "inactive"


--------------------------------------------------------------------------------
pageCtx :: Context String
pageCtx = mconcat
    [ listField "years" yearCtx $ do
        (_, archived) <- findEvents
        years         <-
            mapM (fmap getYear . getItemUTC defaultTimeLocale) archived
        mapM makeItem $ nub $ reverse $ sort years
    , listField "events" eventCtx $ do
        (current, _) <- findEvents
        chronological =<< mapM load current
    , activeClassField
    , defaultContext
    ]
  where
    yearCtx :: Context Integer
    yearCtx = mconcat
        [ field "year" $ return . show . itemBody
        , listFieldWith "events" eventCtx $ \yearItem -> do
            (_, archived) <- findEvents
            thisYear      <- filterM
                (fmap ((== itemBody yearItem) . getYear) .
                    getItemUTC defaultTimeLocale)
                archived
            chronological =<< mapM load thisYear
        ]


--------------------------------------------------------------------------------
findEvents :: Compiler ([Identifier], [Identifier])  -- current, archived
findEvents = do
    metadatas <- getAllMetadata eventsP
    let (current, archived) =
            partition (\(_, md) -> lookupString "archived" md /= Just "true") $
            metadatas
    return (map fst current, map fst archived)


--------------------------------------------------------------------------------
getYear :: UTCTime -> Integer
getYear utct = let (year, _, _) = toGregorian (utctDay utct) in year


--------------------------------------------------------------------------------
moveDownIntoDirectory
    :: String -> FilePath -> FilePath
moveDownIntoDirectory dir = joinPath . go . splitDirectories
  where
    go []       = []
    go [x]      = dir : [x]
    go (x : xs) = x : go xs
