{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import System.Process (rawSystem)
import Hakyll

main :: IO ()
main = hakyll $ do
    ----------------------------------------------------------------------------
    -- SVG images
    match "images/*.svg" $ do
        route   $ setExtension "png"
        compile $ do
            filePath       <- toFilePath <$> getUnderlying
            TmpFile target <- newTmpFile "out.png"
            _              <- unsafeCompiler $ rawSystem "inkscape"
                ["-z" , "-e" , target , "-w", "1000" , filePath]
            makeItem $ TmpFile target


    ----------------------------------------------------------------------------
    -- .less files
    match "style/*.less" $ do
        route   $ setExtension "css"
        compile $ getResourceBody >>= withItemBody (unixFilter "lessc" ["-"])
