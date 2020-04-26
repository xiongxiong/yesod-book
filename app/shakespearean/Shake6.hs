{-# LANGUAGE OverloadedStrings, TemplateHaskell, CPP #-}

module Shake6 where

import Text.Lucius (CssUrl, luciusFile, luciusFileReload, renderCss)
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TLIO

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

template :: CssUrl MyRoute
#if PRODUCTION
template = $(luciusFile "template.lucius")
#else
template = $(luciusFileReload "template.lucius")
#endif

main :: IO ()
main = TLIO.putStrLn $ renderCss $ template render