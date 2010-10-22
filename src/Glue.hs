{-# LANGUAGE OverloadedStrings #-}
module Glue
    ( templateHandler
    , defaultReloadHandler
    , templateServe
    , render
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Prelude hiding (catch)
import           Snap.Types hiding (dir)
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory


templateHandler :: TemplateDirectory Snap
                -> (TemplateState Snap -> Snap ())
                -> Snap ()
templateHandler td f = (f =<< getDirectoryTS td)

render :: TemplateState Snap -> ByteString -> Snap ()
render ts template = do
    bytes <- renderTemplate ts template
    flip (maybe pass) bytes $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS x
