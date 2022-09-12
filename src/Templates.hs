{-# LANGUAGE OverloadedStrings #-}

module Templates (makeTask, makeTaskList) where

import Data.Text (Text)
import Text.Blaze.Html5 (Html, div, h5, toHtml, (!))
import Text.Blaze.Html5.Attributes (id)
import Prelude hiding (div, id)

makeTask :: Text -> Html
makeTask task = div $ do
    div ! id "task" $ mempty
    div ! id "task" $ h5 $ toHtml task

makeTaskList :: [Text] -> Html
makeTaskList tasks =
  div $
    do
      mapM_ makeTask tasks
