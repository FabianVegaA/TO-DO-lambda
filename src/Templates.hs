{-# LANGUAGE OverloadedStrings #-}

module Templates (makeTask, makeTaskList) where

import Data.Text (Text)
import Text.Blaze.Html5 (Html, button, div, h5, iframe, toHtml, (!))
import Text.Blaze.Html5.Attributes (id, src)
import Prelude hiding (div, id)

makeTask :: Text -> Html
makeTask task = div $ do
  div ! id "task" $ mempty
  div ! id "task" $ do
    h5 ! id "title-task" $ toHtml task
  button ! id "remove-task" $ do
    iframe ! src "icons/trash.svg" ! id "trash" $ mempty

makeTaskList :: [Text] -> Html
makeTaskList tasks =
  div $
    do
      mapM_ makeTask tasks
