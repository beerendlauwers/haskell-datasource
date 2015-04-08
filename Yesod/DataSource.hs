{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RankNTypes            #-}

module Yesod.DataSource where

import Yesod.DataSource.Generate
import Yesod.DataSource.Data
import Yesod
import ClassyPrelude

instance YesodDataSource m => YesodSubDispatch DataSource (HandlerT m IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesDataSource)

getDataSourceInputR :: YesodDataSource m => HandlerT DataSource (HandlerT m IO) Html
getDataSourceInputR = do 
  toMaster <- getRouteToParent
  (widget, enctype) <- lift $ generateFormPost $ renderDivs $ simpleSourceForm
  lift $ defaultLayout
            [whamlet|
                <p>
                    The widget generated contains only the contents
                    of the form, not the form tag itself. So...
                <form method=post action=@{toMaster DataSourceInputR} enctype=#{enctype}>
                    ^{widget}
                    <p>It also doesn't include the submit button.
                    <button>Submit
            |]

getSubHomeR :: YesodDataSource m => HandlerT DataSource (HandlerT m IO) Html
getSubHomeR = do
 toMaster <- getRouteToParent
 lift $ defaultLayout [whamlet|<a href=@{toMaster SubHomeR}> |]

simpleSourceForm :: YesodDataSource m => AForm (HandlerT m IO) (DataSourceInput Int)
simpleSourceForm = DataSourceInput
  <$> areq textField "Name" Nothing
  <*> areq intField "Start" Nothing
  <*> areq intField "End" Nothing

postDataSourceInputR :: YesodDataSource m => HandlerT DataSource (HandlerT m IO) Html
postDataSourceInputR = do
    toMaster <- getRouteToParent
    ((result, widget), enctype) <- lift $ runFormPost $ renderDivs $  simpleSourceForm
    case result of
        FormSuccess datasource -> lift $ defaultLayout [whamlet|<p>#{show datasource}|]
        _ -> lift $ defaultLayout
            [whamlet|<a href=@{toMaster SubHomeR}> |]
