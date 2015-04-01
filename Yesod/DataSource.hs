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

--instance Yesod DataSource
--instance RenderMessage DataSource FormMessage where
-- renderMessage _ _ = defaultFormMessage

--instance YesodSubDispatch DataSource (HandlerT DataSource IO) where
instance YesodDataSource m => YesodSubDispatch DataSource (HandlerT m IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesDataSource)

--getDataSourceInputR :: HandlerT DataSource (HandlerT DataSource IO) Html
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

--getSubHomeR :: HandlerT DataSource (HandlerT DataSource IO) Html
getSubHomeR :: YesodDataSource m => HandlerT DataSource (HandlerT m IO) Html
getSubHomeR = do
 toMaster <- getRouteToParent
 lift $ defaultLayout [whamlet|<a href=@{toMaster SubHomeR}> |]
 
--simpleSourceForm :: AForm (HandlerT DataSource IO) DataSourceInput
simpleSourceForm :: YesodDataSource m => AForm (HandlerT m IO) DataSourceInput
simpleSourceForm = DataSourceInput
  <$> areq textField "Name" Nothing
  <*> areq intField "Start" Nothing
  <*> areq intField "End" Nothing

  
--postDataSourceInputR :: HandlerT DataSource (HandlerT DataSource IO) Html
postDataSourceInputR :: YesodDataSource m => HandlerT DataSource (HandlerT m IO) Html
postDataSourceInputR = do
    toMaster <- getRouteToParent
    ((result, widget), enctype) <- lift $ runFormPost $ renderDivs $  simpleSourceForm
    case result of
        FormSuccess datasource -> lift $ defaultLayout [whamlet|<p>#{show datasource}|]
        _ -> lift $ defaultLayout
            [whamlet|<a href=@{toMaster SubHomeR}> |]
