{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings     #-}
module Yesod.DataSource.Data where

import Yesod
import Data.Text (Text)

-- Subsites have foundations just like master sites.
data DataSource = DataSource

class (RenderMessage master FormMessage, Yesod master) => YesodDataSource master

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "DataSource" [parseRoutes|
/ SubHomeR GET
/datasource DataSourceInputR POST GET
|]

data DataSourceInput a = DataSourceInput
    { dataSourceName        :: Text
    , dataSourceStart      :: a
    , dataSourceEnd         :: a
    }
  deriving Show