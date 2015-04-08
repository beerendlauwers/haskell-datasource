{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.DataSource.Generate where

import Language.Haskell.Generate.AutoGen
import Language.Haskell.Generate.AutoGen.Extra
import Language.Haskell.Exts.Syntax (InstDecl(InsDecl), Decl(FunBind),Exp(EnumFromTo),Pat(PApp,PWildCard))
import System.Directory (createDirectoryIfMissing)
import Language.Haskell.Generate.Monad (ModuleM)
import Yesod.DataSource.Data
import Data.Text (Text)
import ClassyPrelude

type MachineName = Text

-- The type class of which instances are automatically generated.
class DataSourceView machinename t | machinename -> t where
    generate :: machinename -> IO () ->  [t]
    
newtype ValidDataSourceInput a = ValidDataSourceInput (MachineName,Text,a,a)

-- Data sources just puke out ranges, and you need stuff that's enumerable.
class Enum a => DataSourceGenerator a where
 generateDataSourceTypeClass :: MachineName -> Text -> a -> a -> Decl
 convertDataSourceInput :: DataSourceInput a -> ValidDataSourceInput a
 
-- Here's an example for an Integer data source.
-- TODO: type2 is actually just the a. Might be nice to find out how we can get rid of that duplication.
instance DataSourceGenerator Int where
 generateDataSourceTypeClass machinename type2 range1 range2 = mkClassInstance "DataSourceView" (map unpack [machinename,type2]) [(mkClassInstanceDecl range1 range2)]
  where 
   mkClassInstanceDecl range1 range2 = InsDecl $ FunBind [mkMatch "generate" [PApp (mkQName $ unpack machinename) [],PWildCard] (EnumFromTo (mkIntLit range1) (mkIntLit range2))]
   
 convertDataSourceInput DataSourceInput {..} = ValidDataSourceInput (dataSourceName, "Int",dataSourceStart,dataSourceEnd)
 
saveToFile input = let (ValidDataSourceInput (nm,ty,start,end)) = convertDataSourceInput input
                    in saveDataSourceTypeClassToModule nm ty start end
  
-- Save a data source to a file. This does the heavy lifting.
saveDataSourceTypeClassToModule :: (DataSourceGenerator a) => MachineName -> Text -> a -> a -> IO ()
saveDataSourceTypeClassToModule machinename type2 range1 range2 = do 
 createDirectoryIfMissing True "Generated"
 saveModuleToFile imports pragmas (m,unpack modulename) ("Generated/" ++ (unpack machinename) ++ ".hs")
 where modulename = "Yesod.DataSource.Generated." ++ machinename
       imports = ["Yesod.DataSource"]
       pragmas = [mkPragma [MultiParamTypeClasses]]
       m = do
             addDecl (mkDataDecl (unpack machinename) [] [(unpack machinename)])
             addDecl (generateDataSourceTypeClass machinename type2 range1 range2)
             mapM addImport imports
             return $ Nothing 
       