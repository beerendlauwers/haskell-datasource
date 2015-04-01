{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Yesod.DataSource.Generate where

import Language.Haskell.Generate.AutoGen
import Language.Haskell.Generate.AutoGen.Extra
import Language.Haskell.Exts.Syntax (InstDecl(InsDecl), Decl(FunBind),Exp(EnumFromTo),Pat(PApp,PWildCard))
import System.Directory (createDirectoryIfMissing)

-- The type class of which instances are automatically generated.
class DataSourceView machinename t | machinename -> t where
    generate :: machinename -> IO () ->  [t]

-- Data sources just puke out ranges, and you need stuff that's enumerable.
class Enum a => DataSourceGenerator a where
 generateDataSourceTypeClass :: String -> String -> a -> a -> Decl
 
-- Here's an example for an Integer data source.
-- TODO: type2 is actually just the a. Might be nice to find out how we can get rid of that duplication.
instance DataSourceGenerator Integer where
 generateDataSourceTypeClass machinename type2 range1 range2 = mkClassInstance "DataSourceView" [machinename,type2] [(mkClassInstanceDecl range1 range2)]
  where 
   mkClassInstanceDecl range1 range2 = InsDecl $ FunBind [mkMatch "generate" [PApp (mkQName machinename) [],PWildCard] (EnumFromTo (mkIntLit range1) (mkIntLit range2))]
  
-- Save a data source to a file. This does the heavy lifting.
saveDataSourceTypeClassToModule machinename type2 range1 range2 = do 
 createDirectoryIfMissing True "Generated"
 saveModuleToFile imports pragmas (m,modulename) ("Generated/" ++ machinename ++ ".hs")
 where modulename = "Yesod.DataSource.Generated." ++ machinename
       imports = ["Yesod.DataSource"]
       pragmas = [mkPragma [MultiParamTypeClasses]]
       m = do
             addDecl (mkDataDecl machinename [] [machinename])
             addDecl (generateDataSourceTypeClass machinename type2 range1 range2)
             mapM addImport imports
             return $ Nothing 
       