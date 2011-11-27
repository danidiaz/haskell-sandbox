{-# LANGUAGE TemplateHaskell #-}
module TH where
import Data.List
import Language.Haskell.TH
import Data.Generics.Uniplate.Data

iHateDelete :: Q [Dec] -> Q [Dec]
iHateDelete = fmap (transformBi f)
    where
        f :: Exp -> Exp
        f (VarE x) | x == 'delete = VarE 'deleteBy `AppE` VarE '(==)
        f x = x
