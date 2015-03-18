{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module TH

where

import Data.Char
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Options.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


-- | Derives an argument parser for this type constructor using its Read instance.
--   Named `pArgInt`, `pArgMyArg` etc.
deriveArgument :: Name -> Q [Dec]
deriveArgument name = do
  TyConI tycon <- reify name
  let  tyname  = typeName tycon
       tyvars  = typeVars tycon
       mv      = litE $ stringL $ map toUpper (nameBase tyname)
       pName   = prependName "pArg" tyname
       pExp    = [| argument $lookupAuto (metavar $mv) |]

  pSig <- sigD pName $ forallT tyvars (cxt [])
                     $ appT lookupTypeParser (appTVars tyname tyvars)
  pFun <- funD pName [clause [] (normalB pExp) []]
  return [pSig, pFun]

deriveCommands:: Name -> Q [Dec]
deriveCommands name = do
  TyConI (DataD _ tyname _ dacons _) <- reify name

  let pName     = prependName "p" tyname
      pCommands = infixWith lookupMappend (map commandParserE dacons)
      pExp      = [| subparser $pCommands |]

  pSig  <- sigD pName (appT lookupTypeParser (conT tyname))
  pFun  <- funD pName [clause [] (normalB pExp) []]
  pSubs <- mapM commandParserD dacons

  return (pSig:pFun:pSubs)

appArguments :: Name -> [StrictType] -> Q Exp
appArguments n []     = conE n
appArguments n (t:ts) = infixE (Just $ conE n) lookupFmap (Just $ foldr _ _ ts)
  where appTy (_, ty) app = undefined

appTVars :: Name -> [TyVarBndr] -> Q Type
appTVars name = foldr appTV (conT name)
  where appTV (PlainTV tyvar) app = appT app (varT tyvar)

typeName :: Dec -> Name
typeName (DataD    _ ty _ _ _) = ty
typeName (NewtypeD _ ty _ _ _) = ty

typeVars :: Dec -> [TyVarBndr]
typeVars (DataD    _ _ vars _ _) = vars
typeVars (NewtypeD _ _ vars _ _) = vars

prependName :: String -> Name -> Name
prependName pre name = mkName (pre <> nameBase name)

lookupTypeParser :: Q Type
lookupTypeParser = ConT . fromJust <$> lookupTypeName "Parser"

lookupMappend :: Q Exp
lookupMappend = VarE . fromJust <$> lookupValueName "<>"

lookupFmap :: Q Exp
lookupFmap = VarE . fromJust <$> lookupValueName "<$>"

lookupAuto :: Q Exp
lookupAuto = VarE . fromJust <$> lookupValueName "auto"

infixWith :: Q Exp -> [Q Exp] -> Q Exp
infixWith _ []     = error "no expression to infix"
infixWith _ [x]    = x
infixWith m (x:xs) = infixE (Just x) m (Just (infixWith m xs))

commandParserE :: Con -> Q Exp
commandParserE (NormalC commandName _) = do
  let commandStr = litE (stringL (nameBase commandName))
      pName      = prependName "pCommand" commandName
  [| command $commandStr $(varE pName) |]

commandParserD :: Con -> Q Dec
commandParserD (NormalC commandName vars) = do
  let pName = prependName "pCommand" commandName
  pFun <- funD pName [clause [] (normalB $ appArguments commandName vars) []]
  return pFun
