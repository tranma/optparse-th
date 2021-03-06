{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Options.Applicative.TH
  ( genOpts
  , genOptsRead
  ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Data.Char
import qualified Data.List           as L
import           Data.Maybe
import           GHC.TypeLits
import           Language.Haskell.TH
import           Options.Applicative

import           Debug.Trace


genOpts :: Name -> Q [Dec]
genOpts = flip genOptsRead []

genOptsRead :: Name -> [(Name, Name)] -> Q [Dec]
genOptsRead name readers = do
  d            <- reifyDec name
  tree         <- mkOpt d
  decls        <- gen (mkReaders readers) "" tree
  (sig, toplv) <- topLevelD name
  return (sig:toplv:decls)

topLevelD :: Name -> Q (Dec, Dec)
topLevelD name = do
  let nameE = varE (concatNameS ["p", nameBase name])
      pName = concatNameS ["optparse", nameBase name]
      pExp  = [| info $nameE (fullDesc <> header "header goes here") |]
  s <- sigD pName (appT lookupParser (conT name))
  d <- funD pName [clause [] (normalB pExp) []]
  return (s, d)

reifyDec :: Name -> Q Dec
reifyDec n = do
  x <- reify n
  case x of
    TyConI d -> return d
    _        -> error "must give a decl"

--------------------------------------------------------------------------------

-- * Parsing Dec

-- only support these patterns
data Opt n where
  L  :: (KnownNat n, n <= 2) => Opt n -> Opt (n + 1)      -- dummy lift constructor, no good simple hlist
  P0 ::                         Name  -> Opt 0            -- no parser
  A0 ::                         Name  -> Opt 0            -- single args
  A1 ::                         Name  -> [Opt 0] -> Opt 1 -- args, no commands
  C0 :: (KnownNat n, n <= 1) => Name  -> [Opt n] -> Opt 2 -- commands, no inner commands
  A2 :: (KnownNat n, n <= 2) => Name  -> [Opt n] -> Opt 3 -- args, commands allowed

-- | User-supplied custom `ReadM` readers.
--
type ReadMF = [(String, Name)]

-- parser (partial)
mkOpt :: Dec -> Q (Opt 3)
mkOpt d = case d of
  NewtypeD _ _ _ _   _ -> L . L . L <$> decA0 d
  DataD    _ _ _ []  _ -> L . L . L <$> decA0 d
  DataD    _ n _ [c] _ -> decA2 n c
  DataD    _ n _ cs  _
    -> if   areCommands cs
       then L         <$> decC0 n cs
       else L . L . L <$> decA0 d

decA0 :: Dec -> Q (Opt 0)
decA0 d = return (A0 (decName d))

decC0 :: Name -> [Con] -> Q (Opt 2)
decC0 n cs = C0 n <$> mapM con1 cs

decA2 :: Name -> Con -> Q (Opt 3)
decA2 n c  = A2 n <$> mapM ty2 (conTypes c)

con1 :: Con -> Q (Opt 1)
con1 c = case c of
  NormalC n [] -> return . L . P0 $ n
  RecC    n [] -> return . L . P0 $ n
  NormalC n ts -> A1 n <$> mapM (ty0 . snd) ts
  _            -> bail "con1: can't deal with" c

ty0 :: Type -> Q (Opt 0)
ty0 t = do
  inf <- reifyTy t
  case inf of
    PrimTyConI n _ _ -> prim0 n
    TyConI     d     -> decA0 d
    _ -> bail "ty0: can't deal with" t

ty2 :: Type -> Q (Opt 2)
ty2 t = do
  inf <- reifyTy t
  case inf of
    PrimTyConI n _ _               -> L . L <$> prim0 n
    TyConI d@(NewtypeD _ _ _ _  _) -> L . L <$> decA0 d
    TyConI d@(DataD    _ _ _ [] _) -> L . L <$> decA0 d
    TyConI d@(DataD    _ n _ cs _)
      -> if   areCommands cs
         then decC0 n cs
         else L . L <$> decA0 d
    _ -> bail "ty: can't deal with" t

prim0 :: Name -> Q (Opt 0)
prim0 n = return (A0 n)

--------------------------------------------------------------------------------

-- * Traversal

type Prefix = String

gen :: KnownNat n => ReadMF -> Prefix -> Opt n -> Q [Dec]
gen readers ps (L x) = gen readers ps x

gen readers ps (P0 name) = pure <$> case lookupCustom name readers of
  Just custom -> readArgD ps custom name
  Nothing     -> pureD ps name

gen readers ps (A0 name) = pure <$> case lookupCustom name readers of
  Just custom -> readArgD ps custom name
  Nothing     -> autoArgD ps name

gen readers ps (A1 name fields) = genA readers (ps <+> name) name fields
gen readers ps (A2 name fields) = genA readers (ps <+> name) name fields

gen readers ps (C0 name cmds) = do
  subDecls <- concat <$> mapM (gen readers ps) cmds
  this     <- commandD ps name (map optName cmds)
  return (this:subDecls)

genA :: KnownNat n => ReadMF -> Prefix -> Name -> [Opt n] -> Q [Dec]
genA readers ps name fields = case lookupCustom name readers of
  Just custom -> pure <$> readArgD ps custom name
  Nothing     -> do
    subDecls <- concat <$> mapM (gen readers ps) fields
    this     <- appArgD ps name (map optName fields)
    return (this:subDecls)

--------------------------------------------------------------------------------

-- * Templates

pureD :: Prefix -> Name -> Q Dec
pureD p n = do
  let pName = funName p n
      pExp = [| pure $(conE n) |]
  funD pName [clause [] (normalB pExp) []]

autoArgD :: Prefix -> Name -> Q Dec
autoArgD p fieldName = do
  let pExp  = [| argument $lookupAuto $lookupMempty |]
      pName = funName p fieldName
  funD pName [clause [] (normalB pExp) []]

readArgD :: Prefix -> Name -> Name -> Q Dec
readArgD p reader fieldName = do
  let pExp  = [| argument $(varE reader) $lookupMempty |]
      pName = funName p fieldName
  funD pName [clause [] (normalB pExp) []]

appArgD :: Prefix -> Name -> [Name] -> Q Dec
appArgD p tyname fieldNames = do
  let pName = mkName $ "p" ++ p
  funD pName [clause [] (normalB $ appArgE p tyname fieldNames) []]

appArgE :: Prefix -> Name -> [Name] -> Q Exp
appArgE _ n []     = [| pure $(conE n) |]
appArgE p n (s:ss) = foldInfix
  (infixE (Just (conE n)) lookupFmap (Just (pArgE s)))
  lookupFapp
  (map pArgE ss)
  where pArgE t = varE $ funName p t

commandD :: Prefix -> Name -> [Name] -> Q Dec
commandD _ x      []     = bail "commandD: impossibru! no command in" x
commandD p tyname (c:cs) = do
  let pName = funName p tyname
      cmds  = foldInfix (commandE p c) lookupMappend (map (commandE p) cs)
      pExp  = [| subparser $cmds |]
  funD pName [clause [] (normalB pExp) []]

commandE :: Prefix -> Name -> Q Exp
commandE p name = do
  let commandStr = litE . stringL . (p++) . map toLower . stripCmd . nameBase $ name
      pInfo      = infoE name (funName p name)
  [| command $commandStr $pInfo |]

--------------------------------------------------------------------------------

(<+>) :: Prefix -> Name -> Prefix
(<+>) p n = p ++ nameBase n

reifyTy :: Type -> Q Info
reifyTy (ConT name) = reify name

-- | Folds some expressions with a left-associative infix operator.
--
foldInfix
  :: Q Exp -- ^ initial exp
  -> Q Exp -- ^ the operator
  -> [Q Exp] -> Q Exp
foldInfix e _ [] = e
foldInfix e m xs = L.foldl' mkInfix e xs
  where mkInfix acc x = infixE (Just acc) m (Just x)

infoE :: Name -> Name -> Q Exp
infoE _ pName =
  [| info $(varE pName) (progDesc "haddock here") |]

optName :: Opt n -> Name
optName (L x)    = optName x
optName (P0 n)   = n
optName (A0 n)   = n
optName (A1 n _) = n
optName (C0 n _) = n
optName (A2 n _) = n

mkReaders :: [(Name, Name)] -> ReadMF
mkReaders = map (first show)

lookupCustom :: Name -> ReadMF -> Maybe Name
lookupCustom n = L.lookup (show n)

concatNameS :: [String] -> Name
concatNameS = mkName . concat

funName :: Prefix -> Name -> Name
funName prefix name = concatNameS ["p", prefix, nameBase name]

areCommands :: [Con] -> Bool
areCommands = all (L.isPrefixOf "Cmd" . nameBase . conName)

stripCmd :: String -> String
stripCmd s = fromMaybe (bail "stripCmd: not a command" s) $ L.stripPrefix "Cmd" s

decName :: Dec -> Name
decName (NewtypeD _ n _ _ _) = n
decName (DataD    _ n _ _ _) = n
decName (TySynD   n _ _)     = n
decName x = bail "decName: can't handle" x


-- | Name of a constructor.
--
conName :: Con -> Name
conName (NormalC n _) = n
conName (RecC    n _) = n
conName _             = error "conName: can't handle this constructor"

-- | Type vars of a constructor.
--
conTypes :: Con -> [Type]
conTypes (NormalC _ t) = map snd t
conTypes (RecC    _ t) = map trd t
conTypes _             = error "conName: can't handle this constructor"

lookupAuto    = VarE . justInScope "auto"   <$> lookupValueName "auto"
lookupMempty  = VarE . justInScope "mempty" <$> lookupValueName "mempty"
lookupFmap    = VarE . justInScope "<$>" <$> lookupValueName "<$>"
lookupFapp    = VarE . justInScope "<*>" <$> lookupValueName "<*>"
lookupMappend = VarE . justInScope "<>"  <$> lookupValueName "<>"
lookupParser  = ConT . justInScope "ParserInfo" <$> lookupTypeName "ParserInfo"

justInScope :: String -> Maybe a -> a
justInScope n Nothing  = error ("not in scope: " <> n)
justInScope _ (Just a) = a

trd (_,_,x) = x

bail s x = error (s ++ ": " ++ show x)
