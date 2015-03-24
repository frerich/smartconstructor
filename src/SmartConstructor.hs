{-|
Module:         SmartConstructor
Copyright:      (c) 2015 Frerich Raabe
License:        BSD3
Maintainer:     frerich.raabe@gmail.com
Stability:      experimental

This module exposes a 'makeSmartCtor' function which automatically creates
smart constructors for custom newtype'd Haskell types. See
<http://wiki.haskell.org/Smart_constructors> for a more in-depth
discussion of smart constructors in Haskell.

Smart constructors are useful for imposing additional checks on values;
given e.g.

> {-# LANGUAGE TemplateHaskell #-}
>
> import SmartConstructor
>
> newtype Positive = Positive Int
> newtype NonEmptyList a = NonEmptyList [a]
> newtype Interval = Interval (Integer, Integer)

You can use 'makeSmartCtor' to generate smart constructors as follows:

> -- Defines 'makePositive :: Int -> Maybe Positive'
> makeSmartCtor defaultOptions ''Positive [|(> 0)|]
>
> -- Defines 'makeNonEmptyList :: [a] -> Maybe (NonEmptyList a)
> makeSmartCtor defaultOptions ''NonEmptyList [|not . null|]

Notice how the third argument defines a predicate; the generated functions
apply this predicate to the given value: if it yields true, the smart
constructor call evaluates to a 'Just' value. If the predicate yields false,
the smart constructor evaluates to 'Nothing'.

By default, the name for the smart constructor is derived from the
type name. A custom name can be specified by modifying the 'ctorName'
field of the defaultOptions:

> -- Defines 'createIV :: (Integer, Integer) -> Maybe Interval
> makeSmartCtor defaultOptions{ ctorName = "createIV" } ''Interval [|uncurry (<=)|]
-}

{-# LANGUAGE LambdaCase #-}

module SmartConstructor
    ( SmartCtorOptions(..)
    , defaultOptions
    , makeSmartCtor
    )
    where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (trueName, nothingName, justName, mkNameG, NameSpace(TcClsName))

{-|
    Values of the 'SmartCtorOptions' type can be passed to 'makeSmartCtor' in
    order to customize the generated constructor functions. At this point,
    only the name of the function can be changed.
-}
data SmartCtorOptions = SmartCtorOptions {
    {-|
        The desired name for the smart constructor function. An empty string will make
        'makeSmartCtor' derive the function name from the type by prepending 'make'
        to the type name.
    -}
    ctorName :: String
}

{-|
    The default smart constructor generation options; the smart constructor
    function will be named after the type, e.g.

    > makeSmartCtor defaultOptions ''Foo [|const True|]

    defines a function 'makeFoo'.
-}
defaultOptions :: SmartCtorOptions
defaultOptions = SmartCtorOptions ""

-- Alas, not available in Language.Haskell.TH.Syntax
maybeName :: Name
maybeName = mkNameG TcClsName "base" "Data.Maybe" "Maybe"

makeFuncT :: Type -> Type -> Type
makeFuncT a = AppT (AppT ArrowT a)

typeConType :: Name -> [Name] -> Type
typeConType typeName typeVars = foldl AppT (ConT typeName) (map VarT typeVars)

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV n)    = n
tyVarName (KindedTV n _) = n

conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC n _)      = n
conName (InfixC _ n _)  = n
conName (ForallC _ _ c) = conName c

-- |The 'makeSmartCtor' function creates a smart constructor for the given type, using the given predicate.
makeSmartCtor :: SmartCtorOptions -- Options to customize the smart constructor function; the name of the defined function can be changed.
              -> Name             -- The type to generate a smart constructor function for.
              -> Q Exp            -- A predicate which is applied to the smart constructor argument to decide whether a Just value or Nothing is returned.
              -> Q [Dec]
makeSmartCtor opts typeName predicate = do
    predExp <- predicate
    (dataCtor, tyVarBndrs) <- reify typeName >>= \case
        TyConI (NewtypeD _ _ tyVarBndrs ctor _) -> return (ctor, tyVarBndrs)
        _                                       -> fail "smartCtor: Expected name of newtype'd type constructor"
    sequence [ctorSignature tyVarBndrs dataCtor, ctorDefinition predExp dataCtor]
  where
    ctorSignature :: [TyVarBndr] -> Con -> Q Dec
    ctorSignature tyVarBndrs con = do
        let tyVarNames = map tyVarName tyVarBndrs
        let resultType = AppT (ConT maybeName) (typeConType typeName tyVarNames)
        return (SigD ctorName_ (ForallT tyVarBndrs [] (makeFuncT (innerType con) resultType)))
      where
        innerType (NormalC _ [(_, t)]) = t
        innerType (RecC _ [(_, _, t)]) = t
        innerType _                    = undefined

    ctorDefinition :: Exp -> Con -> Q Dec
    ctorDefinition predExp con =
          return (FunD ctorName_ [Clause [VarP argName] ctorBody []])
      where
        ctorBody = GuardedB [ (NormalG (AppE predExp (VarE argName)), AppE (ConE justName) (AppE (ConE (conName con)) (VarE argName)))
                            , (NormalG (ConE trueName), ConE nothingName)
                            ]
        argName = mkName "x"

    ctorName_ :: Name
    ctorName_ = mkName $
        if null (ctorName opts)
            then "make" ++ nameBase typeName
            else ctorName opts

