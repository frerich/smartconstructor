smartconstructor
================

This project builds an Haskell module `SmartConstructor` which exports a
`makeSmartCtor` function which can be used to generate
[smart constructors](http://www.haskell.org/haskellwiki/SmartConstructors) for Haskell
types. See
[the Haskell Wiki](http://wiki.haskell.org/Smart_constructors) for a more in-depth
discussion of smart constructors in Haskell.

Smart constructors are useful for imposing additional checks on values;
given e.g.

    {-# LANGUAGE TemplateHaskell #-}

    import SmartConstructor

    newtype Positive = Positive Int
    newtype NonEmptyList a = NonEmptyList [a]
    newtype Interval = Interval (Integer, Integer)

You can use 'makeSmartCtor' to generate smart constructors as follows:

    -- Defines 'makePositive :: Int -> Maybe Positive'
    makeSmartCtor defaultOptions ''Positive [|(> 0)|]

    -- Defines 'makeNonEmptyList :: [a] -> Maybe (NonEmptyList a)
    makeSmartCtor defaultOptions ''NonEmptyList [|not . null|]

Notice how the third argument defines a predicate; the generated functions
apply this predicate to the given value: if it yields true, the smart
constructor call evaluates to a 'Just' value. If the predicate yields false,
the smart constructor evaluates to 'Nothing'.

By default, the name for the smart constructor is derived from the
type name. A custom name can be specified by modifying the 'ctorName'
field of the defaultOptions:

    -- Defines 'createIV :: (Integer, Integer) -> Maybe Interval
    makeSmartCtor defaultOptions{ ctorName = "createIV" } ''Interval [|uncurry (<=)|]
