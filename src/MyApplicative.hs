{-# LANGUAGE InstanceSigs #-}

module MyApplicative where

import Control.Applicative

-- class (Functor f) => MyApplicative f where
--     pure :: a -> f a
--     infixl 4 <*>, *>, <*

--     -- Function application within a computational context
--     (<*>) :: f (a -> b) -> f a -> f b
--     liftA2 :: (a -> b -> c) -> f a -> f b -> f c
--     liftA2 f x y = f <$> x MyApplicative.<*> y

--     (*>) :: f a -> f b -> f b
--     a1 *> a2 = (id <$ a1) MyApplicative.<*> a2

--     (<*) :: f a -> f b -> f a
--     (<*) = MyApplicative.liftA2 const

-- Laws
-- Together these 4 laws allow for application of pure and <*> with a single use of pure at the beginning and left-nested occurances of <*>

-- Identity
-- pure id <*> v = v

-- Homomorphism
-- pure f <*> pure x = pure (f x)
-- non-effectful functions with non-effectful arguments result in a pure application of the function on the argument in an effectful context

-- Interchange
-- u <*> pure y = pure (\f -> f y) <*> u
-- u <*> pure y = pure ($ y) <*> u
-- The order of evaluation doesn't matter when evaluating an effectful function on a pure argument

-- Composition
-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
-- A sort of associativity property of <*>

-- Functor Applicative Law
-- fmap g x = pure g <*> x
-- fmap <$> x = pure g <*> x
-- mapping g over a context x is the same as first injecting g into the context with pure and then applying x with <*>

instance MyApplicative Maybe where
    Just f <*> Just x = Just (f x)
    _ <*> _ = Nothing

newtype ZipList a = ZipList {getZipList :: [a]}

instance MyApplicative ZipList where
    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
    pure a = ZipList [a]

sequenceAL :: (Applicative f) => [f a] -> f [a]
sequenceAL = foldl (<*>) (pure [])
