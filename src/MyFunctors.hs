module MyFunctors where

-- Intuition for Functor
-- fmap takes a single parameter and returns a function (see parenthese in type declerat)

class MyFunctor f where
  fmap' :: (a -> b) -> (f a -> f b)
  (<$) :: a -> f b -> f a
  (<$) = fmap' . const

instance MyFunctor Maybe where
  fmap' _ Nothing = Nothing
  fmap' g (Just x) = Just (g x)

instance MyFunctor (Either e) where
  fmap' _ (Left e) = Left e
  fmap' g (Right a) = Right (g a)

instance MyFunctor ((->) e) where
  fmap' g h = g . h

instance MyFunctor ((,) e) where
  fmap' g (e, a) = (e, g a)

--   fmap' g ((,) e a) = (,) e (g a)

data Pair a = Pair a a

instance MyFunctor Pair where
  fmap' g (Pair a b) = Pair (g a) (g b)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance MyFunctor ITree where
  -- fmap' :: (c -> d) -> Leaf (Int -> c) -> Leaf (Int -> d)
  -- g :: (c -> d)
  -- l :: (Int -> c)
  fmap' g (Leaf l) = Leaf (g . l)
  -- fmap' :: (c -> d) -> Node [ITree c] -> Node [ITree d]
  fmap' g (Node subTrees) = Node (map (fmap' g) subTrees)

data NotFunctor a = NotFunctor (a -> Bool)

newtype MyCompose' f g x = MyCompose' (f (g x))

instance (MyFunctor f1, MyFunctor f2) => MyFunctor (MyCompose' f1 f2) where
  -- fmap :: (a -> b) -> (MyCompose' (h1 (h2 a))) -> (MyCompose' (h1 (h2 b)))
  fmap' g (MyCompose' x) = MyCompose' (fmap' (fmap' g) x)

newtype MyReader e a = MyReaderArrow ((->) e a)

-- Type that of kind (* -> *) that cannot be made an instance of MyFunctor
data T a = T (a -> Char)

-- MyFunctor Laws
-- fmap' id = id
-- fmap' (g . h) = (fmap' g) . (fmap' h)

-- Example of (bogus) MyFunctor that satisfies the second functor law but not the first
data BogusPair a = BogusPair a a

instance (Eq a) => Eq (BogusPair a) where
  BogusPair a1 a2 == BogusPair b1 b2 = a1 == b1 && a2 == b2
  p1 /= p2 = not (p1 == p2)

-- fmap' (g . h) = (fmap' g) . (fmap' h)
-- BogusPair ((g . h) a1) ((g . h) a2) = BogusPair (g (h a1)) (g (h a2))
instance MyFunctor BogusPair where
  -- fmap' :: (c -> d) -> Pair c -> Pair d
  fmap' g (BogusPair a1 a2) = BogusPair (g a1) (g a2)

class BogusFunctor f where
  fmap'' :: (a -> b) -> (f a -> f b)

instance BogusFunctor [] where
  fmap'' _ _ = []

bogusPair1 :: BogusPair Int
bogusPair1 = BogusPair 1 1

bogusId = fmap' id bogusPair1 == id bogusPair1

-- fmap' id = id
-- fmap' (g . h) \= (fmap' g) . (fmap' h)

data BogusMaybe a = BogusNothing | BogusJust a

-- instance MyFunctor BogusMaybe where
--   fmap' g BogusNothing = BogusNothing
--   fmap' g (BogusJust a) = if g == id then BogusJust (g a) else BogusNothing

-- Evil Functor instance
instance MyFunctor [] where
  fmap' _ [] = []
  fmap' g (x : xs) = g x : g x : fmap' g xs

-- This Functor instance violates the first and second law
testList = [1, 2, 3]

evilId = (fmap' id) testList == id testList

timesTwo = (*) 2

evilComposition = fmap' (timesTwo . timesTwo) testList == ((fmap' timesTwo) . (fmap' timesTwo)) testList
