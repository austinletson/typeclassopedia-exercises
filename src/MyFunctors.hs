module MyFunctors
  ( MyMaybe,
    MyEither,
    MyReader,
  )
where

class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap' . const

data MyMaybe a = MyJust a | MyNothing
  deriving (Eq, Ord)

instance MyFunctor MyMaybe where
  fmap' _ MyNothing = MyNothing
  fmap' g (MyJust x) = MyJust (g x)

data MyEither e a = MyLeft e | MyRight a

instance MyFunctor (MyEither e) where
  fmap' _ (MyLeft e) = MyLeft e
  fmap' g (MyRight x) = MyRight (g x)

newtype MyReader e a = MyReaderArrow ((->) e a)

instance MyFunctor (MyReader e) where
  -- fmap' :: (d -> c) -> MyReader e d -> MyReader e c
  -- fmap' :: (d -> c) -> (e -> d) -> (e -> c)
  -- d :: a
  -- c :: a
  -- g :: (d -> c)
  -- h :: (e -> d)
  -- return :: (e -> c)
  fmap' g (MyReaderArrow h) = MyReaderArrow (g . h)

data MyAnnotation e a = MyAnnotationComma e a

instance MyFunctor (MyAnnotation e) where
  fmap' g (MyAnnotationComma e a) = MyAnnotationComma e (g a)

data Pair a = Pair a a

instance MyFunctor Pair where
  -- fmap' :: (c -> d) -> Pair c -> Pair d
  fmap' g (Pair a1 a2) = Pair (g a1) (g a2)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance MyFunctor ITree where
  -- fmap' :: (c -> d) -> Leaf (Int -> c) -> Leaf (Int -> d)
  -- g :: (c -> d)
  -- l :: (Int -> c)
  fmap' g (Leaf l) = Leaf (g . l)
  -- fmap' :: (c -> d) -> Node [ITree c] -> Node [ITree d]
  fmap' g (Node subTrees) = Node (map (fmap' g) subTrees)

-- Type that of kind (* -> *) that cannot be made an instance of MyFunctor
data T a = T (a -> Char)

newtype MyCompose f g x = MyCompose (f (g x))

instance (MyFunctor f, MyFunctor g) => MyFunctor (MyCompose f g) where
  fmap' f (MyCompose x) = MyCompose (fmap' (fmap' f) x)

-- MyFunctor Laws
-- fmap' id = id
-- fmap' (g . h) = (fmap' g) . (fmap' h)

-- Example of (bogus) MyFunctor that satisfies the second functor law but not the first
data BogusPair a = BogusPair a a

instance (Eq a) => Eq (BogusPair a) where
  BogusPair a1 a2 == BogusPair b1 b2 = a1 == b1 && a2 == b2
  p1 /= p2 = not (p1 == p2)

-- fmap' id \= id
-- BogusPair (id a1) (id a2) \= id (BogusPair a1 a2)

-- fmap' (g . h) = (fmap' g) . (fmap' h)
-- BogusPair ((g . h) a1) ((g . h) a2) = BogusPair (g (h a1)) (g (h a2))
instance MyFunctor BogusPair where
  -- fmap' :: (c -> d) -> Pair c -> Pair d
  fmap' g (BogusPair a1 a2) = BogusPair (g a1) (g a2)

bogusPair1 :: BogusPair Int
bogusPair1 = BogusPair 1 1

bogusId = fmap' id bogusPair1 == id bogusPair1

-- Evil Functor instance
instance MyFunctor [] where
  fmap' _ [] = []
  fmap' g (x : xs) = g x : g x : fmap' g xs

-- This Functor instance violates the first and second law
testList = [1, 2, 3]

evilId = (fmap' id) testList == id testList

timesTwo = (*) 2

evilComposition = fmap' (timesTwo . timesTwo) testList == ((fmap' timesTwo) . (fmap' timesTwo)) testList
