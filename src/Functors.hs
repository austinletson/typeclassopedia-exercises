module Functors
  ( testFunctorsModule,
    MyMaybe,
  )
where

data MyMaybe a = MyJust a | MyNothing
  deriving (Eq, Ord)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap g (MyJust x) = MyJust (g x)

data MyEither e a = MyLeft e | MyRight a

instance Functor (MyEither e) where
  fmap _ (MyLeft e) = MyLeft e
  fmap g (MyRight x) = MyRight (g x)

-- data MyReader e =
