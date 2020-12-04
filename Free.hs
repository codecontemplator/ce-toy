{-# LANGUAGE InstanceSigs, Rank2Types, TypeOperators, DeriveFunctor #-}

module Free where

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap :: (a->b) -> Free f a -> Free f b 
  fmap f (Pure a) = Pure $ f a
  fmap f (Free g) = Free $ fmap (fmap f) g 

instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure
  (<*>) :: Free f (a->b) -> Free f a -> Free f b
  (Pure f) <*> as = fmap f as
  (Free faf) <*> as = Free $ fmap (<*>as) faf

instance Functor f => Monad (Free f) where
  return :: a -> Free f a
  return = Pure
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Pure a) >>= fm = fm a
  (Free faf) >>= fm = Free $ fmap (>>=fm) faf

type f ~> g = forall x . f x -> g x

freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure a) = Pure a
freeM phi (Free faf) = Free $ phi $ fmap (freeM phi) faf 

monad :: Monad m => Free m ~> m
monad (Pure a) = pure a
monad (Free maf) = do
  af <- maf
  monad af

interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi

liftF :: Functor f => f ~> Free f
liftF f = Free $ fmap Pure f
