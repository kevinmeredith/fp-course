{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

printCompose :: Show a => Compose Optional Optional a -> List Char
printCompose (Compose fga) = "Compose(" ++ (show' fga) ++ ")"

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) f (Compose fga) = Compose $ (\a -> f <$> a) <$> fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose $ pure $ pure a
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose f) (Compose fga) = Compose $ ((\gab -> \ga -> gab <*> ga) <$> f) <*> fga

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
