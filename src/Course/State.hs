{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec state s = snd (runState state s)


-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval state s = fst (runState state s)

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\x -> (x, x))


-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (\_ -> ((), s))

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f st = State $ \s -> case (runState st s) of
     (a, b) -> (f a, b)


-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure x = State (\s -> (x, s))
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) fa sa = State $ \s ->
        let (f, newS) = runState fa s
        in (runState ((<$>) f sa) newS)

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) fa sa = State $ \s ->
        let (a, newState) = runState sa s
        in (runState (fa a) newState)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil       = pure Empty
findM f (x :. xs) =  (=<<) (\a -> if (a == True) then (pure $ Full x) else (findM f xs)) (f x)

-- foldRight :: (a -> b -> b) -> b -> List a -> b

f :: Num s => Char -> State s Bool
f x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get

g :: Ord a => S.Set a -> a -> State b Bool
g x = \s -> (const $ pure (S.member s x)) =<< get


-- State s a :: s -> (a, s)
g' :: Ord a => S.Set a -> State a Bool
g' set = (\a -> State $ \s -> (S.member a set, s)) =<< get

--h :: Ord a => S.Set a -> State (S.Set a) Bool
--h set = (\a -> State $ \s -> (S.member a set, S.insert s set)) =<< get

-- g' set = (\s -> State $ (const $ pure (S.member set s))) =<< get

k :: Ord a => State a (S.Set a -> S.Set a)
k = State $ \s -> (\y -> S.insert s y, s)

--λ: >let x = (<*>) k (State $ \s -> (Data.Set.insert s Data.Set.empty, s))
--λ: >:t x
--x :: Ord a => State a (Set a)

-- z :: State (List a) (S.Set a -> S.Set a, Bool)
-- z = State $ \s -> case s of
--     Nil     -> ((const S.empty, False), Nil)
--     x :. xs -> let newSet = \set -> S.union set (S.insert x set) in 

--     ((\set -> S.insert x set, )))

foo :: Ord a => S.Set a -> State (List a) (S.Set a, Optional a) 
foo set = State $ \s -> case s of
      Nil     -> ((set, Empty)  , Nil)
      x :. xs -> let found = if (S.member x set) then (Full x) else Empty
                 in ((S.insert x set, found), xs) 
      

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat xs = 
  let p x = (\s -> (const $ pure (S.member x s)) =<< put (S.insert x s)) =<< get in eval (findM p xs) S.empty

--  findM :: (a -> f Bool) -> List a -> f (Optional a)


-- filtering ::
  -- Applicative f =>
  -- (a -> f Bool)
  -- -> List a
  -- -> f (List a)

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct xs =
  let p x = (\s -> (const $ pure (not $ S.member x s)) =<< put (S.insert x s)) =<< get in eval (filtering p xs) S.empty

distinctA ::
  Ord a =>
  List a
  -> List a
distinctA xs =
   eval (filtering p xs) S.empty
    where p x = (\s -> (const $ pure (not $ S.member x s)) =<< put (S.insert x s)) =<< get

-- produce :: (a -> a) -> a -> List a

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
-- 7^2 = 49
-- 4^2 + 9^2 = 16 + 81 = 97
-- 9^2 + 7^2 = 81 + 49 = 130
-- 1^2 + 3^2 + 0^2 = 1 + 9 + 0 = 10
-- 1^0 + 0^2 = 1 + 0 = 1 --> HAPPY
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy int =
  case firstRepeat (produce (exec happyHelper) int) of
    Empty  -> True
    Full i -> if(i == 1) then True else False

-- input: 49
-- output (False, 97)
happyHelper :: State Integer Bool
happyHelper = State $ \s ->
  let
    digits  = digs s
    sumd    = sum' digits -- Integer 
    squared = P.fmap (\i -> (P.^) i 2) digits -- List Integer
    next    = sum' squared -- Integer
  in
    if(sumd == 1) then (True, next) else (False, next)

-- credit: https://stackoverflow.com/a/3963286/409976
digs :: Integral a => a -> List a
digs 0 = 0 :. Nil 
digs n = digs' n
  where digs' 0 = Nil
        digs' n = digs' (n `div` 10) ++ ((n `mod` 10) :. Nil)
