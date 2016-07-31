module Chapter8.Exercises where

import Prelude
import Control.Monad (class Monad)
import Control.Monad.Eff (forE, Pure, Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (error, Error, throwException, EXCEPTION)
import Control.Monad.Eff.Random (random, RANDOM)
import Control.Monad.ST (readSTRef, modifySTRef, newSTRef, ST, runST)
import Data.Array (length, filter, cons, foldM, sort, nub, head, tail)
import Data.Int (toNumber)
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe)

third :: forall a. Array a -> Maybe a
third a = do
    t <- tail a
    t' <- tail t
    head t'

sums :: Array Int -> Array Int
sums = nub <<< sort <<< foldM (\acc x-> cons (acc + x) [acc]) 0

filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _  Nil = pure Nil
filterM p (Cons x xs) = do
    yes <- p x
    if yes then Cons x <$> (filterM p xs) else filterM p xs

map' :: forall a b m. Monad m => (a -> b) -> m a -> m b
map' f m = m >>= f >>> pure

safeDivide :: forall e. Number -> Number -> Eff (err:: EXCEPTION | e) Number
safeDivide _ 0.0 = throwException $ error "Can't divide by zero"
safeDivide n d = pure $ n / d

type Point = {x:: Number, y:: Number}

estPi :: forall e. Int -> Eff (console :: CONSOLE, st:: ST (Array Point), random:: RANDOM | e) Number
estPi pts = do
    s <- newSTRef []
    forE 0 pts $ \_ -> do
        x <- random
        y <- random
        modifySTRef s $ \xs -> cons {x: x, y: y} xs
        pure unit
    pts <- readSTRef s
    pure $ doEst <<< length <<< filter inUnitCircle $ pts
    where
        rSquared = 0.5 * 0.5
        inUnitCircle :: Point -> Boolean
        inUnitCircle {x, y} = let
            a = x - 0.5
            b = y - 0.5
            in rSquared > ((a * a) + (b * b))
        doEst n = (4.0 * (toNumber n)) / (toNumber pts)


