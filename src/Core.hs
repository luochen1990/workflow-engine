{-# language GADTs,AllowAmbiguousTypes,ScopedTypeVariables,KindSignatures #-}
module Core where

import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Either

data Flow :: * -> * -> * where
    --ProxyTask :: Flow a b -> Flow a b
    Pass :: Flow a a
    Task :: (a -> IO (TaskResponse a b)) -> Flow a b
    Wrap :: (a -> a') -> (b' -> b) -> Flow a' b' -> Flow a b
    Pipe :: Flow a b -> Flow b c -> Flow a c
    Branch :: Flow a Bool -> Flow a b -> Flow a b -> Flow a b
    Race :: Flow a b -> Flow c d -> Flow (a, c) (Either b d)
    Join :: Flow a b -> Flow c d -> Flow (a, c) (b, d)

-- | this is isomorphism to "Either b (Flow a b)"
data TaskResponse a b where
    Direct :: b -> TaskResponse a b
    ProxyTo :: Flow a b -> TaskResponse a b

instance Category Flow where
    id = Pass --Task (pure Prelude.. Direct)
    (.) = flip Pipe

instance Arrow Flow where
    arr f = Wrap f Prelude.id Control.Category.id
    (***) = Join

instance ArrowChoice Flow where
    (+++) t1 t2 = Branch (arr isLeft) (Wrap (fromLeft undefined) Left t1) (Wrap (fromRight undefined) Right t2)

instance ArrowApply Flow where
    app = _appFlow

-- * execution

execTask :: Flow a b -> a -> IO b
execTask tsk x = case tsk of
    Pass -> pure x
    Task f -> do
        res <- f x
        case res of
            Direct r -> pure r
            ProxyTo t -> execTask t x
    Wrap f g t -> g <$> execTask t (f x)
    Pipe t1 t2 -> execTask (t1 >>> t2) x
    Branch tp ta tb -> do
        p <- execTask tp x
        execTask (if p then ta else tb) x
    Race ta tb -> race (execTask ta (fst x)) (execTask tb (snd x))
    Join ta tb -> concurrently (execTask ta (fst x)) (execTask tb (snd x))

-- * inspect progress

data VarSignal a
data Progress = Pend | Wait | WIP | Prox | Done

runFlow :: Flow a b -> a -> IO (VarSignal Progress, IO b)
runFlow = undefined

glimpse :: VarSignal a -> IO a
glimpse = undefined

main :: IO ()
main = undefined

