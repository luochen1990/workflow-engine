{-# language GADTs,AllowAmbiguousTypes,ScopedTypeVariables,KindSignatures #-}
module Core where

import Control.Applicative
import Control.Category
import Control.Arrow
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M

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
    id = Pass
    (.) = flip Pipe

instance Arrow Flow where
    arr f = Wrap f Prelude.id Control.Category.id
    (***) = Join

instance ArrowChoice Flow where
    (+++) t1 t2 = Branch (arr isLeft) (Wrap (fromLeft undefined) Left t1) (Wrap (fromRight undefined) Right t2)

instance ArrowApply Flow where
    app = Task $ \(f, x) -> Direct <$> execFlow f x

-- * execution

execFlow :: Flow a b -> a -> IO b
execFlow tsk x = case tsk of
    Pass -> pure x
    Task f -> do
        res <- f x
        case res of
            Direct r -> pure r
            ProxyTo t -> execFlow t x
    Wrap f g t -> g <$> execFlow t (f x)
    Pipe t1 t2 -> execFlow (t1 >>> t2) x
    Branch tp ta tb -> do
        p <- execFlow tp x
        execFlow (if p then ta else tb) x
    Race ta tb -> race (execFlow ta (fst x)) (execFlow tb (snd x))
    Join ta tb -> concurrently (execFlow ta (fst x)) (execFlow tb (snd x))

-- * inspect progress

-- | a variable signal
data VarSignal a
type VS = VarSignal

glimpse :: VS a -> IO a
glimpse = undefined

-- | Working Progress of "Flow a b"
data Progress a b = Pend | WIP | Prox (Flow a b) (Map Name (VS (Progress a b))) | Done

type Name = String

runFlow :: Flow a b -> a -> IO (Map Name (VS (Progress a b)), IO b)
runFlow = undefined

main :: IO ()
main = undefined

