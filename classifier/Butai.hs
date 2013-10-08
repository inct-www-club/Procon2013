{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Butai where

import Data.Karakuri
import Graphics.UI.FreeGame

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Operational.Mini
import Control.Monad.Operational.TH
import qualified Data.IntMap as IM
import Control.Comonad

import Unsafe.Coerce
import Control.Monad.Free
import GHC.Prim

newtype K a = Kao Int

data ButaiBase m a where
    Register :: Karakuri m r -> ButaiBase m (K r)
    Look :: K r -> ButaiBase m r
    UpdateAll :: ButaiBase m ()
makeSingletons ''ButaiBase

transButaiBase :: (forall a. m a -> n a) -> ButaiBase m a -> ButaiBase n a
transButaiBase t (Register k) = Register (transKarakuri t k)
transButaiBase _ (Look k) = Look k
transButaiBase _ UpdateAll = UpdateAll

newtype ButaiT m a = ButaiT { unButaiT :: ReifiedProgramT (ButaiBase (ButaiT m)) m a } deriving (Monad, Applicative, Functor)

instance MonadTrans ButaiT where
    lift = ButaiT . lift

instance Monad m => ButaiBase (ButaiT m) :! ButaiT m where
    singleton = ButaiT . singleton

instance MonadState s m => MonadState s (ButaiT m) where
    get = lift get
    put = lift . put

instance MonadIO m => MonadIO (ButaiT m) where
    liftIO = lift . liftIO

instance (Monad m, Picture2D m) => Picture2D (ButaiT m) where
    fromBitmap = lift . fromBitmap
    translate t = transButaiT (translate t)
    rotateD r = transButaiT (rotateD r)
    rotateR r = transButaiT (rotateR r)
    scale s = transButaiT (scale s)
    colored c = transButaiT (colored c)

instance (Monad m, Figure2D m) => Figure2D (ButaiT m) where
    line = lift . line
    circle = lift . circle
    circleOutline = lift . circleOutline
    polygon = lift . polygon
    polygonOutline = lift . polygonOutline
    thickness t = transButaiT (thickness t)

instance (Monad m, Mouse m) => Mouse (ButaiT m) where
    mousePosition = lift mousePosition
    mouseButtonL = lift mouseButtonL
    mouseButtonR = lift mouseButtonR
    mouseWheel = lift mouseWheel

instance (Monad m, Keyboard m) => Keyboard (ButaiT m) where
    keyChar = lift . keyChar
    keySpecial = lift . keySpecial

transButaiT :: (Monad m, Monad n) => (forall x. m x -> n x) -> ButaiT m a -> ButaiT n a
transButaiT t = ButaiT . hoistReifiedT (transButaiBase (transButaiT t)) . transReifiedT t . unButaiT

runButaiT :: forall m a. Monad m => ButaiT m a -> m a
runButaiT = go (0 :: Int) (IM.empty :: IM.IntMap (Karakuri (ButaiT m) Any)) . unButaiT where
    go i m (Register k :>>= cont) = go (succ i) (IM.insert i (unsafeCoerce k) m) $ cont (Kao i)
    go i m (Look k@(Kao j) :>>= cont) = go i m $ cont $ extract (unsafeCoerce (m IM.! j) `asKarakuriOf` k)
    go i m (UpdateAll :>>= cont) = do
        rs <- runButaiT $ forM (IM.toAscList m) (\(i, m) -> (,) i `liftM` step m)
        go i (IM.fromAscList rs) (cont ())
    go i m (Lift a cont) = a >>= go i m . cont
    go _ _ (Return a) = return a

    asKarakuriOf :: Karakuri m x -> p x -> Karakuri m x
    asKarakuriOf x _ = x
