{-# LANGUAGE RankNTypes #-}
module RetroClash.Sim.IO
    ( simulateIO
    , simulateIO_
    ) where

import Clash.Prelude
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad (void)
import Control.Arrow.Transformer.Automaton

simulateIO
    :: (KnownDomain dom, MonadIO m, NFDataX i, NFDataX o)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> i
    -> IO ((o -> m (i, a)) -> m a)
simulateIO circuit input0 = do
    let Automaton step = signalAutomaton circuit
    ref <- newMVar $ step input0
    return $ \world -> do
        (out, Automaton step) <- liftIO $ takeMVar ref
        (input, result) <- world out
        liftIO $ putMVar ref $ step input
        return result

simulateIO_
    :: (KnownDomain dom, MonadIO m, NFDataX i, NFDataX o)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> i
    -> IO ((o -> m i) -> m ())
simulateIO_ circuit input0 = do
    sim <- simulateIO circuit input0
    return $ \world -> do
        void $ sim $ \output -> do
            input <- world output
            return (input, ())
