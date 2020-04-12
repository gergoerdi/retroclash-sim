{-# LANGUAGE RankNTypes #-}
module RetroClash.Sim.IO
    ( simulateIO
    , simulateIO_
    ) where

import Clash.Prelude
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad (void)

newSink :: IO ([a], a -> IO ())
newSink = do
    chan <- newChan
    list <- getChanContents chan
    let sink = writeChan chan
    return (list, sink)

newSource :: [a] -> IO (IO a)
newSource xs = do
    mvar <- newMVar xs
    return $ modifyMVar mvar $ \(out:outs) -> return (outs, out)

simulateIO
    :: (KnownDomain dom, MonadIO m, NFDataX i, NFDataX o)
    => (HiddenClockResetEnable dom => Signal dom i -> Signal dom o)
    -> i
    -> IO ((o -> m (i, a)) -> m a)
simulateIO circuit input0 = do
    (ins, sinkIn) <- newSink
    sourceOut <- newSource $ simulate circuit ins
    sinkIn input0
    return $ \world -> do
        out <- liftIO sourceOut
        (input, result) <- world out
        liftIO $ sinkIn input
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
