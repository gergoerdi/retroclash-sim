{-# LANGUAGE TupleSections #-}
module RetroClash.Sim.IO
    ( driveIO
    , driveIO_
    ) where

import Clash.Prelude hiding (lift)
import Control.Concurrent
import Control.Monad.IO.Class

driveIO :: (MonadIO m) => ([i] -> [o]) -> i -> IO ((o -> m (i, a)) -> m a)
driveIO circuit input0 = do
    inChan <- newChan
    writeChan inChan input0

    ins <- getChanContents inChan
    outs <- newMVar $ circuit ins

    return $ \world -> do
        out <- liftIO $ modifyMVar outs $ \(out:outs) -> return (outs, out)
        (input, result) <- world out
        liftIO $ writeChan inChan input
        return result

driveIO_ :: (MonadIO m) => ([i] -> [o]) -> i -> IO ((o -> m i) -> m ())
driveIO_ circuit input0 = do
    sim <- driveIO circuit input0
    return $ \world -> sim $ \out -> (,()) <$> world out
