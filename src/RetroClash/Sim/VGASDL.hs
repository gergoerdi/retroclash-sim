{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module RetroClash.Sim.VGASDL
    ( vgaSinkBuf
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Sim.VGA
import RetroClash.Sim.SDL

import Control.Monad.State
import Data.Word
import Data.Array.IO

{-# INLINE vgaSinkBuf #-}
vgaSinkBuf
    :: (KnownNat w, KnownNat h, MonadIO m, MonadState (SinkState w, SinkState h) m)
    => VGATimings ps w h
    -> BufferArray w h
    -> (Bit, Bit, Color)
    -> m Bool
vgaSinkBuf vgaMode (BufferArray arr) = vgaSink vgaMode writeBuf
  where
    writeBuf x y rgb = liftIO $ writeArray arr (x, y) $ packColor rgb
