{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Sim.VGA
    ( vgaSink
    , initSink
    , SinkState
    ) where

import RetroClash.Utils
import RetroClash.VGA

import Control.Monad.State
import Data.Foldable (for_)
import Control.Lens hiding (Index)
import Clash.Prelude hiding (lift)
import Data.Proxy

import Debug.Trace

vgaRetrace :: VGATiming visible -> (Int, Bit)
vgaRetrace VGATiming{..} = (snatToNum pulseWidth + snatToNum postWidth - 1, toActiveDyn polarity True)

data SinkState
    = Visible Int
    | WaitSync Bool
    | Retrace Int
    deriving (Show)

{-# INLINE vgaSink #-}
vgaSink
    :: forall w h rgb m ps. (KnownNat w, KnownNat h, MonadState (SinkState, SinkState) m)
    => VGATimings ps w h
    -> (Int -> Int -> rgb -> m ())
    -> (Bit, Bit, rgb)
    -> m Bool
vgaSink VGATimings{..} paint (hsync0, vsync0, color) = do
    (x, endLine) <- stateZoom _1 $ direction w horizRetrace hsync
    (y, endFrame) <- stateZoomIf endLine _2 $ direction h vertRetrace vsync
    for_ (liftA2 (,) x y) $ \(x, y) -> paint x y color
    return $ endLine && endFrame
  where
    (horizRetrace, hsyncTarget) = vgaRetrace vgaHorizTiming
    (vertRetrace, vsyncTarget) = vgaRetrace vgaVertTiming

    vsync = vsync0 == vsyncTarget
    hsync = hsync0 == hsyncTarget

    w = snatToNum (SNat @w)
    h = snatToNum (SNat @h)

    direction vis retrace sync s = case s of
        Retrace n -> ((Nothing, False), s')
          where
            n' = n + 1
            s' = if n' == retrace then Visible 0 else Retrace n'
        Visible i -> ((Just i, False), s')
          where
            i' = i + 1
            s' = if i' == vis then WaitSync sync else Visible i'
        WaitSync prevSync -> ((Nothing, end), s')
          where
            end = not prevSync && sync
            s' = if end then Retrace 0 else WaitSync sync

initSink :: (SinkState, SinkState)
initSink = (WaitSync False, WaitSync False)

{-# INLINE stateZoom #-}
stateZoom :: (MonadState s0 m) => Lens' s0 s -> (s -> (a, s)) -> m a
stateZoom = stateZoomIf True

{-# INLINE stateZoomIf #-}
stateZoomIf :: (MonadState s0 m) => Bool -> Lens' s0 s -> (s -> (a, s)) -> m a
stateZoomIf update l f = state $ \s0 ->
  let s = view l s0
      (x, s') = f s
      s0' = s0 & l .~ s'
  in (x, if update then s0' else s0)
