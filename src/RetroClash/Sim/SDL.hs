{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
module RetroClash.Sim.SDL
    ( VideoParams(..)
    , withMainWindow
    , Rasterizer

    , BufferArray(..)
    , newBufferArray

    , rasterizePattern
    , rasterizeBuffer

    , Color
    , packColor
    ) where

import Prelude
import Clash.Prelude hiding (lift)
import RetroClash.Utils

import SDL hiding (get)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Control.Monad
import Control.Monad.IO.Class
import Data.Array.IO
import Data.IORef
import Control.Monad.Loops (whileJust_)

newtype Rasterizer (w :: Nat) (h :: Nat) = Rasterizer{ runRasterizer :: Ptr () -> Int -> IO () }

data VideoParams = MkVideoParams
    { windowTitle :: Text
    , screenScale :: CInt
    , screenRefreshRate :: Word32
    }

withMainWindow
    :: forall w h m. (KnownNat w, KnownNat h, MonadIO m)
    => VideoParams
    -> ([Event] -> (Scancode -> Bool) -> m (Maybe (Rasterizer w h)))
    -> m ()
withMainWindow MkVideoParams{..} runFrame = do
    initializeAll
    window <- createWindow windowTitle defaultWindow
    windowSize window $= fmap (screenScale *) screenSize

    renderer <- createRenderer window (-1) defaultRenderer
    texture <- createTexture renderer RGB888 TextureAccessStreaming screenSize

    let withTexture drawTo = do
            (ptr, rowstride) <- lockTexture texture Nothing
            drawTo ptr (fromIntegral rowstride)
            unlockTexture texture
            SDL.copy renderer texture Nothing Nothing
            present renderer

    let render rasterizer = withTexture $ \ptr rowstride ->
            liftIO $ runRasterizer rasterizer ptr rowstride

    let prepareFrame = do
            before <- ticks
            events <- pollEvents
            keys <- getKeyboardState
            let windowClosed = any isWindowCloseEvent events
            rasterizer <- if windowClosed then return Nothing else runFrame events keys
            return $ (before,) <$> rasterizer

        drawFrame (before, rasterizer) = do
            render rasterizer
            after <- ticks
            let elapsed = after - before
            when (elapsed < frameTime) $ liftIO $ threadDelay (fromIntegral (frameTime - elapsed) * 1000)

    whileJust_ prepareFrame drawFrame
  where
    frameTime = 1000 `div` screenRefreshRate
    screenSize = V2 (snatToNum (SNat @w)) (snatToNum (SNat @h))

    isWindowCloseEvent ev = case eventPayload ev of
        WindowClosedEvent{} -> True
        _ -> False

type Color = (Word8, Word8, Word8)

{-# INLINE packColor #-}
packColor :: Color -> Word32
packColor (r, g, b) =
    fromIntegral r `shiftL` 16 .|.
    fromIntegral g `shiftL` 8 .|.
    fromIntegral b `shiftL` 0

rasterizePattern :: (KnownNat w, KnownNat h) => (Index w -> Index h -> Color) -> Rasterizer w h
rasterizePattern draw = Rasterizer $ \ptr stride -> do
    forM_ [minBound..maxBound] $ \y -> do
        let base = plusPtr ptr $ fromIntegral y * stride
        forM_ [minBound .. maxBound] $ \x -> do
            pokeElemOff base (fromIntegral x) (packColor $ draw x y)

newtype BufferArray (w :: Nat) (h :: Nat) = BufferArray{ getArray :: IOUArray (Int, Int) Word32 }

{-# INLINE bufferBounds #-}
bufferBounds :: SNat w -> SNat h -> ((Int, Int), (Int, Int))
bufferBounds w h = ((0, 0), (snatToNum w - 1, snatToNum h - 1))

newBufferArray :: forall w h. (KnownNat w, KnownNat h) => IO (BufferArray w h)
newBufferArray = BufferArray <$> newArray (bufferBounds (SNat @w) (SNat @h)) 0

rasterizeBuffer :: forall w h. (KnownNat w, KnownNat h) => BufferArray w h -> Rasterizer w h
rasterizeBuffer (BufferArray arr) = Rasterizer $ \ptr stride -> do
    forM_ [y0..yn] $ \y -> do
        let base = plusPtr ptr $ y * stride
        forM_ [x0..xn] $ \x -> do
            pokeElemOff base x =<< readArray arr (x, y)
  where
    ((x0, y0), (xn, yn)) = bufferBounds (SNat @w) (SNat @h)
