{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeApplications #-}
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

type Color = (Word8, Word8, Word8)

packColor :: Color -> Word32
packColor (r, g, b) =
    fromIntegral r `shiftL` 16 .|.
    fromIntegral g `shiftL` 8 .|.
    fromIntegral b `shiftL` 0

newtype Rasterizer (w :: Nat) (h :: Nat) = Rasterizer{ runRasterizer :: Ptr () -> Int -> IO () }

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
    return (window, renderer, texture)

    let render rasterizer = do
            (ptr, stride) <- lockTexture texture Nothing
            liftIO $ runRasterizer rasterizer ptr (fromIntegral stride)
            unlockTexture texture
            SDL.copy renderer texture Nothing Nothing
            present renderer

    let loop = do
            before <- ticks
            events <- pollEvents
            keys <- getKeyboardState
            let windowClosed = any isQuitEvent events
            draw <- if windowClosed then return Nothing else runFrame events keys
            forM_ draw $ \draw -> do
                render draw
                after <- ticks
                let elapsed = after - before
                when (elapsed < frameTime) $ liftIO $ threadDelay (fromIntegral (frameTime - elapsed) * 1000)
                loop
    loop

    destroyWindow window
  where
    frameTime = 1000 `div` screenRefreshRate
    screenSize = V2 (snatToNum (SNat @w)) (snatToNum (SNat @h))

    isQuitEvent ev = case eventPayload ev of
        WindowClosedEvent{} -> True
        KeyboardEvent KeyboardEventData{ keyboardEventKeysym = Keysym{..}, ..} ->
            keyboardEventKeyMotion == Pressed && keysymKeycode == KeycodeEscape
        _ -> False
