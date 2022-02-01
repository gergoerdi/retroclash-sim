{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections, NumericUnderscores #-}
module RetroClash.Sim.SDL
    ( VideoParams(..)
    , withMainWindow
    , module SDL.Input.Keyboard.Codes

    , Rasterizer
    , rasterizePattern

    , BufferArray(..)
    , newBufferArray
    , rasterizeBuffer

    , Color
    , packColor
    ) where

import Clash.Prelude hiding (lift)
import RetroClash.Utils

import SDL hiding (get)
import SDL.Input.Keyboard.Codes
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Array.IO
import Data.IORef
import Text.Printf

newtype Rasterizer (w :: Nat) (h :: Nat) = Rasterizer{ runRasterizer :: Ptr () -> Int -> IO () }

data VideoParams = MkVideoParams
    { windowTitle :: Text
    , screenScale :: CInt
    , screenRefreshRate :: Int
    , reportFPS :: Bool
    }

withMainWindow
    :: forall w h m. (KnownNat w, KnownNat h, MonadIO m)
    => VideoParams
    -> ([Event] -> (Scancode -> Bool) -> MaybeT m (Rasterizer w h))
    -> m ()
withMainWindow MkVideoParams{..} runFrame = do
    initializeAll
    window <- createWindow windowTitle defaultWindow
    windowSize window $= fmap (screenScale *) screenSize

    withTexture <- setupTexture window
    let render rasterizer = withTexture $ \ptr rowstride ->
            liftIO $ runRasterizer rasterizer ptr rowstride

    runMaybeT $ atFrameRate reportFPS screenRefreshRate $ do
        events <- pollEvents
        keys <- getKeyboardState
        let windowClosed = any isWindowCloseEvent events
        guard $ not windowClosed
        rasterizer <- runFrame events keys
        render rasterizer
    destroyWindow window
  where
    screenSize = V2 (snatToNum (SNat @w)) (snatToNum (SNat @h))

    setupTexture window = do
        renderer <- createRenderer window (-1) defaultRenderer
        texture <- createTexture renderer RGB888 TextureAccessStreaming screenSize

        return $ \drawToTexture -> do
            (ptr, stride) <- lockTexture texture Nothing
            drawToTexture ptr (fromIntegral stride)
            unlockTexture texture
            SDL.copy renderer texture Nothing Nothing
            present renderer

    isWindowCloseEvent ev = case eventPayload ev of
        WindowClosedEvent{} -> True
        _ -> False

atFrameRate :: (MonadIO m) => Bool -> Int -> m a -> m b
atFrameRate reportFPS frameRate act = do
    before0 <- ticks
    go before0 1
  where
    go before0 i = do
        before <- ticks
        act
        after <- ticks
        waitFrame frameRate before after
        if i < frameRate then go before0 (i + 1) else do
            when reportFPS $ do
                let elapsed = after - before0
                liftIO $ printf "%d frames at %d ms, %.2f fps\n" frameRate elapsed (fps elapsed)
            before0 <- ticks
            go before0 1

    fps elapsed = (fromIntegral frameRate * 1000) / fromIntegral elapsed :: Double

waitFrame :: (MonadIO m) => Int -> Word32 -> Word32 -> m ()
waitFrame frameRate before after = when (slack > 0) $ liftIO $ threadDelay slack
  where
    frameTime = 1_000_000 `div` frameRate
    elapsed = fromIntegral $ 1000 * (after - before)
    slack = frameTime - elapsed

type Color = (Word8, Word8, Word8)

{-# INLINE packColor #-}
packColor :: Color -> Word32
packColor (r, g, b) =
    fromIntegral r `shiftL` 16 .|.
    fromIntegral g `shiftL` 8 .|.
    fromIntegral b `shiftL` 0

rasterizePattern :: (KnownNat w, KnownNat h) => (Index w -> Index h -> Color) -> Rasterizer w h
rasterizePattern draw = Rasterizer $ \ptr rowstride -> do
    forM_ [minBound..maxBound] $ \y -> do
        let rowPtr = plusPtr ptr $ fromIntegral y * rowstride
        forM_ [minBound .. maxBound] $ \x -> do
            pokeElemOff rowPtr (fromIntegral x) (packColor $ draw x y)

newtype BufferArray (w :: Nat) (h :: Nat) = BufferArray{ getArray :: IOUArray (Index w, Index h) Word32 }

newBufferArray :: forall w h. (KnownNat w, KnownNat h) => IO (BufferArray w h)
newBufferArray = BufferArray <$> newArray ((minBound, minBound), (maxBound, maxBound)) 0

rasterizeBuffer :: forall w h. (KnownNat w, KnownNat h) => BufferArray w h -> Rasterizer w h
rasterizeBuffer (BufferArray arr) = Rasterizer $ \ptr rowstride -> do
    forM_ [minBound..maxBound] $ \y -> do
        let rowPtr = plusPtr ptr $ fromIntegral y * rowstride
        forM_ [minBound..maxBound] $ \x -> do
            pokeElemOff rowPtr (fromIntegral x) =<< readArray arr (x, y)
