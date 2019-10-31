{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Haze.Util where

import           UIO

import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Vector.Storable           as VS

import qualified Data.ByteString.Unsafe        as B'
import qualified Data.Vector.Storable.Mutable  as VSM'

import qualified Network.WebSockets            as WS
import qualified Data.Aeson                    as A
import           Foreign


wsSendText :: A.ToJSON a => WS.Connection -> a -> IO ()
wsSendText wsc jsonCmd =
    -- 'A.encode' may crash the process if lazily called here
    WS.sendDataMessage wsc $ flip WS.Text Nothing $! (A.encode jsonCmd)

wsSendData :: WS.Connection -> VS.MVector (PrimState IO) Double -> IO ()
wsSendData wsc arry = do
    let (fptr, len) = VSM'.unsafeToForeignPtr0 arry
    -- we use unsafe coercion to ByteString here for zero-copy performance,
    -- it is still safe as the memory is only used during 'withForeignPtr'
    withForeignPtr fptr $ \ptr -> do
        bs <- B'.unsafePackCStringLen
            (castPtr ptr, len * sizeOf (undefined :: Double))
        WS.sendDataMessage wsc $ WS.Binary $ BL.fromStrict bs

