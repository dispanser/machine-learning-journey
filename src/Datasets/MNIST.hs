module Datasets.MNIST where

import qualified Data.Binary.Get      as B
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector.Storable as VS

readImages :: FilePath -> IO [VS.Vector Word8]
readImages f = do
    bytes <- BSL.readFile f
    return $ VS.fromList . BS.unpack <$> B.runGet parseImageFile bytes

readLabels :: FilePath -> IO [Word8]
readLabels f = do
    bytes <- BSL.readFile f
    return $ B.runGet parseLabelFile bytes

parseImageFile :: B.Get [BS.ByteString]
parseImageFile = do
    _ <- B.skip 4
    n <- fromIntegral <$> B.getWord32be
    r <- fromIntegral <$> B.getWord32be
    c <- fromIntegral <$> B.getWord32be
    replicateM n $ B.getByteString (r * c)

parseLabelFile :: B.Get [Word8]
parseLabelFile = do
    _ <- B.skip 4
    n <- fromIntegral <$> B.getWord32be
    replicateM n B.getWord8

convertImage :: VS.Vector Word8 -> VS.Vector Double
convertImage = VS.map ((/255) . fromIntegral)
