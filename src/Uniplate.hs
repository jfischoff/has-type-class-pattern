{-# LANGUAGE DeriveDataTypeable #-}
module Uniplate where
import Data.Set
import Data.Generics.Uniplate.Data
import System.FilePath
import Crypto.Hash
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T

data Scene = Scene
  { backgroundImage   :: Text
  , characters        :: [Character]
  , bewilderedTourist :: Maybe Character
  , objects           :: [Either Rock WoodenCrate]
  } deriving (Show, Data)
data Character = Character
 { hat   :: Maybe DamageArray
 , head  :: DamageArray
 , torso :: DamageArray
 , legs  :: DamageArray
 , shoes :: Maybe DamageArray
 } deriving (Show, Data)
data DamageArray = DamageArray
  { noDamage        :: Text
  , someDamage      :: Text
  , excessiveDamage :: Text
  } deriving (Show, Data)
data Rock = Rock
  { weight    :: Double
  , rockImage :: Text
  } deriving (Show, Data)
data WoodenCrate = WoodenCrate
  { strength         :: Double
  , woodenCrateImage :: DamageArray
  } deriving (Show, Data)

collectImages :: Scene -> Set Text
collectImages x = fromList (universeBi x)

hashSceneImages :: Scene -> IO Scene
hashSceneImages x = transformBiM hashFilePath x

sha1 :: ByteString -> Digest SHA1
sha1 = hash

hashBytes :: BSL.ByteString -> String
hashBytes bs = show (hash (BSL.toStrict bs) :: Digest SHA3_512)

hashFilePath :: Text -> IO Text
hashFilePath filePath = do
  let pathStr = T.unpack filePath
  fileHash <- hashBytes <$> BSL.readFile pathStr
  return $ T.pack $ dropExtension pathStr
         ++ "-" ++ fileHash <.> takeExtension pathStr