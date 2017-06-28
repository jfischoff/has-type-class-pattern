{-# LANGUAGE RecordWildCards #-}
module TraverseImages where
import Types
import Control.Lens hiding ((<.>))
import System.FilePath
import Crypto.Hash
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import Data.Set

class HasImages a where
  images :: Traversal' a Text

instance HasImages a => HasImages [a] where
  images = traversed . images

instance HasImages a => HasImages (Maybe a) where
  images = traversed . images

instance (HasImages a, HasImages b) => HasImages (Either a b) where
  images f e = case e of
    Left  x -> Left  <$> traverseOf images f x
    Right x -> Right <$> traverseOf images f x

instance HasImages Scene where
  images f Scene {..}
    =  Scene
   <$> f backgroundImage
   <*> traverseOf images f characters
   <*> traverseOf images f bewilderedTourist
   <*> traverseOf images f objects

instance HasImages Character where
  images f Character {..}
     =  Character
    <$> traverseOf images f hat
    <*> traverseOf images f head
    <*> traverseOf images f torso
    <*> traverseOf images f legs
    <*> traverseOf images f shoes

instance HasImages DamageArray where
  images f DamageArray {..}
    =  DamageArray
   <$> f noDamage
   <*> f someDamage
   <*> f excessiveDamage

instance HasImages Rock where
  images f Rock {..}
    =  Rock weight
   <$> f rockImage

instance HasImages WoodenCrate where
  images f WoodenCrate {..}
    = WoodenCrate strength
   <$> traverseOf images f woodenCrateImage

collectImages :: Scene -> Set Text
collectImages x = fromList (toListOf images x)

hashSceneImages :: Scene -> IO Scene
hashSceneImages x = traverseOf images hashFilePath x

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