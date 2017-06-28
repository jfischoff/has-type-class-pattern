{-# LANGUAGE RecordWildCards #-}
module TraverseImages where
import Types
import Control.Lens
import Data.Text

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