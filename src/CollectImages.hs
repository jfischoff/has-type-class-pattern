{-# LANGUAGE RecordWildCards #-}
module CollectImages where
import Types
import Data.Set (Set, fromList, singleton)
import Data.Text (Text)
import Data.Monoid

collectImages :: Scene -> Set Text
collectImages Scene {..}
  =  singleton backgroundImage
  <> mconcat (map collectCharacterImages characters)
  <> maybe mempty collectCharacterImages bewilderedTourist
  <> mconcat (map (either (singleton . collectRockImage)
                          collectWoodenCrateImages)
                  objects)
collectCharacterImages :: Character -> Set Text
collectCharacterImages Character {..}
  =  maybe mempty collectDamageArrayImages hat
  <> collectDamageArrayImages head
  <> collectDamageArrayImages torso
  <> collectDamageArrayImages legs
  <> maybe mempty collectDamageArrayImages shoes

collectDamageArrayImages :: DamageArray -> Set Text
collectDamageArrayImages DamageArray {..} = fromList
  [ noDamage
  , someDamage
  , excessiveDamage
  ]

collectRockImage :: Rock -> Text
collectRockImage Rock {..} = rockImage

collectWoodenCrateImages :: WoodenCrate -> Set Text
collectWoodenCrateImages WoodenCrate {..} =
  collectDamageArrayImages woodenCrateImage


class HasImages a where
  images :: a -> Set Text

instance HasImages a => HasImages [a] where
  images xs = foldr (\x accum -> images x <> accum) mempty xs

instance HasImages a => HasImages (Maybe a) where
  images x = maybe mempty images x

instance (HasImages a, HasImages b) => HasImages (Either a b) where
  images x = either images images x

instance HasImages Scene where
  images Scene {..}
    =  singleton backgroundImage
    <> images characters
    <> images bewilderedTourist
    <> images objects
instance HasImages Character where
  images Character {..}
    =  images hat
    <> images head
    <> images torso
    <> images legs
    <> images shoes
instance HasImages DamageArray where
  images DamageArray {..} = fromList
    [ noDamage
    , someDamage
    , excessiveDamage
    ]
instance HasImages Rock where
  images Rock {..} = singleton rockImage
instance HasImages WoodenCrate where
  images WoodenCrate {..} = images woodenCrateImage