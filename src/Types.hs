module Types where
import Data.Text (Text)

data Scene = Scene
  { backgroundImage   :: Text
  , characters        :: [Character]
  , bewilderedTourist :: Maybe Character
  , objects           :: [Either Rock WoodenCrate]
  }
data Character = Character
 { hat   :: Maybe DamageArray
 , head  :: DamageArray
 , torso :: DamageArray
 , legs  :: DamageArray
 , shoes :: Maybe DamageArray
 }
data DamageArray = DamageArray
  { noDamage        :: Text
  , someDamage      :: Text
  , excessiveDamage :: Text
  }
data Rock = Rock
  { weight    :: Double
  , rockImage :: Text
  }
data WoodenCrate = WoodenCrate
  { strength         :: Double
  , woodenCrateImage :: DamageArray
  }