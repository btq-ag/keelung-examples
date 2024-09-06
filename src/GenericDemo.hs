{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module GenericDemo where

import GHC.Generics hiding (UInt)
import Keelung

data Enc a = A a | R (Enc a) a a
  deriving (Generic)

instance Encode a => (Encode (Enc a))

testEnc :: Enc a -> Comp (Enc a)
testEnc = return

testMaybe :: Integer -> Comp (Maybe (UInt 8, UInt 8))
testMaybe divisor = do dividend <- inputUInt Public :: Comp (UInt 8)
                       if divisor == 0 then
                         return Nothing
                       else
                         Just <$> performDivMod dividend (UInt divisor)

-- Test dataype and generic inputs

data Person = Person (UInt 8) Boolean deriving Generic

instance Inputable Person where

testPerson :: Comp ()
testPerson = do p <- inputData Public :: Comp Person
                case p of
                  Person _ b -> assert (b `eq` (Boolean True))
