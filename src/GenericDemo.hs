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

-- | Encoding of Either has been defined in the standard library
-- testEither :: Either a b -> Comp (Either a b)
-- testEither = return

testMaybe :: Comp (Maybe (UInt 8, UInt 8))
testMaybe = do dividend <- inputUInt Public :: Comp (UInt 8)
               divisor  <- inputUInt Public :: Comp (UInt 8)
               if divisor == 0 then
                 return Nothing
               else
                 Just <$> performDivMod dividend divisor

-- Test dataype and generic inputs

data Person = Person (UInt 8) Boolean deriving Generic

instance Inputable Person where

testPerson :: Comp ()
testPerson = do p <- inputData Public :: Comp Person
                case p of
                  Person _ b -> assert (b `eq` (Boolean True))
