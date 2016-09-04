module Test.SlowTest
  ( slowTest
  , slowTestQCRatio
  ) where

import Control.Applicative          ((<|>))
import Test.Tasty.QuickCheck (QuickCheckTests (..))
import Test.Tasty (TestTree, adjustOption, askOption)
import Data.Proxy                   (Proxy (..))
import Data.Ratio                   ((%))
import Data.Tagged                  (Tagged (..))
import Numeric                      (readFloat)
import Test.Tasty.Options           (IsOption (..), OptionDescription (..))
import Text.ParserCombinators.ReadP (char, eof, readP_to_S, readS_to_P)

--------------------------------------------------------------------------------
-- Disable some really slow tests by default
--------------------------------------------------------------------------------

newtype SlowTestQCRatio = SlowTestQCRatio Rational

slowTestQCRatio :: OptionDescription
slowTestQCRatio = Option (Proxy :: Proxy SlowTestQCRatio)

readRational :: String -> Maybe Rational
readRational s = case readP_to_S readRationalP s of
                   [(r,"")] -> Just r
                   _ -> Nothing
  where readRationalP = readS_to_P readFloat <* eof
                    <|> do n <- readS_to_P reads
                           _ <- char '/'
                           d <- readS_to_P reads
                           eof
                           pure (n%d)

instance IsOption SlowTestQCRatio where
  defaultValue = SlowTestQCRatio (1%10)
  parseValue = fmap SlowTestQCRatio . readRational
  optionName = Tagged "slow-test-ratio"
  optionHelp = Tagged $
    unwords [ "Some of the slow tests can take a long time to run; set this"
            , "flag to change the number of slow test QuickCheck test cases as"
            , "a proportion of the non-slow test number."
            ]

-- | Mark a tree as slow
slowTest :: TestTree -> TestTree
slowTest t = askOption (\(SlowTestQCRatio r) ->
                          adjustOption (qcRatio r) t)
  where qcRatio r (QuickCheckTests n) =
          QuickCheckTests (floor (fromIntegral n * r))

