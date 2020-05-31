{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main
  ( main
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.CReal                     ( CReal )
import           Data.CReal.QuickCheck          ( )
import           Data.Coerce                    ( coerce )
import           Data.Constants.Mechanics.Extra
import           Data.Maybe
import           Data.Maybe                     ( fromJust )
import           Data.Metrology          hiding ( (%) )
import           Data.Metrology.Extra
import           Data.Proxy                     ( Proxy(..) )
import           Data.Ratio                     ( (%) )
import           Data.Tagged                    ( Tagged(..) )
import           Data.Units.SI.Parser
import           Numeric                        ( readFloat )
import           Physics.Orbit
import           Physics.Orbit.QuickCheck
import           Test.QuickCheck                ( Small(..) )
import           Test.QuickCheck.Arbitrary      ( Arbitrary )
import           Test.QuickCheck.Checkers       ( inverse
                                                , inverseL
                                                )
import           Test.QuickCheck.Extra          ( )
import           Test.Tasty                     ( TestTree
                                                , adjustOption
                                                , askOption
                                                , defaultIngredients
                                                , defaultMainWithIngredients
                                                , includingOptions
                                                , testGroup
                                                )
import           Test.Tasty.Options             ( IsOption(..)
                                                , OptionDescription(..)
                                                )
import           Test.Tasty.QuickCheck          ( (===)
                                                , (==>)
                                                , QuickCheckTests(..)
                                                , testProperty
                                                )
import           Test.Tasty.TH                  ( testGroupGenerator )
import           Text.ParserCombinators.ReadP   ( char
                                                , eof
                                                , readP_to_S
                                                , readS_to_P
                                                )
import           WrappedAngle                   ( WrappedAngle(..) )

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | The type used for tests which require exact arithmetic. They are compared
-- at a resolution of 2^32
type Exact = CReal 32

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

slowTest :: TestTree -> TestTree
slowTest t = askOption (\(SlowTestQCRatio r) ->
                          adjustOption (qcRatio r) t)
  where qcRatio r (QuickCheckTests n) =
          QuickCheckTests (floor (fromIntegral n * r))

--------------------------------------------------------------------------------
-- The tests
--------------------------------------------------------------------------------

test_sanity :: [TestTree]
test_sanity = [ testProperty "circular isValid"
                  (\(CircularOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "elliptic isValid"
                  (\(EllipticOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "parabolic isValid"
                  (\(ParabolicOrbit o) -> isValid (o :: Orbit Double))
              , testProperty "hyperbolic isValid"
                  (\(HyperbolicOrbit o) -> isValid (o :: Orbit Double))
              ]

test_classify :: [TestTree]
test_classify = [ testProperty "circular"
                    (\(CircularOrbit o) ->
                       classify (o :: Orbit Double) === Elliptic)
                , testProperty "elliptic"
                    (\(EllipticOrbit o) ->
                       classify (o :: Orbit Double) === Elliptic)
                , testProperty "parabolic"
                    (\(ParabolicOrbit o) ->
                       classify (o :: Orbit Double) === Parabolic)
                , testProperty "hyperbolic"
                    (\(HyperbolicOrbit o) ->
                       classify (o :: Orbit Double) === Hyperbolic)
                ]

test_semiMajorAxis :: [TestTree]
test_semiMajorAxis = [ testProperty "circular"
                         (\(CircularOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) === periapsis o)
                     , testProperty "elliptic"
                         (\(EllipticOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) > zero)
                     , testProperty "parabolic"
                         (\(ParabolicOrbit o) ->
                            semiMajorAxis (o :: Orbit Double) === Nothing)
                     , testProperty "hyperbolic"
                         (\(HyperbolicOrbit o) ->
                            fromJust (semiMajorAxis (o :: Orbit Double)) < zero)
                     ]

test_semiMinorAxis :: [TestTree]
test_semiMinorAxis = [ testGroup "range"
                         [ testProperty "elliptic: b > 0"
                             (\(EllipticOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) > zero)
                         , testProperty "parabolic: b = 0"
                             (\(ParabolicOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) === zero)
                         , testProperty "hyperbolic: b < 0"
                             (\(HyperbolicOrbit o) ->
                                semiMinorAxis (o :: Orbit Double) < zero)
                         ]
                     , testProperty "semiMinorAxis circular = q"
                         (\(CircularOrbit o) ->
                            semiMinorAxis (o :: Orbit Double) === periapsis o)
                     , testGroup "b^2 = a * l"
                         [ testProperty "elliptic"
                             (\(EllipticOrbit o) -> let a = fromJust (semiMajorAxis (o :: Orbit Exact))
                                                        b = semiMinorAxis o
                                                        l = semiLatusRectum o
                                                    in b |*| b === a |*| l)
                         , testProperty "hyperbolic"
                             (\(HyperbolicOrbit o) -> let a = fromJust (semiMajorAxis (o :: Orbit Exact))
                                                          b = semiMinorAxis o
                                                          l = semiLatusRectum o
                                                      in b |*| b === qNegate (a |*| l))
                         ]
                     ]

test_apoapsis :: [TestTree]
test_apoapsis = [ testProperty "ap > q"
                    (\(EllipticOrbit o) ->
                       eccentricity (o :: Orbit Double) /= 0
                       ==> fromJust (apoapsis o) > periapsis o)
                , testProperty "circular: ap = q"
                    (\(CircularOrbit o) ->
                       fromJust (apoapsis (o :: Orbit Double)) === periapsis o)
                , testProperty "parabolic: no ap"
                    (\(ParabolicOrbit o) ->
                       apoapsis (o :: Orbit Double) === Nothing)
                , testProperty "hyperbolic: no ap"
                    (\(HyperbolicOrbit o) ->
                       apoapsis (o :: Orbit Double) === Nothing)
                ]

test_meanMotion :: [TestTree]
test_meanMotion = [ testProperty "n > 0"
                      (\o -> meanMotion (o :: Orbit Double) > zero)
                  ]

test_period :: [TestTree]
test_period = [ testProperty "p > 0"
                  (\(EllipticOrbit o) ->
                     fromJust (period (o :: Orbit Double)) > zero)
                , testProperty "4 π a^3 / p^2 = μ"
                    (\(EllipticOrbit o) ->
                      let Just p = period (o :: Orbit Exact)
                          Just a = semiMajorAxis o
                          μ = primaryGravitationalParameter o
                      in (4 * qSq pi) |*| qCube a |/| qSq p === μ)
                , testProperty "parabolic: no p"
                    (\(ParabolicOrbit o) ->
                       period (o :: Orbit Double) === Nothing)
                , testProperty "hyperbolic: no p"
                    (\(HyperbolicOrbit o) ->
                       period (o :: Orbit Double) === Nothing)
              ]

-- TODO: Put converge test here
test_hyperbolicAngles :: [TestTree]
test_hyperbolicAngles = [ testProperty "parabolic approach"
                            (\(ParabolicOrbit o) ->
                               fromJust
                                 (hyperbolicApproachAngle (o :: Orbit Double))
                                 === qNegate halfTurn)
                        , testProperty "parabolic departure"
                            (\(ParabolicOrbit o) ->
                               fromJust
                                 (hyperbolicDepartureAngle (o :: Orbit Double))
                                 === halfTurn)
                        , testProperty "hyperbolic symmetry"
                            (\(HyperbolicOrbit o) ->
                               fromJust (hyperbolicDepartureAngle (o :: Orbit Double))
                               === qNegate (fromJust (hyperbolicApproachAngle o)))
                        , testProperty "elliptic: no approach"
                            (\(EllipticOrbit o) ->
                               hyperbolicApproachAngle (o :: Orbit Double) === Nothing)
                        , testProperty "elliptic: no departure"
                            (\(EllipticOrbit o) ->
                               hyperbolicDepartureAngle (o :: Orbit Double) === Nothing)
                        ]

anomalyConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                                  => Orbit a -> Angle a -> Angle a)
                       -> String -> String -> [TestTree]
anomalyConversionTests convertAnomaly fromName toName =
  [ testProperty (toName ++ " when " ++ fromName ++ " = 0")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) zero
       in to === zero)

  , testProperty (toName ++ " when " ++ fromName ++ " = π")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) halfTurn
       in to === halfTurn)

  , testProperty (toName ++ " when " ++ fromName ++ " = 2π")
     (\(EllipticOrbit o) ->
       let to = convertAnomaly (o :: Orbit Double) turn
       in to === turn)

  , testProperty "identity on circular orbits"
     (\(CircularOrbit o) from ->
       let to = convertAnomaly (o :: Orbit Exact) from
       in from === to)

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) from ->
       let to = convertAnomaly (o :: Orbit Double) from
       in from `div'` turn === (to `div'` turn :: Unitless Integer))
  ]

timeAnomalyConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                                      => Orbit a -> Time a -> Angle a)
                           -> String -> [TestTree]
timeAnomalyConversionTests timeToAnomaly toName =
  [ testProperty (toName ++ " when time = 0")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Double) zero
       in to === zero)

  , testProperty (toName ++ " when time = p/2")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Exact) (p|/|2)
           Just p = period o
       in to === halfTurn)

  , testProperty (toName ++ " when time = p")
     (\(EllipticOrbit o) ->
       let to = timeToAnomaly (o :: Orbit Exact) p
           Just p = period o
       in to === turn)

  , testProperty "identity on the unit orbit (modulo units!)"
     (\time ->
       let o = unitOrbit
           to = timeToAnomaly (o :: Orbit Exact) time
       in time # [si|s|] === to # [si|rad|])

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) time ->
       let to = timeToAnomaly (o :: Orbit Double) time
           Just p = period o
       in time `div'` p === (to `div'` turn :: Unitless Integer))
  ]

anomalyTimeConversionTests :: (forall a. (RealFloat a, Show a, Arbitrary a, Converge [a])
                                      => Orbit a -> Angle a -> Time a)
                           -> String -> [TestTree]
anomalyTimeConversionTests anomalyToTime fromName =
  [ testProperty ("time when " ++ fromName ++ " = 0")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) zero
       in t === zero)

  , testProperty ("time when " ++ fromName ++ " = π")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) halfTurn
           Just p = period o
       in t === p |/| 2)

  , testProperty ("time when " ++ fromName ++ " = 2π")
     (\(EllipticOrbit o) ->
       let t = anomalyToTime (o :: Orbit Double) turn
           Just p = period o
       in t === p)

  , testProperty "identity on the unit orbit (modulo units!)"
     (\from ->
       let o = unitOrbit
           t = anomalyToTime (o :: Orbit Exact) from
       in from # [si|rad|] === t # [si|s|])

  , testProperty "orbit number preservation"
     (\(EllipticOrbit o) from ->
       let t = anomalyToTime (o :: Orbit Double) from
           Just p = period o
       in from `div'` turn === (t `div'` p :: Unitless Integer))
  ]

(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f .: g = \x y -> f (g x y)

(~>) :: Bool -> Bool -> Bool
a ~> b = not a || b

test_conversions :: [TestTree]
test_conversions = [ conversionToTime
                   , conversionToMeanAnomaly
                   , conversionToEccentricAnomaly
                   , conversionToTrueAnomaly
                   , conversionInverses
                   ]
  where
    conversionToTime = testGroup "conversion to time"
      [ testGroup "from mean anomaly"
                  (anomalyTimeConversionTests timeAtMeanAnomaly "mean anomaly")
      , testGroup "from eccentric anomaly"
                  (anomalyTimeConversionTests (fromJust .: timeAtEccentricAnomaly)
                                              "eccentric anomaly")
      , testGroup "from true anomaly"
                  (anomalyTimeConversionTests (fromJust .: timeAtTrueAnomaly)
                                              "true anomaly")
      , testProperty "from true anomaly out of bounds parabolic"
        (\ν (ParabolicOrbitF o) ->
          validTrueAnomaly o ν ~> isJust (timeAtTrueAnomaly o ν))
      ]

    conversionToMeanAnomaly = let s = "mean anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests meanAnomalyAtTime s)
      , testGroup "from eccentric anomaly"
                  (anomalyConversionTests (fromJust .: meanAnomalyAtEccentricAnomaly)
                                          "eccentric anomaly"
                                          s)
      , testGroup "from true anomaly"
                  (anomalyConversionTests (fromJust .: meanAnomalyAtTrueAnomaly)
                                          "true anomaly"
                                          s)
      ]

    conversionToEccentricAnomaly = let s = "eccentric anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests (fromJust .: eccentricAnomalyAtTime) s)
      , testGroup "from mean anomaly"
                  (anomalyConversionTests (fromJust .: eccentricAnomalyAtMeanAnomaly)
                                          "mean anomaly"
                                          s)
      , testGroup "from true anomaly"
                  (anomalyConversionTests (fromJust .: eccentricAnomalyAtTrueAnomaly)
                                          "true anomaly"
                                          s)
      ]

    conversionToTrueAnomaly = let s = "true anomaly" in testGroup ("conversion to " ++ s)
      [ testGroup "from time"
                  (timeAnomalyConversionTests trueAnomalyAtTime s)
      , testGroup "from mean anomaly"
                  (anomalyConversionTests trueAnomalyAtMeanAnomaly
                                          "mean anomaly"
                                          s)
      , testGroup "from eccentric anomaly"
                  (anomalyConversionTests (fromJust .: trueAnomalyAtEccentricAnomaly)
                                          "eccentric anomaly"
                                          s)
      ]

    conversionInverses = testGroup "conversionInverses"
      [ testProperty "mean time inverse"
          (\o -> inverse (meanAnomalyAtTime (o :: Orbit Exact))
                         (timeAtMeanAnomaly o))

      , slowTest $ testProperty "mean eccentric inverse"
          (\(EllipticOrbit o) ->
            inverse (coerce (fromJust . meanAnomalyAtEccentricAnomaly (o :: Orbit Exact)) :: WrappedAngle Exact -> WrappedAngle Exact)
                    (coerce (fromJust . eccentricAnomalyAtMeanAnomaly o)))

      , slowTest $ testProperty "mean hyperbolic inverse"
          (\(HyperbolicOrbit o) ->
            inverseL (fromJust . meanAnomalyAtHyperbolicAnomaly @Exact o)
                     (fromJust . hyperbolicAnomalyAtMeanAnomaly o))

      , slowTest $ testProperty "mean true inverse"
          (\(EllipticOrbit o) ->
            inverse (fromJust . meanAnomalyAtTrueAnomaly (o :: Orbit Exact))
                    (trueAnomalyAtMeanAnomaly o))

      , slowTest $ testProperty "time true inverse elliptic"
          (\(EllipticOrbit o) ->
            inverse (fromJust . timeAtTrueAnomaly (o :: Orbit Exact))
                    (trueAnomalyAtTime o))

      , slowTest $ testProperty "true time inverse parabolic"
          (\(ParabolicOrbit o) ->
            -- Use inverseL because there doesn't exist a time for every true
            -- anomaly
            inverseL (fromJust . timeAtTrueAnomaly (o :: Orbit Exact))
                     (trueAnomalyAtTime o)
                    )

      , testProperty "time eccentric inverse"
          (\(EllipticOrbit o) ->
            inverse (fromJust . timeAtEccentricAnomaly (o :: Orbit Exact))
                    (fromJust . eccentricAnomalyAtTime o))

      -- , slowTest $ testProperty "time hyperbolic inverse"
      --     (\(HyperbolicOrbit o) ->
      --       inverseL (fromJust . timeAtHyperbolicAnomaly @Exact o)
      --                (fromJust . hyperbolicAnomalyAtTime o))

      , testProperty "eccentric true inverse"
          (\(EllipticOrbit o) ->
            inverse (coerce (fromJust . eccentricAnomalyAtTrueAnomaly (o:: Orbit Exact)) :: WrappedAngle Exact -> WrappedAngle Exact)
                    (fromJust . coerce (trueAnomalyAtEccentricAnomaly o)))

      , testProperty "hyperbolic true inverse"
          (\(HyperbolicOrbit o) ->
            inverseL (fromJust . hyperbolicAnomalyAtTrueAnomaly o)
                     (fromJust . trueAnomalyAtHyperbolicAnomaly @Exact o))
      ]

test_anomalies :: [TestTree]
test_anomalies =
  [ slowTest $ testProperty
      "hyperbolic true"
      (\(HyperbolicOrbit o) _M ->
        let Just _H = hyperbolicAnomalyAtMeanAnomaly @Exact o _M
            ν       = trueAnomalyAtMeanAnomaly o _M
            e       = eccentricity o
        in  qCosh _H === (qCos ν + e) / (1 + e * qCos ν)
      )
  ]

-- TODO: Put parabolic and hyperbolic tests here
test_areal :: [TestTree]
test_areal = [ testProperty "elliptic areal area"
                 (\(EllipticOrbit o) -> let Just a = semiMajorAxis (o :: Orbit Exact)
                                            b = semiMinorAxis o
                                            area = pi |*| a |*| b
                                            Just p = period o
                                        in area === p |*| arealVelocity o)
             ]

test_orbitalEnergy :: [TestTree]
test_orbitalEnergy =
  [ testProperty "negative elliptical energy"
                 (\(EllipticOrbitF o) -> specificOrbitalEnergy o < zero)
  , testProperty "zero parabolic energy"
                 (\(ParabolicOrbitF o) -> specificOrbitalEnergy o === zero)
  , testProperty "positive hyperbolic energy"
                 (\(HyperbolicOrbitF o) -> specificOrbitalEnergy o > zero)
  , testGroup
    "potential + kinetic"
    (overAllClasses
      (\o ν ->
        specificOrbitalEnergy @Exact o
          === specificPotentialEnergyAtTrueAnomaly o ν
          |+| specificKineticEnergyAtTrueAnomaly o ν
      )
    )
  ]

test_radius :: [TestTree]
test_radius =
  [ testGroup
    "periapsis when ν = 0"
    (overAllClasses (\o -> radiusAtTrueAnomaly @Exact o zero === periapsis o))
  , testProperty
    "constant on circular"
    (\(CircularOrbitF o) ν -> radiusAtTrueAnomaly o ν === periapsis o)
  , testProperty
    "apoapsis when ν == π for elliptic"
    (\(EllipticOrbit o) ->
      radiusAtTrueAnomaly @Exact o halfTurn === fromJust (apoapsis o)
    )
  , testGroup
    "l when ν == π/2"
    (overAllClasses
      (\o -> radiusAtTrueAnomaly @Exact o (halfTurn |*| (-0.5))
        === semiLatusRectum o
      )
    )
  , testGroup
    "l when ν == -π/2"
    (overAllClasses
      (\o -> radiusAtTrueAnomaly @Exact o (halfTurn |*| (-0.5))
        === semiLatusRectum o
      )
    )
  , testProperty
    "from E"
    (\(EllipticOrbit o) ν ->
      let Just _E = eccentricAnomalyAtTrueAnomaly @Exact o ν
      in  radiusAtTrueAnomaly o ν
            === fromJust (semiMajorAxis o)
            |*| (1 - eccentricity o |*| qCos _E)
    )
  ]

test_speed :: [TestTree]
test_speed =
  [ testProperty
    "constant on circular"
    (\(CircularOrbitF o) ν ν' ->
      speedAtTrueAnomaly o ν === speedAtTrueAnomaly o ν'
    )
  , testProperty
    "zero at apex"
    (\(ParabolicOrbitF o) -> speedAtTrueAnomaly o halfTurn === zero)
  , testProperty
    "below escape velocity for elliptical"
    (\(EllipticOrbitF o) ν -> speedAtTrueAnomaly o ν < escapeVelocityAtDistance
      (primaryGravitationalParameter o)
      (radiusAtTrueAnomaly o ν)
    )
  , testProperty
    "escape velocity for parabolic"
    (\(ParabolicOrbitF o) ν ->
      speedAtTrueAnomaly o ν === escapeVelocityAtDistance
        (primaryGravitationalParameter o)
        (radiusAtTrueAnomaly o ν)
    )
  , testProperty
    "above escape velocity for hyperbolic"
    (\(HyperbolicOrbitF o) _M ->
      let ν = trueAnomalyAtMeanAnomaly o _M
      in  speedAtTrueAnomaly o ν > escapeVelocityAtDistance
            (primaryGravitationalParameter o)
            (radiusAtTrueAnomaly o ν)
    )
  ]

test_angularMomentum :: [TestTree]
test_angularMomentum =
  [ testProperty "negative elliptical energy"
                 (\(EllipticOrbitF o) -> specificOrbitalEnergy o < zero)
  , testProperty "zero parabolic energy"
                 (\(ParabolicOrbitF o) -> specificOrbitalEnergy o === zero)
  , testProperty "positive hyperbolic energy"
                 (\(HyperbolicOrbitF o) -> specificOrbitalEnergy o > zero)
  ]

validTrueAnomaly :: (Floating a, Ord a) => Orbit a -> Angle a -> Bool
validTrueAnomaly o ν = case hyperbolicDepartureAngle o of
  Nothing -> True
  Just d  -> qAbs ν < d
  where qAbs x = if x < zero then qNegate x else x

main :: IO ()
main = do
  let is = includingOptions [slowTestQCRatio] : defaultIngredients
  defaultMainWithIngredients is $(testGroupGenerator)

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

qCos :: Floating a => Angle a -> Unitless a
qCos θ = quantity $ cos (θ # [si|rad|])

qSin :: Floating a => Angle a -> Unitless a
qSin θ = quantity $ sin (θ # [si|rad|])

qCosh :: Floating a => AngleH a -> Unitless a
qCosh = quantity . cosh . (# RadianHyperbolic)
