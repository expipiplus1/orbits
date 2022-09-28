module Physics.Orbit.Sol
  where

import           Data.Constants.Mechanics
import           Data.Metrology
import           Data.Units.Astronomical
import           Data.Units.SI.Parser
import           Physics.Orbit

import           Data.Tree (Forest, Tree(..))

type System a = Forest (String, Orbit a)

solarSystem :: Fractional a => System a
solarSystem =
  [ Node ("Mercury", mercuryOrbit) []
  , Node ("Venus",   venusOrbit)   []
  , Node ("Earth",   earthOrbit)   earthSystem
  , Node ("Mars",    marsOrbit)    marsSystem
  , Node ("Jupiter", jupiterOrbit) jupiterSystem
  , Node ("Saturn",  saturnOrbit)  [] -- saturnSystem
  , Node ("Uranus",  uranusOrbit)  [] -- uranusSystem
  , Node ("Neptune", neptuneOrbit) [] -- neptuneSystem
  ]

-- * Sol

solMass :: Fractional a => Mass a
solMass = 1988500e24 % [si|kg|]

solGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
solGraviationalParameter = solMass |*| gravity_G

-- * Planets

-- ** Mercury ☿

mercuryOrbit :: Fractional a => Orbit a
mercuryOrbit = Orbit
  { eccentricity                  = 0.205630
  , periapsis                     = 0.307499 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 48.331 % [si|deg|]
                                      , inclination = 6.35 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 29.124 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

mercuryMass :: Fractional a => Mass a
mercuryMass = 33011e19 % [si|kg|]

mercuryGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
mercuryGraviationalParameter = mercuryMass |*| gravity_G

-- ** Venus ♀

venusOrbit :: Fractional a => Orbit a
venusOrbit = Orbit
  { eccentricity                  = 0.006772
  , periapsis                     = 0.718440 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 76.680 % [si|deg|]
                                      , inclination = 2.19 % [si|deg|] -- XXX: 2.15 ?
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 54.884 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

venusMass :: Fractional a => Mass a
venusMass = 48675e20 % [si|kg|]

venusGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
venusGraviationalParameter = venusMass |*| gravity_G

-- ** Earth 🜨

earthOrbit :: Fractional a => Orbit a
earthOrbit = Orbit
  { eccentricity                  = 0.01671123
  , periapsis                     = 0.9832899 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 348.73936 % [si|deg|]
                                      , inclination = 1.578690 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 114.20783 % [si|deg|]
                                   }
  , primaryGravitationalParameter = solGraviationalParameter
  }

earthMass :: Fractional a => Mass a
earthMass = 597237e19 % [si|kg|]

earthGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
earthGraviationalParameter = earthMass |*| gravity_G

earthSystem :: Fractional a => System a
earthSystem =
  [ Node ("Moon", moonOrbit) []
  ]

-- *** Moon

-- | XXX: longitudeOfAscendingNode and argumentOfPeriapsis have to be corrected to date.
moonOrbit :: Fractional a => Orbit a
moonOrbit = Orbit
  { eccentricity                  = 0.0549
  , periapsis                     = 362600 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 0 % [si|deg|] -- XXX: Regressing by one revolution in 18.61 years
                                      , inclination = 5.14 % [si|deg|] -- XXX: to ecliptic, i.e. Earth's orbital plane
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 0 % [si|deg|] -- XXX: progressing by one revolution in 8.85 years
                                   }
  , primaryGravitationalParameter = earthGraviationalParameter
  }

moonLongitudeOfAscendingNodePeriod :: Fractional a => Time a
moonLongitudeOfAscendingNodePeriod = -18.61 * 3.15576e7 % [si|s|]

moonArgumentOfPeriapsisPeriod :: Fractional a => Time a
moonArgumentOfPeriapsisPeriod = 8.85 * 3.15576e7 % [si|s|]

-- ** Mars ♂
--
-- XXX: Elements from https://iopscience.iop.org/article/10.1088/0004-6256/139/2/668
-- Inclination elements at 1950-01 TT, use rates to adjust.

marsOrbit :: Fractional a => Orbit a
marsOrbit = Orbit
  { eccentricity                  = 0.0934
  , periapsis                     = 1.382 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 49.558 % [si|deg|]
                                      , inclination = 1.67 % [si|deg|] -- XXX: 1.63?
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 286.502 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

marsMass :: Fractional a => Mass a
marsMass = 64171e19 % [si|kg|]

marsGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
marsGraviationalParameter = marsMass |*| gravity_G

marsSystem :: Fractional a => System a
marsSystem =
  [ Node ("Phobos", phobosOrbit) []
  , Node ("Deimos", deimosOrbit) []
  ]

-- *** Phobos

phobosOrbit :: Fractional a => Orbit a
phobosOrbit = Orbit
  { eccentricity                  = 0.01511
  , periapsis                     = 9234.42 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = loan_Ω
                                      , inclination = 1.0756 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = aop_ω }
  , primaryGravitationalParameter = marsGraviationalParameter
  }
  where
    loan_Ω = 207.7875 % [si|deg|]
    lop_ϖ = 357.4308 % [si|deg|]
    aop_ω = lop_ϖ |-| loan_Ω

phobosLoaNRate :: Fractional a => Quantity [si|deg/s|] a
phobosLoaNRate = -0.000005044 % [si|deg/s|]

phobosLoPRate :: Fractional a => Quantity [si|deg/s|] a
phobosLoPRate = 0.000005037 % [si|deg/s|]

phobosMass :: Fractional a => Mass a
phobosMass = 10659e12 % [si|kg|]

phobosGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
phobosGraviationalParameter = phobosMass |*| gravity_G

-- *** Deimos

deimosOrbit :: Fractional a => Orbit a
deimosOrbit = Orbit
  { eccentricity                  = 0.01511
  , periapsis                     = 23455.5 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = loan_Ω
                                      , inclination = 1.7878 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = aop_ω }
  , primaryGravitationalParameter = marsGraviationalParameter
  }
  where
    loan_Ω = 24.5123 % [si|deg|]
    lop_ϖ = 290.7208 % [si|deg|]
    aop_ω = lop_ϖ |-| loan_Ω

deimosLoaNRate :: Fractional a => Quantity [si|deg/s|] a
deimosLoaNRate = -0.000000209 % [si|deg/s|]

deimosLoPRate :: Fractional a => Quantity [si|deg/s|] a
deimosLoPRate = 0.000000207 % [si|deg/s|]

deimosMass :: Fractional a => Mass a
deimosMass = 14762e11 % [si|kg|]

deimosGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
deimosGraviationalParameter = deimosMass |*| gravity_G

-- ** Jupiter ♃

jupiterOrbit :: Fractional a => Orbit a
jupiterOrbit = Orbit
  { eccentricity                  = 0.0489
  , periapsis                     = 4.9501 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 100.464 % [si|deg|]
                                      , inclination = 0.32 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 273.867 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

jupiterMass :: Fractional a => Mass a
jupiterMass = 18982e23 % [si|kg|]

jupiterGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
jupiterGraviationalParameter = jupiterMass |*| gravity_G

jupiterSystem :: Fractional a => System a
jupiterSystem = mconcat
  [ jupiterMoonsInner
  , jupiterMoonsGalilean
    -- TODO: lots and lots of irregular satellites
  ]

-- *** Inner moons
--
-- XXX: Mass assuming that its mean density is like that of Amalthea (around 0.86 g/cm3).

jupiterMoonsInner :: Fractional a => System a
jupiterMoonsInner =
  [ Node ("Metis", metisOrbit) []
  , Node ("Adrastea", adrasteaOrbit) []
  , Node ("Amalthea", amaltheaOrbit) []
  , Node ("Thebe", thebeOrbit) []
  ]

metisOrbit :: Fractional a => Orbit a
metisOrbit = Orbit
  { eccentricity                  = 0.0
  , periapsis                     = 128000 % [si|km|]
  , inclinationSpecifier          = NonInclined
  , periapsisSpecifier            = Circular
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

metisMass :: Fractional a => Mass a
metisMass = 3.6e16 % [si|kg|]

metisGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
metisGraviationalParameter = metisMass |*| gravity_G

adrasteaOrbit :: Fractional a => Orbit a
adrasteaOrbit = Orbit
  { eccentricity                  = 0.0
  , periapsis                     = 129000 % [si|km|]
  , inclinationSpecifier          = NonInclined
  , periapsisSpecifier            = Circular
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

adrasteaMass :: Fractional a => Mass a
adrasteaMass = 0.2e16 % [si|kg|]

adrasteaGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
adrasteaGraviationalParameter = adrasteaMass |*| gravity_G

amaltheaOrbit :: Fractional a => Orbit a
amaltheaOrbit = Orbit
  { eccentricity                  = 0.003
  , periapsis                     = aePeriapsis 181400 0.003
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 282.9 % [si|deg|]
                                      , inclination = 0.4 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 180.1 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

amaltheaMass :: Fractional a => Mass a
amaltheaMass = 208e16 % [si|kg|]

amaltheaGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
amaltheaGraviationalParameter = amaltheaMass |*| gravity_G

thebeOrbit :: Fractional a => Orbit a
thebeOrbit = Orbit
  { eccentricity                  = 0.018
  , periapsis                     = aePeriapsis 221900 0.018
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 340.4 % [si|deg|]
                                      , inclination = 1.1 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 26.6 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

thebeMass :: Fractional a => Mass a
thebeMass = 43e16 % [si|kg|]

thebeGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
thebeGraviationalParameter = thebeMass |*| gravity_G

-- *** Galilean moons

jupiterMoonsGalilean :: Fractional a => System a
jupiterMoonsGalilean =
  [ Node ("Io", ioOrbit) []
  , Node ("Europa", europaOrbit) []
  , Node ("Ganymede", ganymedeOrbit) []
  , Node ("Callisto", callistoOrbit) []
  ]

ioOrbit :: Fractional a => Orbit a
ioOrbit = Orbit
  { eccentricity                  = 0.0041
  , periapsis                     = 420000 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 0 % [si|deg|]
                                      , inclination = 2.213 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 0 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

ioMass :: Fractional a => Mass a
ioMass = 8931900e16 % [si|kg|]

ioGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
ioGraviationalParameter = ioMass |*| gravity_G

europaOrbit :: Fractional a => Orbit a
europaOrbit = Orbit
  { eccentricity                  = 0.009
  , periapsis                     = 664862 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 0 % [si|deg|]
                                      , inclination = 1.791 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 0 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

europaMass :: Fractional a => Mass a
europaMass = 4799800e16 % [si|kg|]

europaGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
europaGraviationalParameter = europaMass |*| gravity_G

ganymedeOrbit :: Fractional a => Orbit a
ganymedeOrbit = Orbit
  { eccentricity                  = 0.0013
  , periapsis                     = 1069200 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 0 % [si|deg|]
                                      , inclination = 2.214 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 0 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

ganymedeMass :: Fractional a => Mass a
ganymedeMass = 14819000e16 % [si|kg|]

ganymedeGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
ganymedeGraviationalParameter = ganymedeMass |*| gravity_G

callistoOrbit :: Fractional a => Orbit a
callistoOrbit = Orbit
  { eccentricity                  = 0.0074
  , periapsis                     = 1869000 % [si|km|]
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 0 % [si|deg|]
                                      , inclination = 0.192 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 0 % [si|deg|] }
  , primaryGravitationalParameter = jupiterGraviationalParameter
  }

callistoMass :: Fractional a => Mass a
callistoMass = 10759000e16 % [si|kg|]

callistoGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
callistoGraviationalParameter = callistoMass |*| gravity_G

-- ** Saturn ♄

saturnOrbit :: Fractional a => Orbit a
saturnOrbit = Orbit
  { eccentricity                  = 0.0565
  , periapsis                     = 9.0412 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 113.665 % [si|deg|]
                                      , inclination = 0.93 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 339.392 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

saturnMass :: Fractional a => Mass a
saturnMass = 56834e22 % [si|kg|]

saturnGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
saturnGraviationalParameter = saturnMass |*| gravity_G

-- ** Uranus ♅

uranusOrbit :: Fractional a => Orbit a
uranusOrbit = Orbit
  { eccentricity                  = 0.04717
  , periapsis                     = 18.2861 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 74.006 % [si|deg|]
                                      , inclination = 0.99 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 96.998857 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

uranusMass :: Fractional a => Mass a
uranusMass = 86810e21 % [si|kg|]

uranusGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
uranusGraviationalParameter = uranusMass |*| gravity_G

-- ** Neptune ♆

neptuneOrbit :: Fractional a => Orbit a
neptuneOrbit = Orbit
  { eccentricity                  = 0.008678
  , periapsis                     = 29.81 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 131.783 % [si|deg|]
                                      , inclination = 0.74 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 273.187 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

neptuneMass :: Fractional a => Mass a
neptuneMass = 102413e21 % [si|kg|]

neptuneGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
neptuneGraviationalParameter = neptuneMass |*| gravity_G

-- * Comets

halleyOrbit :: Fractional a => Orbit a
halleyOrbit = Orbit
  { eccentricity                  = 0.96714
  , periapsis                     = 0.586 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 58.42 % [si|deg|]
                                      , inclination = 162.26 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 111.33 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

-- | The fastest comet in the west. Nice for testing as it's on a hyperbolic
-- trajectory. See https://en.wikipedia.org/wiki/C/1980_E1
--
-- Orbital data from:
-- http://ssd.jpl.nasa.gov/horizons.cgi?CGISESSID=6c2730c1201457522760d3f26b7d1f00#results
c1980E1Orbit :: Fractional a => Orbit a
c1980E1Orbit = Orbit
  { eccentricity                  = 1.057731876173255
  , periapsis                     = 3.363937831611605 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
    { longitudeOfAscendingNode = 114.5581951921299 % [si|deg|]
    , inclination              = 1.661712630614323 % [si|deg|]
    }
  , periapsisSpecifier            = Eccentric
    { argumentOfPeriapsis = 135.0826233919265 % [si|deg|]
    }
  , primaryGravitationalParameter = solGraviationalParameter
  }

-- * Conversion utils

aePeriapsis :: Fractional a => a -> a -> Distance a
aePeriapsis sma ecc = sma * (1 - ecc) % [si|km|]
