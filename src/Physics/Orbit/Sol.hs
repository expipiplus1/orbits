module Physics.Orbit.Sol
  where

import           Data.Constants.Mechanics
import           Data.Metrology
import           Data.Units.Astronomical
import           Data.Units.SI.Parser
import           Physics.Orbit

solMass :: Fractional a => Mass a
solMass = 1988500e24 % [si|kg|]

solGraviationalParameter :: Fractional a => Quantity [si| m^3 s^-2 |] a
solGraviationalParameter = solMass |*| gravity_G

venusOrbit :: Fractional a => Orbit a
venusOrbit = Orbit
  { eccentricity                  = 0.006772
  , periapsis                     = 0.718440 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 76.680 % [si|deg|]
                                      , inclination = 2.19 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 54.884 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

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

marsOrbit :: Fractional a => Orbit a
marsOrbit = Orbit
  { eccentricity                  = 0.0934
  , periapsis                     = 1.382 % AstronomicalUnit
  , inclinationSpecifier          = Inclined
                                      { longitudeOfAscendingNode = 49.558 % [si|deg|]
                                      , inclination = 1.67 % [si|deg|]
                                      }
  , periapsisSpecifier = Eccentric { argumentOfPeriapsis = 286.502 % [si|deg|] }
  , primaryGravitationalParameter = solGraviationalParameter
  }

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


