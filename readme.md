# orbits

*For my uncle Zbys who watched the planets and stars.*

-----

Types and functions for dealing with Kepler orbits.

The main data type is `Orbit`, which describes the path of a body in orbit.

Nomenclature
------------

| Symbol | Meaning                          | Notes                          |
|--------|----------------------------------|--------------------------------|
| a      | Semi-major axis                  | Negative for hyperbolic orbits |
| b      | Semi-minor axis                  | Negative for hyperbolic orbits |
| e      | Eccentricity                     |                                |
| q      | Periapsis                        |                                |
| i      | Inclination                      |                                |
| μ      | Standard gravitational parameter |                                |
| Ω      | Longitude of the ascending node  |                                |
| l      | Semi-latus Rectum                |                                |
| n      | Mean motion                      |                                |
| p      | Period                           |                                |
| t      | Time since periapse              |                                |
| M      | Mean anomaly                     |                                |
| E      | Eccentric anomaly                | Only for elliptic orbits       |
| H      | Hyperbolic anomaly               | Only for hyperbolic orbits     |
| ν      | True anomaly                     |                                |
| h      | Specific angular momentum        |                                |
| ε      | Specific orbital energy          |                                |
| εp     | Specific potential energy        |                                |
| εk     | Specific kinetic energy          |                                |
| v      | Orbital speed or velocity        |                                |
| r      | The radius to the orbiting body  |                                |


Note that in the Haskell source uppercase symbols such as Ω and M are written
with a leading underscore.

Implementation
--------------

This package makes use of the
[`units`](https://hackage.haskell.org/package/units) package to ensure that the
implementation is correct regarding units of measure.

Contributing
------------

Contributions and bug reports are welcome!

Please feel free to contact me on GitHub or as "jophish" on freenode.

-Joe

