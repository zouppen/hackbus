-- |Dew point calculation using a formula known as Bögel modification.
module Control.Hackbus.Math.DewPoint (dewPoint) where

-- Maybe unnecessarily accurate, but using good formulas because those
-- are available. This is Bögel modification or Arden Buck equation,
-- from https://en.wikipedia.org/wiki/Dew_point

b = 18.678
c = 257.14 -- °C
d = 234.5  -- °C

gamma_m :: Double -> Double -> Double
gamma_m t rh = log(rh/100*exp ((b-t/d)*(t/(c+t))))

-- |Calculates dew point based on temperature and relative humidity.
dewPoint :: Double -> Double -> Double
dewPoint temp rh = (c*g)/(b-g)
  where g = gamma_m temp rh
