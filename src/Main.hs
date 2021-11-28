module Main where

import System.Environment

import qualified Α01 as Α ( main , test ) -- 01 Alpha
import qualified Β02 as Β ( main , test ) -- 02 Beta
import qualified Γ03 as Γ ( main , test ) -- 03 Gamma
import qualified Δ04 as Δ ( main , test ) -- 04 Delta
import qualified Ε05 as Ε ( main , test ) -- 05 Epsilon
import qualified Ζ06 as Ζ ( main , test ) -- 06 Zeta
import qualified Η07 as Η ( main , test ) -- 07 Eta
import qualified Θ08 as Θ ( main , test ) -- 08 Theta
import qualified Ι09 as Ι ( main , test ) -- 09 Iota
import qualified Κ10 as Κ ( main , test ) -- 10 Kappa
import qualified Λ11 as Λ ( main , test ) -- 11 Lambda
import qualified Μ12 as Μ ( main , test ) -- 12 Mu
import qualified Ν13 as Ν ( main , test ) -- 13 Nu
import qualified Ξ14 as Ξ ( main , test ) -- 14 Xi
import qualified Ο15 as Ο ( main , test ) -- 15 Omicron
import qualified Π16 as Π ( main , test ) -- 16 Pi
import qualified Ρ17 as Ρ ( main , test ) -- 17 Rho
import qualified Σ18 as Σ ( main , test ) -- 18 Sigma
import qualified Τ19 as Τ ( main , test ) -- 19 Tau
import qualified Υ20 as Υ ( main , test ) -- 20 Upsilon
import qualified Φ21 as Φ ( main , test ) -- 21 Phi
import qualified Χ22 as Χ ( main , test ) -- 22 Chi
import qualified Ψ23 as Ψ ( main , test ) -- 23 Psi
import qualified Ω24 as Ω ( main , test ) -- 24 Omega

data Day = Day { test :: IO () , solv :: IO () }

day "01" = Day Α.test Α.main
day "02" = Day Β.test Β.main
day "03" = Day Γ.test Γ.main
day "04" = Day Δ.test Δ.main
day "05" = Day Ε.test Ε.main
day "06" = Day Ζ.test Ζ.main
day "07" = Day Η.test Η.main
day "08" = Day Θ.test Θ.main
day "09" = Day Ι.test Ι.main
day "10" = Day Κ.test Κ.main
day "11" = Day Λ.test Λ.main
day "12" = Day Μ.test Μ.main
day "13" = Day Ν.test Ν.main
day "14" = Day Ξ.test Ξ.main
day "15" = Day Ο.test Ο.main
day "16" = Day Π.test Π.main
day "17" = Day Ρ.test Ρ.main
day "18" = Day Σ.test Σ.main
day "19" = Day Τ.test Τ.main
day "20" = Day Υ.test Υ.main
day "21" = Day Φ.test Φ.main
day "22" = Day Χ.test Χ.main
day "23" = Day Ψ.test Ψ.main
day "24" = Day Ω.test Ω.main
day _ = Day (putStrLn "{{01-25}}") (error "invalid argument")

main :: IO ()
main = do
   n <- head <$> getArgs
   test $ day $ n
   solv $ day $ n
   pure ()

