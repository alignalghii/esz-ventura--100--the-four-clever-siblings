module Oracle (oracleForAllPrincesses) where

import Princess (Princess, allPrincesses)

oracleForAllPrincesses :: Bool
oracleForAllPrincesses = all oracleForPrincess $ allPrincesses 4

oracleForPrincess :: Princess -> Bool
oracleForPrincess = oracleForPrincess_ 0

oracleForPrincess_ :: Int -> Princess -> Bool
oracleForPrincess_ = undefined
