module RelativeWitch where

import Answer
import Round
import Type4

relativeWitch :: Int -> Type4 -> Round2      -> Int -> Answer
relativeWitch    _      _        Preexistent    _   =  Nothing
relativeWitch    flock  boy   round2         counts
    | counts == succ flock = Just True
    | otherwise            = relativeWitch flock (opposite4 boy) (pred round2) (flock - counts) `combineOpposite` relativeWitch flock (opposite4 boy) (pred round2) (flock - counts + 1)

testRelativeWitch  :: (Int, Type4, Round2, [Int], [Answer]) -> Bool
testRelativeWitch (flock, boy, round2, counts, answers) = map (relativeWitch flock boy round2) counts == answers

theorem5 :: Bool
theorem5 = all testRelativeWitch
    [
        (4, N0Per4, minInf2  , [0..5], [Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing  ]),
        (4, N0Per4, whole2  0, [0..5], [Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Just True]),
        (4, N0Per4, andHalf 0, [0..5], [Just False, Nothing   , Nothing   , Nothing   , Nothing   , Just True]),
        (4, N0Per4, whole2  1, [0..4], [Just False, Nothing   , Nothing   , Nothing   , Just True            ]),
        (4, N0Per4, andHalf 1, [1..4], [            Just False, Nothing   , Nothing   , Just True            ]),
        (4, N0Per4, whole2  2, [1..3], [            Just False, Nothing   , Just True                        ]),
        (4, N0Per4, andHalf 2, [2..3], [                        Just False, Just True                        ]),
        (4, N0Per4, whole2  3, [    ], [                                                                     ])
    ]


theorem6 :: Bool
theorem6 = all testRelativeWitch
    [
        (4, N2Per4, minInf2  , [0..5], [Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Nothing  ]),
        (4, N2Per4, whole2  0, [0..5], [Nothing   , Nothing   , Nothing   , Nothing   , Nothing   , Just True]),
        (4, N2Per4, andHalf 0, [0..5], [Just False, Nothing   , Nothing   , Nothing   , Nothing   , Just True]),
        (4, N2Per4, whole2  1, [0..4], [Just False, Nothing   , Nothing   , Nothing   , Just True            ]),
        (4, N2Per4, andHalf 1, [1..4], [            Just False, Nothing   , Nothing   , Just True            ]),
        (4, N2Per4, whole2  2, [1..3], [            Just False, Nothing   , Just True                        ]),
        (4, N2Per4, andHalf 2, [2..3], [                        Just False, Just True                        ]),
        (4, N2Per4, whole2  3, [    ], [                                                                     ])
    ]
