module Answer where


type Answer = Maybe Bool

combineOpposite :: Answer -> Answer -> Answer
combineOpposite Nothing Nothing      = Nothing
combineOpposite Nothing (Just flag)  = Just $ not flag
combineOpposite (Just flag) Nothing  = Just $ not flag
combineOpposite (Just _   ) (Just _) = error "Not in domain!"
