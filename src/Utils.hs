module Utils where

readEither :: Read a => e -> String -> Either e a
readEither err str = case reads str of
    [(res, "")] -> Right res
    _ -> Left err

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

extrapolate :: Fractional a => (a, a) -> (a, a) -> a -> a
extrapolate (a1, b1) (a2, b2) a = b1 + (b2 - b1) * (a - a1) / (a2 - a1)
