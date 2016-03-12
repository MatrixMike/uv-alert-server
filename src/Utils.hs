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
