module List.Extra exposing (limit, addAndDrop)

limit: Int -> List a -> List a
limit max list =
    if (list |> List.length) >= max then list |> List.take (max - 1) else list


addAndDrop: Int -> a -> List a -> List a
addAndDrop max element list =
    element :: (list |> limit max)