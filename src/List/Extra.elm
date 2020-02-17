module List.Extra exposing (limit, addAndDrop, removeIndex)

limit: Int -> List a -> List a
limit max list =
    if (list |> List.length) >= max then list |> List.take (max - 1) else list


addAndDrop: Int -> a -> List a -> List a
addAndDrop max element list =
    element :: (list |> limit max)

removeIndex: Int -> List a -> List a
removeIndex i list =
    let run aggregator current remaining =
            case remaining of
                [] -> aggregator
                head :: tail -> 
                    if current == i then run aggregator (current + 1) tail
                    else run (head :: aggregator) (current + 1) tail
    in
        run [] 0 list |> List.reverse