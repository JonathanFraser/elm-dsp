module Signal
    exposing
        ( Signal
        , product
        , inner
        , generate
        , decimate
        , swap
        , scale
        , length
        , merge
        , repeat
        , sum
        , add
        , zeroPad
        , symmetricPad
        , conj
        )

import Complex exposing (..)


type alias Signal =
    List Complex


product : Signal -> Signal -> Signal
product a b =
    List.map2 mul a b


add : Signal -> Signal -> Signal
add a b =
    List.map2 Complex.add a b


sum : Signal -> Complex.Complex
sum a =
    List.foldl Complex.add Complex.zero a


inner : Signal -> Signal -> Complex
inner a b =
    product a b |> sum


generate : (Int -> Complex) -> Int -> Signal
generate f len =
    List.range 0 (len - 1) |> List.map f


null : Int -> Signal
null len =
    generate (always Complex.zero) len


decimate : Signal -> ( Signal, Signal )
decimate input =
    List.indexedMap (,) input
        |> List.partition (Tuple.first >> (\x -> x % 2 == 0))
        |> Tuple.mapFirst (List.map Tuple.second)
        |> Tuple.mapSecond (List.map Tuple.second)


map : (Complex -> Complex) -> Signal -> Signal
map f input =
    List.map f input


conj : Signal -> Signal
conj input =
    map Complex.conj input


swap : Signal -> Signal
swap input =
    map Complex.swap input


scale : Float -> Signal -> Signal
scale value input =
    map (Complex.fromReal value |> Complex.mul) input


length : Signal -> Int
length input =
    List.length input


merge : Signal -> Signal -> Signal
merge a b =
    List.append a b


repeat : Signal -> Signal
repeat a =
    merge a a


zeroPad : Int -> Signal -> Signal
zeroPad len signal =
    len
        - length signal
        |> null
        |> merge signal


symmetricPad : Int -> Signal -> Signal
symmetricPad len signal =
    length signal
        |> (\x -> len - 2 * x + 1)
        |> null
        |> merge signal
        |> flip merge (List.drop 1 signal |> List.reverse)
