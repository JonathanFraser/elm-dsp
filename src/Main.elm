module Main exposing (..)

import Html
import FFT
import Complex
import Plot


impulse : List Complex.Complex
impulse =
    List.range 0 15
        |> List.map toFloat
        |> List.map (always 1.0)
        |> List.map Complex.fromReal


plotComplex : (Float -> Float -> Plot.DataPoint msg) -> (Complex.Complex -> Float) -> List Complex.Complex -> List (Plot.DataPoint msg)
plotComplex point xfrm list =
    List.map xfrm list
        |> List.indexedMap (\i f -> point (toFloat i) f)

main : Html.Html msg
main =
    Plot.viewSeries
        [ Plot.line (Tuple.first >> plotComplex Plot.circle Complex.real)
        , Plot.line (Tuple.second >> plotComplex Plot.square Complex.real)
        ]
        (FFT.fft impulse, FFT.dft impulse)