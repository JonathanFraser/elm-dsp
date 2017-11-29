module FFT exposing (..)

import Signal exposing (..)
import Complex


-- Computes a Tukey-Cooley Twiddle factor


twiddle : Int -> Int -> Complex.Complex
twiddle den num =
    Complex.fromPolar 1.0 (-2.0 * pi * (toFloat num) / (toFloat den))


chirpFactor : Int -> Int -> Complex.Complex
chirpFactor den num =
    Complex.fromPolar 1.0 (-1.0 * pi * (toFloat (num * num)) / (toFloat den))


nextPow : Int -> Int -> Int
nextPow pow val =
    let
        nextPow ret =
            if ret > val then
                ret
            else
                nextPow (pow * ret)
    in
        nextPow 1



--naive dft with O(n^2) time complexity


dft : Signal -> Signal
dft input =
    let
        len =
            List.length input

        row k =
            Signal.generate (\n -> twiddle len (n * k)) len
    in
        Signal.generate (\k -> Signal.inner (row k) input) len


idft : Signal -> Signal
idft input =
    inverter dft input


inverter : (Signal -> Signal) -> Signal -> Signal
inverter xfrm input =
    Signal.swap input
        |> xfrm
        |> Signal.swap
        |> Signal.scale (1.0 / (List.length input |> toFloat))



--implements fft, falls back on a bluestein (chirpz) for non-power of 2 lengths


fft : Signal -> Signal
fft input =
    case input of
        [] ->
            []

        a :: [] ->
            [ a ]

        _ ->
            if (Signal.length input % 2 == 0) then
                mod2 input
            else
                bluestein input


ifft : Signal -> Signal
ifft input =
    inverter fft input


mod2 : Signal -> Signal
mod2 input =
    let
        ( evens, odds ) =
            decimate input

        efft =
            fft evens

        offt =
            fft odds

        len =
            Signal.length input

        factors =
            Signal.generate (twiddle len) len

        even =
            Signal.repeat efft

        odd =
            Signal.repeat offt
    in
        Signal.product factors odd |> Signal.add even


chirp : Int -> Signal
chirp len =
    Signal.generate (chirpFactor len) len


circularConvolve : Signal -> Signal -> Signal
circularConvolve a b =
    Signal.product (fft a) (fft b) |> ifft



--Implements an fft via the bluestein algorithm, this can handle ffts of any lengths


bluestein : Signal -> Signal
bluestein input =
    let
        len =
            Signal.length input

        paddedlength =
            nextPow 2 (2 * len - 1)

        rawchirp =
            chirp len
    in
        --chirp the input
        Signal.product rawchirp input
            --pad out to a power of two for efficient convolve
            |> Signal.zeroPad paddedlength
            --convolve with the chirp TODO: precompute
            |> circularConvolve (rawchirp |> Signal.conj |> Signal.symmetricPad paddedlength)
            --recover the result an truncate
            |> Signal.product rawchirp
