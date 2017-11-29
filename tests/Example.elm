module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import FFT 
import Complex 

complexFuzz: Fuzz.Fuzzer Complex.Complex
complexFuzz = Fuzz.map2 Complex.fromRect Fuzz.float Fuzz.float 

arrayFuzz: Fuzz.Fuzzer (List Complex.Complex)
arrayFuzz = Fuzz.list complexFuzz 

epsilon : Float
epsilon = 1.0e-4

tol : Expect.FloatingPointTolerance
tol = Expect.Absolute epsilon 

compareComplexList: List Complex.Complex -> List Complex.Complex -> Expect.Expectation 
compareComplexList a b = 
    let
        lena = List.length a
        lenb = List.length b
    in 
    if lena /= lenb then
        Expect.fail "lists are different length"
    else if lena == 0 then 
        Expect.pass --same length and length zero
    else
        List.map2 Complex.sub a b 
            |> List.foldl (\a b -> b+ (Complex.abs a)) 0.0
            |> (\x -> x / (toFloat lenb))
            |> Expect.within tol 0.0
            

suite : Test
suite =
   describe "FFT Tests" [
       describe "dft" [
         fuzz arrayFuzz "checks that it is invertable" <|
            \randomArray -> 
                randomArray 
                    |> FFT.fft
                    |> FFT.ifft
                    |> compareComplexList randomArray
       ]
   ]