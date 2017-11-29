module Complex
    exposing
        ( Complex
        , scale
        , sub
        , swap
        , mul
        , add
        , angle
        , abs
        , conj
        , real
        , fromReal
        , imag
        , fromImag
        , zero
        , fromPolar
        , fromRect
        , toPolar
        , toRect
        )


type Complex
    = Complex Float Float


mul : Complex -> Complex -> Complex
mul (Complex a b) (Complex c d) =
    Complex (a * c - b * d) (c * b + a * d)


scale : Float -> Complex -> Complex
scale f (Complex a b) =
    Complex (f * a) (f * b)


add : Complex -> Complex -> Complex
add (Complex a b) (Complex c d) =
    Complex (a + c) (b + d)


sub : Complex -> Complex -> Complex
sub a b =
    add a (scale -1.0 b)


conj : Complex -> Complex
conj (Complex a b) =
    Complex a -b


fromReal : Float -> Complex
fromReal r =
    Complex r 0.0


fromImag : Float -> Complex
fromImag i =
    Complex 0.0 i


real : Complex -> Float
real (Complex a _) =
    a


imag : Complex -> Float
imag (Complex _ b) =
    b


zero : Complex
zero =
    Complex 0.0 0.0


swap : Complex -> Complex
swap (Complex a b) =
    Complex b a


abs : Complex -> Float
abs val =
    conj val |> mul val |> real |> sqrt


ln : Complex -> Complex
ln input =
    toPolar input
        |> Tuple.mapFirst (logBase e)
        |> uncurry Complex


exp : Complex -> Complex
exp power =
    fromPolar (e ^ (real power)) (imag power)


pow : Complex -> Complex -> Complex
pow base power =
    --exploits the fact that a^b = e ^ (b*ln(a))
    exp (ln base |> mul power)


angle : Complex -> Float
angle (Complex r i) =
    atan2 i r


fromPolar : Float -> Float -> Complex
fromPolar r theta =
    Complex (cos theta) (sin theta) |> scale r


fromRect : Float -> Float -> Complex
fromRect real imag =
    Complex real imag


toRect : Complex -> ( Float, Float )
toRect (Complex r i) =
    ( r, i )


toPolar : Complex -> ( Float, Float )
toPolar a =
    ( abs a, angle a )
