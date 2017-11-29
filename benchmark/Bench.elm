import Benchmark
import Benchmark.Runner as Runner
import Complex 
import Signal
import FFT 

newList : Int -> Signal.Signal
newList x = Signal.generate (toFloat>>Complex.fromReal) x

benchfft: String -> (Signal.Signal -> Signal.Signal) -> Int -> Benchmark.Benchmark
benchfft name func len = 
    Benchmark.benchmark1 name func (newList len)

comparison: (Int-> Benchmark.Benchmark) -> (Int-> Benchmark.Benchmark) -> Int -> Benchmark.Benchmark
comparison func1 func2 len = 
    Benchmark.compare ("length "++toString len) (func1 len) (func2 len) 


suite : Benchmark.Benchmark 
suite = let
            dftbench = benchfft "dft" FFT.dft
            fftbench = benchfft "fft" FFT.fft
            numlist = [8191,8192]
        in 
        Benchmark.describe "FFT" <| List.map (comparison dftbench fftbench) numlist

main : Runner.BenchmarkProgram
main =
    Runner.program suite