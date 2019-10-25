module RunBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Bitwise
import Bytes.Encode as Encode
import Int64


main : BenchmarkProgram
main =
    program suite


a =
    0x034244FF


b =
    0xDEADBEEF


aa =
    Int64.fromInt a


bb =
    Int64.fromInt b


suite : Benchmark
suite =
    describe "Array.Hamt"
        [ Benchmark.compare "addition" "native" (\_ -> a + b) "int64" (\_ -> Int64.add aa bb)
        , Benchmark.compare "subtraction" "native" (\_ -> a - b) "int64" (\_ -> Int64.subtract aa bb)
        , Benchmark.compare "bitwise and" "native" (\_ -> Bitwise.and a b) "int64" (\_ -> Int64.and aa bb)
        ]
