module TestInt64 exposing (..)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Int64 exposing (Int64(..))
import Test exposing (..)


{-| the maximum value supported by elm-test's intRange fuzzer
-}
maxSignedInt32 =
    0x7FFFFFFF


maxInt64 =
    Int64.fromInt32s 0xFFFFFFFF 0xFFFFFFFF


y =
    Int64.fromInt32s 0x01 0xFFFFFFFF


fuzz2 a b name thunk =
    fuzz (Fuzz.tuple ( a, b )) name <| \( x, p ) -> thunk x p


fuzz3 a b c name thunk =
    fuzz (Fuzz.tuple3 ( a, b, c )) name <| \( x, p, q ) -> thunk x p q


fuzzInt64 =
    Fuzz.map2 Int64.fromInt32s (Fuzz.intRange 0 maxSignedInt32) (Fuzz.intRange 0 maxSignedInt32)


fuzzTests =
    describe "fuzz tests"
        [ fuzz2 (Fuzz.intRange 0 64) fuzzInt64 "rotateLeftBy n << rotateRightBy n = id" <|
            \n value ->
                value
                    |> Int64.rotateRightBy n
                    |> Int64.rotateLeftBy n
                    |> Int64.toHex
                    |> Expect.equal (Int64.toHex value)
        , fuzz2 (Fuzz.intRange 0 64) fuzzInt64 "rotateRightBy n << rotateLeftBy n = id" <|
            \n value ->
                value
                    |> Int64.rotateLeftBy n
                    |> Int64.rotateRightBy n
                    |> Int64.toHex
                    |> Expect.equal (Int64.toHex value)
        , fuzz2 fuzzInt64 fuzzInt64 "a & (a | b) = a" <|
            \a b ->
                Int64.and a (Int64.or a b)
                    |> Int64.toHex
                    |> Expect.equal (Int64.toHex a)
        , fuzz2 fuzzInt64 fuzzInt64 "a | (a & b) = a" <|
            \a b ->
                Int64.or a (Int64.and a b)
                    |> Int64.toHex
                    |> Expect.equal (Int64.toHex a)
        , fuzz3 fuzzInt64 fuzzInt64 fuzzInt64 "a & (b | c) = (a & b) | (a & c)" <|
            \a b c ->
                let
                    left =
                        Int64.and a (Int64.or b c)

                    right =
                        Int64.and a b
                            |> Int64.or (Int64.and a c)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz3 fuzzInt64 fuzzInt64 fuzzInt64 "a & (b ^ c) = (a & b) ^ (a & c)" <|
            \a b c ->
                let
                    left =
                        Int64.and a (Int64.xor b c)

                    right =
                        Int64.and a b
                            |> Int64.xor (Int64.and a c)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz3 fuzzInt64 fuzzInt64 fuzzInt64 "a | (b & c) = (a | b) & (a | c)" <|
            \a b c ->
                let
                    left =
                        Int64.or a (Int64.and b c)

                    right =
                        Int64.or a b
                            |> Int64.and (Int64.or a c)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz2 fuzzInt64 fuzzInt64 "~(a & b) = ~a | ~b" <|
            \a b ->
                let
                    left =
                        Int64.complement (Int64.and a b)

                    right =
                        Int64.or (Int64.complement a) (Int64.complement b)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz2 fuzzInt64 fuzzInt64 "~(a | b) = ~a & ~b" <|
            \a b ->
                let
                    left =
                        Int64.complement (Int64.or a b)

                    right =
                        Int64.and (Int64.complement a) (Int64.complement b)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz2 (Fuzz.tuple ( fuzzInt64, fuzzInt64 )) (Fuzz.tuple ( fuzzInt64, fuzzInt64 )) "(a ^ b) & (c ^ d) = (a & c) ^ (a & d) ^ (b & c) ^ (b & d)" <|
            \( a, b ) ( c, d ) ->
                let
                    left =
                        Int64.and (Int64.xor a b) (Int64.xor c d)

                    right =
                        Int64.and a c
                            |> Int64.xor (Int64.and a d)
                            |> Int64.xor (Int64.and b c)
                            |> Int64.xor (Int64.and b d)
                in
                Int64.toHex left
                    |> Expect.equal (Int64.toHex right)
        , fuzz2 Fuzz.int Fuzz.int "addition" <|
            \a b ->
                Int64.add (Int64.fromInt a) (Int64.fromInt b)
                    |> Expect.equal (Int64.fromInt (a + b))
        , fuzz2 Fuzz.int Fuzz.int "subtraction" <|
            \a b ->
                Int64.subtract (Int64.fromInt a) (Int64.fromInt b)
                    |> Expect.equal (Int64.fromInt (a - b))
        , fuzz Fuzz.int "from int to signed string" <|
            \a ->
                a
                    |> Int64.fromInt
                    |> Int64.toSignedString
                    |> Expect.equal (String.fromInt a)
        , fuzz (Fuzz.map abs Fuzz.int) "from positive in to unsigned string" <|
            \a ->
                a
                    |> Int64.fromInt
                    |> Int64.toUnsignedString
                    |> Expect.equal (String.fromInt a)
        , fuzz fuzzInt64 "decode BE << encode BE = id" <|
            \a ->
                a
                    |> Int64.encoder BE
                    |> Encode.encode
                    |> Decode.decode (Int64.decoder BE)
                    |> Expect.equal (Just a)
        , fuzz fuzzInt64 "decode LE << encode LE = id" <|
            \a ->
                a
                    |> Int64.encoder LE
                    |> Encode.encode
                    |> Decode.decode (Int64.decoder LE)
                    |> Expect.equal (Just a)
        , fuzz2 (Fuzz.intRange 0 maxSignedInt32) (Fuzz.intRange 0 maxSignedInt32) "unsignedCompare" <|
            \a b ->
                Int64.unsignedCompare (Int64.fromInt a) (Int64.fromInt b)
                    |> Expect.equal (Basics.compare a b)
        , fuzz2 Fuzz.int Fuzz.int "signedCompare" <|
            \a b ->
                Int64.signedCompare (Int64.fromInt a) (Int64.fromInt b)
                    |> Expect.equal (Basics.compare a b)
        ]


rotateLeftBy =
    let
        helper input n output =
            test ("rotateLeftBy " ++ String.fromInt n ++ " " ++ Int64.toHex input) <|
                \_ ->
                    input
                        |> Int64.rotateLeftBy n
                        |> Int64.toBitString
                        |> Expect.equal (output |> Int64.toBitString)
    in
    describe "rotate left by"
        [ helper (Int64.fromInt32s 0 1) 1 (Int64.fromInt32s 0 2)
        , helper (Int64.fromInt32s 0 2) 1 (Int64.fromInt32s 0 4)
        , helper (Int64.fromInt32s 0 4) 1 (Int64.fromInt32s 0 8)
        , helper (Int64.fromInt32s 0 8) 1 (Int64.fromInt32s 0 16)
        , helper (Int64.fromInt32s 0 0x80000000) 1 (Int64.fromInt32s 1 0)
        , helper (Int64.fromInt32s 0 1) 15 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 15 1))
        , helper (Int64.fromInt32s 0 1) 16 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 16 1))
        , helper (Int64.fromInt32s 0 1) 30 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 30 1))
        , helper (Int64.fromInt32s 0 1) 31 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 31 1))
        , helper (Int64.fromInt32s 0 1) 32 (Int64.fromInt32s 1 0)
        , helper (Int64.fromInt32s 0x80000000 0) 1 (Int64.fromInt32s 0 1)
        , helper (Int64.fromInt32s 0x80000000 0) 31 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 30 1))
        , helper (Int64.fromInt32s 0x80000000 0) 32 (Int64.fromInt32s 0 (Bitwise.shiftLeftBy 31 1))
        , helper (Int64.fromInt32s 0x80000000 0) 64 (Int64.fromInt32s 0x80000000 0)
        ]


suite : Test
suite =
    describe "Int64"
        [ test "overflow 1" <|
            \_ ->
                Int64.add maxInt64 (Int64.fromInt32s 0x01 0xFFFFFFFF)
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0x01 0xFFFFFFFE |> Int64.toHex)
        , test "overflow 2" <|
            \_ ->
                Int64.add maxInt64 maxInt64
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0xFFFFFFFF 0xFFFFFFFE |> Int64.toHex)
        , test "overflow 3" <|
            \_ ->
                Int64.add maxInt64 (Int64.fromInt32s 0 1)
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0 0 |> Int64.toHex)
        , test "overflow 4" <|
            \_ ->
                Int64.add y y
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0x03 0xFFFFFFFE |> Int64.toHex)
        , test "overflow 5" <|
            \_ ->
                Int64.add (Int64.fromInt32s 0 42) (Int64.fromInt32s 0 42)
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0 84 |> Int64.toHex)
        , test "overflow 6" <|
            \_ ->
                Int64.add (Int64.fromInt32s 1 0xFFFFFFFF) (Int64.fromInt32s 0xFFFFFFFF 0x00)
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0 0xFFFFFFFF |> Int64.toHex)
        ]


unsignedToString =
    let
        testFormatting value result =
            test (Debug.toString value) <|
                \_ ->
                    Int64.toUnsignedString value
                        |> Expect.equal result
    in
    describe "unsigned to string"
        [ testFormatting (Int64.fromInt32s 1 0xFFFFFFFF) "8589934591"
        , testFormatting (Int64.fromInt32s 0 0xFFFFFFFF) "4294967295"
        , testFormatting (Int64.fromInt32s 0 10) "10"
        , testFormatting (Int64.fromInt32s 0x80000000 0) "9223372036854775808"
        , testFormatting (Int64.fromInt32s 0x8E2567F0 0x73460482) "10242707209947841666"
        , testFormatting (Int64.fromInt32s 0x27499948 0xFD6CDA4F) "2830962379547531855"
        , testFormatting (Int64.fromInt32s 0xA670614A 0xC5ADAFAD) "11993192781459599277"
        , testFormatting (Int64.fromInt32s 0xB1114642 0x6A85E060) "12759056470386270304"
        , testFormatting (Int64.fromInt32s 0x281AB7C0 0x30F0F9FB) "2889824146994297339"
        , testFormatting (Int64.fromInt32s 0xA7DD04DC 0x17B747C2) "12095829517524223938"
        , testFormatting (Int64.fromInt32s 0x2CC72718 0x089691C4) "3226590642211033540"
        , testFormatting (Int64.fromInt32s 0x0381A9E3 0xE4ED5E0D) "252669850372890125"
        , testFormatting (Int64.fromInt32s 0x8354ED57 0x40AED833) "9463449676015392819"
        , testFormatting (Int64.fromInt32s 0xF3A2D73E 0x65E23CAA) "17555830960440884394"
        , testFormatting (Int64.fromInt32s 0x127B7C6F 0x0A682D2F) "1331794931175927087"
        , testFormatting (Int64.fromInt32s 0xCC7492D7 0x26CBBB93) "14732561733802245011"
        , testFormatting (Int64.fromInt32s 0x531232BA 0xF7E85EC9) "5985902633333317321"
        , testFormatting (Int64.fromInt32s 0xC3116653 0x3CC53547) "14056128419687904583"
        , testFormatting (Int64.fromInt32s 0x3403A19A 0x314A168B) "3748016998526359179"
        , testFormatting (Int64.fromInt32s 0x6732CEB0 0x696D00F7) "7436233191820427511"
        , testFormatting (Int64.fromInt32s 0xAEEF7A95 0x12F51704) "12605428662720075524"
        , testFormatting (Int64.fromInt32s 0x4C7E3FB7 0xA8B98F26) "5511913051990363942"
        , testFormatting (Int64.fromInt32s 0x8E50E496 0xB22680ED) "10254947687407714541"
        , testFormatting (Int64.fromInt32s 0xFF95F52D 0x11C4CCD7) "18416895825121955031"
        , testFormatting (Int64.fromInt32s 0x9C23341A 0xEE436835) "11250893584372820021"
        , testFormatting (Int64.fromInt32s 0xE6411124 0x9271C710) "16591561350983042832"
        , testFormatting (Int64.fromInt32s 0xF28BFBFC 0xEE601A1A) "17477339842690947610"
        , testFormatting (Int64.fromInt32s 0x4979E438 0xD083781F) "5294513769618307103"
        , testFormatting (Int64.fromInt32s 0xDF8D3E95 0xC03D918D) "16108600255070441869"
        , testFormatting (Int64.fromInt32s 0x2FF80B01 0x5DCB544A) "3456524814503334986"
        , testFormatting (Int64.fromInt32s 0xDCDB249E 0x655000B4) "15914353970966954164"
        , testFormatting (Int64.fromInt32s 0x613DE4F5 0x630B67E9) "7007008337838172137"
        , testFormatting (Int64.fromInt32s 0xF9034610 0x9DE605C5) "17943262377556706757"
        , testFormatting (Int64.fromInt32s 0x61A5B2A9 0x38C37416) "7036226432707818518"
        , testFormatting (Int64.fromInt32s 0xFCB19125 0x8C70DF15) "18208494358891650837"
        , testFormatting (Int64.fromInt32s 0xA1C8A5F5 0xB5DA6784) "11657750110185088900"
        , testFormatting (Int64.fromInt32s 0xA990F5B5 0x5012E876) "12218535948137457782"
        , testFormatting (Int64.fromInt32s 0xF0B74F54 0xDBC089A6) "17345419715723430310"
        , testFormatting (Int64.fromInt32s 0xFB5D0720 0xD9CBF59B) "18112641114028373403"
        , testFormatting (Int64.fromInt32s 0xCB19A2BE 0xBB52AC06) "14634907404187380742"
        , testFormatting (Int64.fromInt32s 0xD2D18E4B 0x360AEFBF) "15191079471777771455"
        , testFormatting (Int64.fromInt32s 0xFFF12FA5 0x87841EA2) "18442574337048583842"
        , testFormatting (Int64.fromInt32s 0x68FF538E 0xE5513CC8) "7565857772203490504"
        , testFormatting (Int64.fromInt32s 0xAE9A98BF 0x4CB37407) "12581536456405906439"
        , testFormatting (Int64.fromInt32s 0x4BD374A5 0x6C9FDA06) "5463839026771384838"
        , testFormatting (Int64.fromInt32s 0x6EAE9A0A 0xC61CBA23) "7975481361183848995"
        , testFormatting (Int64.fromInt32s 0xA7CA1017 0xCB7CFE08) "12090493844013776392"
        , testFormatting (Int64.fromInt32s 0x9A68B00A 0xCB353784) "11126336439824234372"
        , testFormatting (Int64.fromInt32s 0x55773DAA 0x7D6BACD6) "6158458817910385878"
        , testFormatting (Int64.fromInt32s 0xD0755976 0xCC9342D8) "15021010498937373400"
        , testFormatting (Int64.fromInt32s 0x64F72DF6 0x988B6973) "7275334260184803699"
        , testFormatting (Int64.fromInt32s 0xBFDC17C1 0x2B6CBCA3) "13824951074545253539"
        , testFormatting (Int64.fromInt32s 0xF27E2101 0x144B563B) "17473439892763268667"
        , testFormatting (Int64.fromInt32s 0xFB491D8A 0xDD37177A) "18107036259073857402"
        , testFormatting (Int64.fromInt32s 0x21177AF0 0x1A8AD767) "2384509699372013415"
        , testFormatting (Int64.fromInt32s 0x7E050274 0xA18BE3C8) "9080666923612234696"
        , testFormatting (Int64.fromInt32s 0x4B71D9F6 0xE266516E) "5436365879596437870"
        , testFormatting (Int64.fromInt32s 0x777A4278 0x24721590) "8609266721447089552"
        , testFormatting (Int64.fromInt32s 0x3EA89812 0x5ABEBABC) "4515025831038204604"
        , testFormatting (Int64.fromInt32s 0xD60A4505 0x063B30D4) "15423215761765380308"
        , testFormatting (Int64.fromInt32s 0x20696A5B 0x193C96AE) "2335514821266282158"
        , testFormatting (Int64.fromInt32s 0x336081F7 0x6298BA88) "3702101793209629320"
        , testFormatting (Int64.fromInt32s 0x765A1ED5 0xF6A2C09C) "8528162748694184092"
        , testFormatting (Int64.fromInt32s 0xF4B8A08E 0x3AA21DF8) "17634020873698680312"
        , testFormatting (Int64.fromInt32s 0xED16B3AB 0x81CC6146) "17084039785674989894"
        , testFormatting (Int64.fromInt32s 0x9149FDB0 0xFA4B1162) "10469177745354723682"
        , testFormatting (Int64.fromInt32s 0xA97BBDAB 0x83CB877D) "12212563358893442941"
        , testFormatting (Int64.fromInt32s 0x95715FB5 0x68D127DA) "10768493416771823578"
        , testFormatting (Int64.fromInt32s 0xBF61B555 0xCAEFB466) "13790502914066723942"
        , testFormatting (Int64.fromInt32s 0x95F6B5E1 0x24DE159C) "10806024334512887196"
        , testFormatting (Int64.fromInt32s 0x2B068A20 0x23D73A77) "3100317264136059511"
        , testFormatting (Int64.fromInt32s 0x05D388AB 0xBE318C6A) "419829461487291498"
        , testFormatting (Int64.fromInt32s 0x2FBE16BB 0x38D78CA6) "3440212158725983398"
        , testFormatting (Int64.fromInt32s 0x3A9A54A2 0x68F4594E) "4222780657135540558"
        , testFormatting (Int64.fromInt32s 0x4EF54B74 0xED63EAD5) "5689536669823527637"
        , testFormatting (Int64.fromInt32s 0x611B0107 0x0F8EFB1C) "6997187575887624988"
        , testFormatting (Int64.fromInt32s 0x046EEBEF 0xCA61A05F) "319452038714990687"
        , testFormatting (Int64.fromInt32s 0x0A3D34D6 0x3B3BCFD4) "737804008680050644"
        , testFormatting (Int64.fromInt32s 0xFD12BBD7 0x531DE2ED) "18235844374663389933"
        , testFormatting (Int64.fromInt32s 0xAFB8603D 0xD9C38E82) "12661976171114892930"
        , testFormatting (Int64.fromInt32s 0x346E41F0 0x73FED9C4) "3778029638404463044"
        , testFormatting (Int64.fromInt32s 0x51AB4B31 0xA10C5026) "5884880014617104422"
        , testFormatting (Int64.fromInt32s 0x401002B5 0x1F4DCF3C) "4616192594992287548"
        , testFormatting (Int64.fromInt32s 0x551B131F 0xE91A550C) "6132516345370793228"
        , testFormatting (Int64.fromInt32s 0xD1F66EF9 0x73381374) "15129402015856726900"
        , testFormatting (Int64.fromInt32s 0x56688B96 0x57663E1F) "6226379962667384351"
        , testFormatting (Int64.fromInt32s 0xB18B2E99 0x2541E1CE) "12793370401765974478"
        , testFormatting (Int64.fromInt32s 0x02A97023 0x5713B0EC) "191807756227031276"
        , testFormatting (Int64.fromInt32s 0x8FDEE2B7 0x4F35B46A) "10366972669189272682"
        , testFormatting (Int64.fromInt32s 0x079DD3EC 0x8084C0E9) "548827742330994921"
        , testFormatting (Int64.fromInt32s 0x088BF0D4 0x51F3D237) "615850568764871223"
        , testFormatting (Int64.fromInt32s 0xDAF2E5FA 0x1428DB2A) "15776925306875075370"
        , testFormatting (Int64.fromInt32s 0xC74E4766 0x0931360B) "14361494765297546763"
        , testFormatting (Int64.fromInt32s 0x2BF23F08 0x39D82B6D) "3166662792557636461"
        , testFormatting (Int64.fromInt32s 0x2FCB69D5 0x062C56BB) "3443962703707395771"
        , testFormatting (Int64.fromInt32s 0xF726778C 0xB5206B6B) "17809053222701132651"
        , testFormatting (Int64.fromInt32s 0x5E2E8636 0xF296061F) "6786509259050190367"
        , testFormatting (Int64.fromInt32s 0xDD6C0C44 0xD2BF2E2E) "15955141069599878702"
        , testFormatting (Int64.fromInt32s 0x2A28ED9B 0x9638CE93) "3037939201157418643"
        , testFormatting (Int64.fromInt32s 0xDDB3F711 0x29B2498A) "15975383956299336074"
        , testFormatting (Int64.fromInt32s 0x92E34231 0x798BFD38) "10584376329510845752"
        , testFormatting (Int64.fromInt32s 0x7BE34896 0xF17AE585) "8927058699512112517"
        , testFormatting (Int64.fromInt32s 0x76B601F4 0x26FA754C) "8554026690374432076"
        , testFormatting (Int64.fromInt32s 0x97B927EF 0x4C493B8E) "10932813479149124494"
        , testFormatting (Int64.fromInt32s 0x73D07D7A 0x8259FD7A) "8345308074643946874"
        ]


fromInt =
    let
        testFormatting value result =
            test (Debug.toString value ++ result) <|
                \_ ->
                    Int64.toSignedString value
                        |> Expect.equal result
    in
    describe "from int"
        [ let
            value =
                -1 * 0xFFFFFFFF - 0xFFFF
          in
          testFormatting (Int64.fromInt value) (String.fromInt value)
        , testFormatting (Int64.fromInt -5) "-5"
        , testFormatting (Int64.fromInt 0xFFFFFFFF) (String.fromInt 0xFFFFFFFF)
        , let
            value =
                0xFFFFFFFF + 0xFFFF
          in
          testFormatting (Int64.fromInt value) (String.fromInt value)
        , let
            value =
                0xFFFFFFFF + 0xFFFFFFFF
          in
          testFormatting (Int64.fromInt value) (String.fromInt value)
        , testFormatting (Int64.fromInt (-1 * 0xFFFFFFFF)) (String.fromInt (-1 * 0xFFFFFFFF))
        , testFormatting (Int64.fromInt -4294967293) (String.fromInt -4294967293)
        , testFormatting (Int64.fromInt32s 4294967293 3) (String.fromInt -12884901885)
        , testFormatting (Int64.fromInt 8589934591) "8589934591"
        ]


signedToString =
    let
        testFormatting value result =
            test (Debug.toString value) <|
                \_ ->
                    Int64.toSignedString value
                        |> Expect.equal result
    in
    describe "signed to string"
        [ testFormatting (Int64.fromInt32s 0x80000000 0x00) "-9223372036854775808"
        , testFormatting (Int64.fromInt32s 0x80000000 0x80000000) "-9223372034707292160"
        , testFormatting (Int64.fromInt32s 0x90000000 0x00) "-8070450532247928832"
        , testFormatting (Int64.fromInt32s 0xFFFFFFFF 0xFFFFFFFF) "-1"
        , testFormatting (Int64.fromInt32s 0x00 0xFFFFFFFF) "4294967295"
        , testFormatting (Int64.fromInt32s 0xFFFFFFFF 0x00) "-4294967296"
        , testFormatting (Int64.fromInt32s 0 0) "0"
        , testFormatting (Int64.fromInt32s 0xC6665995 0x571C36CB) "-4150531508628867381"
        , testFormatting (Int64.fromInt32s 0x494C2FC9 0x629E1733) "5281649004988208947"
        , testFormatting (Int64.fromInt32s 0xE0A3F483 0xE5ABCE0E) "-2259693740678722034"
        , testFormatting (Int64.fromInt32s 0x57D35F2F 0xB3E57743) "6328506559871940419"
        , testFormatting (Int64.fromInt32s 0x9752491C 0x92C21931) "-7542886038822053583"
        , testFormatting (Int64.fromInt32s 0xB6D6E9F9 0x0815E7C6) "-5271769057998805050"
        , testFormatting (Int64.fromInt32s 0x071F7EE9 0x11D0D76B) "513268422034904939"
        , testFormatting (Int64.fromInt32s 0x5A08C5A5 0x2E6382AC) "6487652576465748652"
        , testFormatting (Int64.fromInt32s 0x0923CA85 0x08003FC2) "658592643239919554"
        , testFormatting (Int64.fromInt32s 0x9D8BD451 0x4A7DA1AD) "-7094343342384897619"
        ]


subtractGenerated =
    let
        testOverflow a b result =
            test (Debug.toString a ++ " - " ++ Debug.toString b ++ " =? " ++ Debug.toString result) <|
                \_ ->
                    Int64.subtract a b
                        |> Int64.toHex
                        |> Expect.equal (result |> Int64.toHex)
    in
    describe "generated signed tests (using rust)"
        [ testOverflow (Int64.fromInt32s 0x7148A443 0xB092DDD4) (Int64.fromInt32s 0xBEB685C7 0xCF55A0A4) (Int64.fromInt32s 0xB2921E7B 0xE13D3D30)
        , testOverflow (Int64.fromInt32s 0x8F8ADC8D 0x97792DDC) (Int64.fromInt32s 0xCEA8FE82 0xEAB697CB) (Int64.fromInt32s 0xC0E1DE0A 0xACC29611)
        , testOverflow (Int64.fromInt32s 0xC9DB4373 0xAD30945B) (Int64.fromInt32s 0xC395475D 0x536142E5) (Int64.fromInt32s 0x0645FC16 0x59CF5176)
        , testOverflow (Int64.fromInt32s 0xCF1B84FD 0x2065CB14) (Int64.fromInt32s 0xA21210E9 0x420AC69F) (Int64.fromInt32s 0x2D097413 0xDE5B0475)
        , testOverflow (Int64.fromInt32s 0x3A8A373B 0x953746BB) (Int64.fromInt32s 0xA0A12B37 0xC96F8479) (Int64.fromInt32s 0x99E90C03 0xCBC7C242)
        , testOverflow (Int64.fromInt32s 0x278FDB1D 0xCA148E45) (Int64.fromInt32s 0xF45B055A 0xABBF9FA5) (Int64.fromInt32s 0x3334D5C3 0x1E54EEA0)
        , testOverflow (Int64.fromInt32s 0xA3288AE8 0x005AC116) (Int64.fromInt32s 0x1B39C0B8 0xADF74707) (Int64.fromInt32s 0x87EECA2F 0x52637A0F)
        , testOverflow (Int64.fromInt32s 0x3EACEAEF 0x8F8B9C85) (Int64.fromInt32s 0x74628332 0x81E2ED93) (Int64.fromInt32s 0xCA4A67BD 0x0DA8AEF2)
        , testOverflow (Int64.fromInt32s 0xE05A0EAF 0xA621BC21) (Int64.fromInt32s 0xA7808A86 0x250013BA) (Int64.fromInt32s 0x38D98429 0x8121A867)
        , testOverflow (Int64.fromInt32s 0xC1594C52 0x16617453) (Int64.fromInt32s 0xAB0D77AA 0xF521C99F) (Int64.fromInt32s 0x164BD4A7 0x213FAAB4)
        , testOverflow (Int64.fromInt32s 0xB79EC4E5 0xD48AB994) (Int64.fromInt32s 0x2C6F5783 0x25EABDD6) (Int64.fromInt32s 0x8B2F6D62 0xAE9FFBBE)
        , testOverflow (Int64.fromInt32s 0xD357C34C 0x7399325A) (Int64.fromInt32s 0x5C071FF4 0xE3FF27A9) (Int64.fromInt32s 0x7750A357 0x8F9A0AB1)
        , testOverflow (Int64.fromInt32s 0x2715E144 0xFCF9AE7D) (Int64.fromInt32s 0xCBE2AFFC 0xD66F7471) (Int64.fromInt32s 0x5B333148 0x268A3A0C)
        , testOverflow (Int64.fromInt32s 0x66E27CF4 0x803619FA) (Int64.fromInt32s 0x110392DE 0xC65BF505) (Int64.fromInt32s 0x55DEEA15 0xB9DA24F5)
        , testOverflow (Int64.fromInt32s 0x9014DC56 0xFE36985B) (Int64.fromInt32s 0x407F7BC9 0x9A5BAA63) (Int64.fromInt32s 0x4F95608D 0x63DAEDF8)
        , testOverflow (Int64.fromInt32s 0x2BE6B71B 0x2B5B0793) (Int64.fromInt32s 0x8DAFBE67 0x4D09575B) (Int64.fromInt32s 0x9E36F8B3 0xDE51B038)
        , testOverflow (Int64.fromInt32s 0x5C7EFBD5 0xF5500996) (Int64.fromInt32s 0x1EB2F73B 0x0F3508CA) (Int64.fromInt32s 0x3DCC049A 0xE61B00CC)
        , testOverflow (Int64.fromInt32s 0xF160EDF9 0xAFC9C84A) (Int64.fromInt32s 0x0F858D8F 0xA5E03A70) (Int64.fromInt32s 0xE1DB606A 0x09E98DDA)
        , testOverflow (Int64.fromInt32s 0x5EE27E0F 0x3D951E83) (Int64.fromInt32s 0xA969064E 0xCB1E2113) (Int64.fromInt32s 0xB57977C0 0x7276FD70)
        , testOverflow (Int64.fromInt32s 0x8A85A049 0x265490C8) (Int64.fromInt32s 0x6D3ADADA 0xD7A12A88) (Int64.fromInt32s 0x1D4AC56E 0x4EB36640)
        , testOverflow (Int64.fromInt32s 0x8D166B04 0xA5DF97B1) (Int64.fromInt32s 0x94B3AB99 0x4204D0CA) (Int64.fromInt32s 0xF862BF6B 0x63DAC6E7)
        , testOverflow (Int64.fromInt32s 0xDC124356 0x54F8E25C) (Int64.fromInt32s 0x7774409F 0x1458C76B) (Int64.fromInt32s 0x649E02B7 0x40A01AF1)
        , testOverflow (Int64.fromInt32s 0xD0D802A9 0x5FAC153A) (Int64.fromInt32s 0xE0DC400D 0xC1568835) (Int64.fromInt32s 0xEFFBC29B 0x9E558D05)
        , testOverflow (Int64.fromInt32s 0x21AFB0B6 0x7FBABDE9) (Int64.fromInt32s 0x9D154F69 0xF8216E10) (Int64.fromInt32s 0x849A614C 0x87994FD9)
        , testOverflow (Int64.fromInt32s 0x0C825B46 0xEC623782) (Int64.fromInt32s 0xE5E7F33E 0x9E92E7CA) (Int64.fromInt32s 0x269A6808 0x4DCF4FB8)
        , testOverflow (Int64.fromInt32s 0xAC352854 0x342D9415) (Int64.fromInt32s 0xEA68A133 0x93C64C61) (Int64.fromInt32s 0xC1CC8720 0xA06747B4)
        , testOverflow (Int64.fromInt32s 0x37531A85 0x7EAFB4EE) (Int64.fromInt32s 0x81CCFECE 0x64E5EEA2) (Int64.fromInt32s 0xB5861BB7 0x19C9C64C)
        , testOverflow (Int64.fromInt32s 0xDE8001F5 0x4206F338) (Int64.fromInt32s 0xC67729E2 0xB23B5C39) (Int64.fromInt32s 0x1808D812 0x8FCB96FF)
        , testOverflow (Int64.fromInt32s 0xA64E7045 0x123ECACF) (Int64.fromInt32s 0x19F2C227 0x9D80E2AD) (Int64.fromInt32s 0x8C5BAE1D 0x74BDE822)
        , testOverflow (Int64.fromInt32s 0xC18E2001 0x87D22310) (Int64.fromInt32s 0x62335866 0xBDF511C8) (Int64.fromInt32s 0x5F5AC79A 0xC9DD1148)
        , testOverflow (Int64.fromInt32s 0x41C764F0 0x97A140DB) (Int64.fromInt32s 0xF1D924FA 0x493A9F80) (Int64.fromInt32s 0x4FEE3FF6 0x4E66A15B)
        , testOverflow (Int64.fromInt32s 0xE4E1B613 0xF6C1E604) (Int64.fromInt32s 0x510099BF 0xAD29C24F) (Int64.fromInt32s 0x93E11C54 0x499823B5)
        , testOverflow (Int64.fromInt32s 0x52A9FEDF 0x490050A7) (Int64.fromInt32s 0x6DF389CF 0xC71BB310) (Int64.fromInt32s 0xE4B6750F 0x81E49D97)
        , testOverflow (Int64.fromInt32s 0x78338409 0xC3FF7EBD) (Int64.fromInt32s 0xBD8F0992 0x76E21BCA) (Int64.fromInt32s 0xBAA47A77 0x4D1D62F3)
        , testOverflow (Int64.fromInt32s 0x68613498 0x25EE7E49) (Int64.fromInt32s 0xD87D5757 0xA8E4E225) (Int64.fromInt32s 0x8FE3DD40 0x7D099C24)
        , testOverflow (Int64.fromInt32s 0xD239FEB6 0x79A441D8) (Int64.fromInt32s 0x651A1DE2 0x78639D0B) (Int64.fromInt32s 0x6D1FE0D4 0x0140A4CD)
        , testOverflow (Int64.fromInt32s 0x587779EB 0x990270AD) (Int64.fromInt32s 0x846FBA79 0x91F50481) (Int64.fromInt32s 0xD407BF72 0x070D6C2C)
        , testOverflow (Int64.fromInt32s 0xF5B477C9 0x11F7A611) (Int64.fromInt32s 0x9C77CDB1 0xC3571011) (Int64.fromInt32s 0x593CAA17 0x4EA09600)
        , testOverflow (Int64.fromInt32s 0x6EB31429 0x866575C9) (Int64.fromInt32s 0x399076B7 0xF73DC987) (Int64.fromInt32s 0x35229D71 0x8F27AC42)
        , testOverflow (Int64.fromInt32s 0x2DD436A2 0x0276BBCD) (Int64.fromInt32s 0x88BC0630 0xF58691EE) (Int64.fromInt32s 0xA5183071 0x0CF029DF)
        , testOverflow (Int64.fromInt32s 0xAB100C95 0xE5D99364) (Int64.fromInt32s 0x91BED6B1 0xD81D5ADF) (Int64.fromInt32s 0x195135E4 0x0DBC3885)
        , testOverflow (Int64.fromInt32s 0xC5FEBCEE 0xBD6D52B0) (Int64.fromInt32s 0x0F5F4FA9 0xBDAAB805) (Int64.fromInt32s 0xB69F6D44 0xFFC29AAB)
        , testOverflow (Int64.fromInt32s 0x7D125712 0xC4F1005E) (Int64.fromInt32s 0xB97E3035 0x18D6E70D) (Int64.fromInt32s 0xC39426DD 0xAC1A1951)
        , testOverflow (Int64.fromInt32s 0xE57281E7 0x6AA71441) (Int64.fromInt32s 0xFFC8AFB6 0x9B4D5DC6) (Int64.fromInt32s 0xE5A9D230 0xCF59B67B)
        , testOverflow (Int64.fromInt32s 0xF64035B3 0x8908BA77) (Int64.fromInt32s 0x6CEDAD34 0xD2A0C154) (Int64.fromInt32s 0x8952887E 0xB667F923)
        , testOverflow (Int64.fromInt32s 0xA8B4A995 0x87BA3D6D) (Int64.fromInt32s 0x82070F6F 0x79092D88) (Int64.fromInt32s 0x26AD9A26 0x0EB10FE5)
        , testOverflow (Int64.fromInt32s 0xEDAFCC00 0x209DE1D0) (Int64.fromInt32s 0x61980FF8 0xA4C0B782) (Int64.fromInt32s 0x8C17BC07 0x7BDD2A4E)
        , testOverflow (Int64.fromInt32s 0xFA558995 0x23F80ABB) (Int64.fromInt32s 0x14EB224C 0xD76FD473) (Int64.fromInt32s 0xE56A6748 0x4C883648)
        , testOverflow (Int64.fromInt32s 0x30ED225A 0xC8D5A6FC) (Int64.fromInt32s 0xE9B69118 0xAB303EE6) (Int64.fromInt32s 0x47369142 0x1DA56816)
        , testOverflow (Int64.fromInt32s 0xF8F83CE3 0x7D7C42EE) (Int64.fromInt32s 0x9C5A9526 0xB39E5A6E) (Int64.fromInt32s 0x5C9DA7BC 0xC9DDE880)
        , testOverflow (Int64.fromInt32s 0xB28A83D2 0x95D84A1F) (Int64.fromInt32s 0x49BF6184 0xFBF6AEE1) (Int64.fromInt32s 0x68CB224D 0x99E19B3E)
        , testOverflow (Int64.fromInt32s 0x198649B5 0x4306F8DB) (Int64.fromInt32s 0xEA921AE1 0x7995464B) (Int64.fromInt32s 0x2EF42ED3 0xC971B290)
        , testOverflow (Int64.fromInt32s 0x814069FA 0xA0E79720) (Int64.fromInt32s 0x3161F8FB 0x9A4C9AD4) (Int64.fromInt32s 0x4FDE70FF 0x069AFC4C)
        , testOverflow (Int64.fromInt32s 0x623C8C79 0x6A2191B2) (Int64.fromInt32s 0x265A8FF6 0x85529770) (Int64.fromInt32s 0x3BE1FC82 0xE4CEFA42)
        , testOverflow (Int64.fromInt32s 0x6583BAE5 0x0EB0EE18) (Int64.fromInt32s 0xFE40F2BD 0xFBDEB755) (Int64.fromInt32s 0x6742C827 0x12D236C3)
        , testOverflow (Int64.fromInt32s 0xE076D802 0xD146A4FF) (Int64.fromInt32s 0x98B06813 0x9B6B505B) (Int64.fromInt32s 0x47C66FEF 0x35DB54A4)
        , testOverflow (Int64.fromInt32s 0x80A22F94 0xA498F5B4) (Int64.fromInt32s 0x00E4F380 0x46B06E6E) (Int64.fromInt32s 0x7FBD3C14 0x5DE88746)
        , testOverflow (Int64.fromInt32s 0x5B507B4F 0x2198270D) (Int64.fromInt32s 0xC644A5F0 0xC3EC9617) (Int64.fromInt32s 0x950BD55E 0x5DAB90F6)
        , testOverflow (Int64.fromInt32s 0x670C583D 0x6D115A87) (Int64.fromInt32s 0x5A33C67B 0x6BBBCFF5) (Int64.fromInt32s 0x0CD891C2 0x01558A92)
        , testOverflow (Int64.fromInt32s 0xB131D66C 0x0A57593C) (Int64.fromInt32s 0x42F019E3 0x6C153BCC) (Int64.fromInt32s 0x6E41BC88 0x9E421D70)
        , testOverflow (Int64.fromInt32s 0xF064816A 0x276127B2) (Int64.fromInt32s 0x415F5386 0x9FC44F1D) (Int64.fromInt32s 0xAF052DE3 0x879CD895)
        , testOverflow (Int64.fromInt32s 0x1D6110CE 0x095F1965) (Int64.fromInt32s 0x7DB346D4 0x3D65D0DD) (Int64.fromInt32s 0x9FADC9F9 0xCBF94888)
        , testOverflow (Int64.fromInt32s 0x1A3E4045 0x84F5B9BC) (Int64.fromInt32s 0xA0E67DF9 0x83222DAD) (Int64.fromInt32s 0x7957C24C 0x01D38C0F)
        , testOverflow (Int64.fromInt32s 0x8E47BC90 0x9184C801) (Int64.fromInt32s 0x9954D318 0x3761E5C3) (Int64.fromInt32s 0xF4F2E978 0x5A22E23E)
        , testOverflow (Int64.fromInt32s 0xFC926678 0xEC160ADA) (Int64.fromInt32s 0x4BFA6A62 0x7C9EFFBC) (Int64.fromInt32s 0xB097FC16 0x6F770B1E)
        , testOverflow (Int64.fromInt32s 0x1F1D7CDB 0x96410FD6) (Int64.fromInt32s 0xC452FFEB 0x434E20E5) (Int64.fromInt32s 0x5ACA7CF0 0x52F2EEF1)
        , testOverflow (Int64.fromInt32s 0xF85D6A1E 0xE55186BC) (Int64.fromInt32s 0x291A91EC 0xD5ED8AE0) (Int64.fromInt32s 0xCF42D832 0x0F63FBDC)
        , testOverflow (Int64.fromInt32s 0x4DFAFB0D 0xF43EC195) (Int64.fromInt32s 0x7F90B80D 0x1656852B) (Int64.fromInt32s 0xCE6A4300 0xDDE83C6A)
        , testOverflow (Int64.fromInt32s 0xC8555BD0 0xBE92F16E) (Int64.fromInt32s 0xB1626B24 0x33DEE982) (Int64.fromInt32s 0x16F2F0AC 0x8AB407EC)
        , testOverflow (Int64.fromInt32s 0x88FC3FDB 0xCB3944F8) (Int64.fromInt32s 0x40AE04CA 0xFEA203CA) (Int64.fromInt32s 0x484E3B10 0xCC97412E)
        , testOverflow (Int64.fromInt32s 0xEF669F0A 0xCC527290) (Int64.fromInt32s 0x1F410834 0x176F08EA) (Int64.fromInt32s 0xD02596D6 0xB4E369A6)
        , testOverflow (Int64.fromInt32s 0xF98206F6 0x25C0B27C) (Int64.fromInt32s 0xFAC141CB 0x56CFE131) (Int64.fromInt32s 0xFEC0C52A 0xCEF0D14B)
        , testOverflow (Int64.fromInt32s 0x118748A1 0xC178A2CC) (Int64.fromInt32s 0xF9733B01 0x46ED023B) (Int64.fromInt32s 0x18140DA0 0x7A8BA091)
        , testOverflow (Int64.fromInt32s 0xBB74B184 0x6E300DD2) (Int64.fromInt32s 0x8251A5C0 0xDA97C10F) (Int64.fromInt32s 0x39230BC3 0x93984CC3)
        , testOverflow (Int64.fromInt32s 0x04648B89 0x635967B2) (Int64.fromInt32s 0x0C69D6C9 0x16453FD9) (Int64.fromInt32s 0xF7FAB4C0 0x4D1427D9)
        , testOverflow (Int64.fromInt32s 0xF6FCB61A 0x1762604C) (Int64.fromInt32s 0x52DBCF02 0x64812E96) (Int64.fromInt32s 0xA420E717 0xB2E131B6)
        , testOverflow (Int64.fromInt32s 0xC6328BF5 0x417484F2) (Int64.fromInt32s 0xE163D3E5 0x7D6C6FFD) (Int64.fromInt32s 0xE4CEB80F 0xC40814F5)
        , testOverflow (Int64.fromInt32s 0x88F2B9DA 0x1B3A97DB) (Int64.fromInt32s 0x851A10F9 0xB9C5ECF3) (Int64.fromInt32s 0x03D8A8E0 0x6174AAE8)
        , testOverflow (Int64.fromInt32s 0xB437297B 0xD090ACA7) (Int64.fromInt32s 0x367865DD 0xD59F77D7) (Int64.fromInt32s 0x7DBEC39D 0xFAF134D0)
        , testOverflow (Int64.fromInt32s 0x9C502BE1 0xD1E0A271) (Int64.fromInt32s 0x9FBEC010 0x1044BD11) (Int64.fromInt32s 0xFC916BD1 0xC19BE560)
        , testOverflow (Int64.fromInt32s 0x07CE06AA 0x454A9782) (Int64.fromInt32s 0x0C513E5B 0x8DCF7B4E) (Int64.fromInt32s 0xFB7CC84E 0xB77B1C34)
        , testOverflow (Int64.fromInt32s 0x3F5B9404 0xB0E59A8A) (Int64.fromInt32s 0x1B31D018 0x60A8668C) (Int64.fromInt32s 0x2429C3EC 0x503D33FE)
        , testOverflow (Int64.fromInt32s 0xECB6D283 0x2F5873AA) (Int64.fromInt32s 0xC6C92968 0xE9CEA689) (Int64.fromInt32s 0x25EDA91A 0x4589CD21)
        , testOverflow (Int64.fromInt32s 0xAD064390 0x18D93BF2) (Int64.fromInt32s 0x969A39AC 0x3732E0E2) (Int64.fromInt32s 0x166C09E3 0xE1A65B10)
        , testOverflow (Int64.fromInt32s 0x4AE71006 0x8B4D4B16) (Int64.fromInt32s 0x2129BD6C 0xA5705232) (Int64.fromInt32s 0x29BD5299 0xE5DCF8E4)
        , testOverflow (Int64.fromInt32s 0x676DCA7F 0x252816B8) (Int64.fromInt32s 0x0058E1B8 0xE5E44934) (Int64.fromInt32s 0x6714E8C6 0x3F43CD84)
        , testOverflow (Int64.fromInt32s 0x0D8E7777 0x187E3B7C) (Int64.fromInt32s 0xCC7D46A7 0x6337FA6C) (Int64.fromInt32s 0x411130CF 0xB5464110)
        , testOverflow (Int64.fromInt32s 0x148BCE63 0x0AE40DDA) (Int64.fromInt32s 0x33DAC0E8 0x98BFC2F1) (Int64.fromInt32s 0xE0B10D7A 0x72244AE9)
        , testOverflow (Int64.fromInt32s 0xC0AE02A4 0xF32D2F27) (Int64.fromInt32s 0xBD851C04 0x751A4F38) (Int64.fromInt32s 0x0328E6A0 0x7E12DFEF)
        , testOverflow (Int64.fromInt32s 0x5D0A6155 0x76B83D3A) (Int64.fromInt32s 0x573FDD4D 0x94611D74) (Int64.fromInt32s 0x05CA8407 0xE2571FC6)
        , testOverflow (Int64.fromInt32s 0xE6E7A243 0x6BC39F24) (Int64.fromInt32s 0xB7D143B8 0xC575F0E2) (Int64.fromInt32s 0x2F165E8A 0xA64DAE42)
        , testOverflow (Int64.fromInt32s 0xFCEBE4F5 0x49D8B395) (Int64.fromInt32s 0xB3CE428D 0xA715B1F6) (Int64.fromInt32s 0x491DA267 0xA2C3019F)
        , testOverflow (Int64.fromInt32s 0x15D8A4D3 0x04C8F97C) (Int64.fromInt32s 0x8B209561 0xDAAAED16) (Int64.fromInt32s 0x8AB80F71 0x2A1E0C66)
        , testOverflow (Int64.fromInt32s 0xAFE942A1 0x73CC0757) (Int64.fromInt32s 0x2A081D48 0x1D591D4A) (Int64.fromInt32s 0x85E12559 0x5672EA0D)
        , testOverflow (Int64.fromInt32s 0xA8C14E10 0xAFA3BD21) (Int64.fromInt32s 0xDA4EA2C6 0x3241F6F9) (Int64.fromInt32s 0xCE72AB4A 0x7D61C628)
        , testOverflow (Int64.fromInt32s 0x03F4F5D1 0xEE39E505) (Int64.fromInt32s 0xBC3E1BCB 0x2F332CF5) (Int64.fromInt32s 0x47B6DA06 0xBF06B810)
        , testOverflow (Int64.fromInt32s 0x917801E9 0x3951FE64) (Int64.fromInt32s 0xCC7BEC98 0x55D2C20B) (Int64.fromInt32s 0xC4FC1550 0xE37F3C59)
        , testOverflow (Int64.fromInt32s 0x76959DF7 0x9B6020A9) (Int64.fromInt32s 0x78043B45 0xE57E7CAF) (Int64.fromInt32s 0xFE9162B1 0xB5E1A3FA)
        , testOverflow (Int64.fromInt32s 0xC1D53BB9 0x57D6D0C2) (Int64.fromInt32s 0xDB8611D1 0x9F5A87BF) (Int64.fromInt32s 0xE64F29E7 0xB87C4903)
        , testOverflow (Int64.fromInt32s 0x7FE4B66F 0xD77D12F0) (Int64.fromInt32s 0xFC9962D5 0x1A2E45B9) (Int64.fromInt32s 0x834B539A 0xBD4ECD37)
        ]


addGenerated =
    let
        testOverflow a b result =
            test (Debug.toString a ++ " + " ++ Debug.toString b ++ " =? " ++ Debug.toString result) <|
                \_ ->
                    Int64.add a b
                        |> Int64.toHex
                        |> Expect.equal (result |> Int64.toHex)
    in
    describe "generated tests (using rust)"
        [ testOverflow (Int64.fromInt32s 0x4A0A5292 0x77AAB70A) (Int64.fromInt32s 0xD74FD8D9 0x4A2F6A7B) (Int64.fromInt32s 0x215A2B6B 0xC1DA2185)
        , testOverflow (Int64.fromInt32s 0x96E1A444 0x9F0A4518) (Int64.fromInt32s 0x07C0AF7F 0xB6961A6F) (Int64.fromInt32s 0x9EA253C4 0x55A05F87)
        , testOverflow (Int64.fromInt32s 0x333F87AE 0x1D54CA22) (Int64.fromInt32s 0x525B0B22 0x9C5A9FCD) (Int64.fromInt32s 0x859A92D0 0xB9AF69EF)
        , testOverflow (Int64.fromInt32s 0x7FF0EA32 0x5485065E) (Int64.fromInt32s 0x3EFB96AE 0xADADD865) (Int64.fromInt32s 0xBEEC80E1 0x0232DEC3)
        , testOverflow (Int64.fromInt32s 0x5E25E2DA 0xCC312BEA) (Int64.fromInt32s 0x72CEF151 0x9D2A4135) (Int64.fromInt32s 0xD0F4D42C 0x695B6D1F)
        , testOverflow (Int64.fromInt32s 0x71CC96EE 0x9582B911) (Int64.fromInt32s 0x7F1C45C6 0xA3ACD813) (Int64.fromInt32s 0xF0E8DCB5 0x392F9124)
        , testOverflow (Int64.fromInt32s 0x70846788 0xCDBDC2F9) (Int64.fromInt32s 0x1D1579D3 0x82C00626) (Int64.fromInt32s 0x8D99E15C 0x507DC91F)
        , testOverflow (Int64.fromInt32s 0x9C1F6061 0x29DE9C32) (Int64.fromInt32s 0xDE7A663D 0xA482214B) (Int64.fromInt32s 0x7A99C69E 0xCE60BD7D)
        , testOverflow (Int64.fromInt32s 0x28CFBDCD 0xD0061B05) (Int64.fromInt32s 0xC985D034 0x9EEACAB6) (Int64.fromInt32s 0xF2558E02 0x6EF0E5BB)
        , testOverflow (Int64.fromInt32s 0x43DDDFA4 0x98C4FBC3) (Int64.fromInt32s 0x4B2CA17F 0xF3B03A64) (Int64.fromInt32s 0x8F0A8124 0x8C753627)
        , testOverflow (Int64.fromInt32s 0xE75B0D7C 0xB005C61C) (Int64.fromInt32s 0xE156C731 0xB535F953) (Int64.fromInt32s 0xC8B1D4AE 0x653BBF6F)
        , testOverflow (Int64.fromInt32s 0x423E62E8 0x4EC36B11) (Int64.fromInt32s 0x2B5C182F 0x622A20B6) (Int64.fromInt32s 0x6D9A7B17 0xB0ED8BC7)
        , testOverflow (Int64.fromInt32s 0x06F9615E 0x125FDCDF) (Int64.fromInt32s 0x6ACF9DF5 0x640704FD) (Int64.fromInt32s 0x71C8FF53 0x7666E1DC)
        , testOverflow (Int64.fromInt32s 0x1F1DD3AB 0x97FD9B3F) (Int64.fromInt32s 0xE2CF4144 0xE4158BB5) (Int64.fromInt32s 0x01ED14F0 0x7C1326F4)
        , testOverflow (Int64.fromInt32s 0xE208AC7D 0xF9529F95) (Int64.fromInt32s 0x25968C99 0x144656C0) (Int64.fromInt32s 0x079F3917 0x0D98F655)
        , testOverflow (Int64.fromInt32s 0xF46D3E71 0xF4237792) (Int64.fromInt32s 0x37DB1AB6 0x7510677F) (Int64.fromInt32s 0x2C485928 0x6933DF11)
        , testOverflow (Int64.fromInt32s 0x631664A0 0xB3E2BAB6) (Int64.fromInt32s 0x40A29398 0x3D459CCD) (Int64.fromInt32s 0xA3B8F838 0xF1285783)
        , testOverflow (Int64.fromInt32s 0xA0E1AA20 0x64220537) (Int64.fromInt32s 0xCCC68363 0x2A113043) (Int64.fromInt32s 0x6DA82D83 0x8E33357A)
        , testOverflow (Int64.fromInt32s 0xAA7886B6 0x3D6714E8) (Int64.fromInt32s 0x0836D58F 0x4AA51C9F) (Int64.fromInt32s 0xB2AF5C45 0x880C3187)
        , testOverflow (Int64.fromInt32s 0x51154D2C 0xD947D39B) (Int64.fromInt32s 0x2D9881BA 0x12001A2F) (Int64.fromInt32s 0x7EADCEE6 0xEB47EDCA)
        , testOverflow (Int64.fromInt32s 0x3FEA3E55 0x2B75FCD3) (Int64.fromInt32s 0xABC8B172 0xA13DB255) (Int64.fromInt32s 0xEBB2EFC7 0xCCB3AF28)
        , testOverflow (Int64.fromInt32s 0x651CBE15 0x6CCFEF0B) (Int64.fromInt32s 0xEAA06CDD 0x1AA30BC8) (Int64.fromInt32s 0x4FBD2AF2 0x8772FAD3)
        , testOverflow (Int64.fromInt32s 0xE0E7996C 0xC90C7D37) (Int64.fromInt32s 0x529F5887 0xBDDA35AA) (Int64.fromInt32s 0x3386F1F4 0x86E6B2E1)
        , testOverflow (Int64.fromInt32s 0x0358E0AD 0xC1CD9C38) (Int64.fromInt32s 0x9AF172F7 0x97647227) (Int64.fromInt32s 0x9E4A53A5 0x59320E5F)
        , testOverflow (Int64.fromInt32s 0x2208D47E 0xFE98FF9B) (Int64.fromInt32s 0x8762D593 0x6381FCBD) (Int64.fromInt32s 0xA96BAA12 0x621AFC58)
        , testOverflow (Int64.fromInt32s 0x3876533F 0x6BFBC27E) (Int64.fromInt32s 0x8DFE5D83 0x32DF2A6A) (Int64.fromInt32s 0xC674B0C2 0x9EDAECE8)
        , testOverflow (Int64.fromInt32s 0x5AD00C02 0x27D5C135) (Int64.fromInt32s 0x3D2814D8 0xAFA517F8) (Int64.fromInt32s 0x97F820DA 0xD77AD92D)
        , testOverflow (Int64.fromInt32s 0x2543D787 0xFBDC5570) (Int64.fromInt32s 0xEFCF8DE0 0x746E33CB) (Int64.fromInt32s 0x15136568 0x704A893B)
        , testOverflow (Int64.fromInt32s 0xE2FA93BB 0xD09C7B92) (Int64.fromInt32s 0x615C1FFE 0x734C460B) (Int64.fromInt32s 0x4456B3BA 0x43E8C19D)
        , testOverflow (Int64.fromInt32s 0x82F2B8A6 0x4F97C81D) (Int64.fromInt32s 0xB8B4E89A 0xEEBC7BE8) (Int64.fromInt32s 0x3BA7A141 0x3E544405)
        , testOverflow (Int64.fromInt32s 0x710F7AB3 0x6750B1DB) (Int64.fromInt32s 0x5D9DF6F9 0x61A55AFF) (Int64.fromInt32s 0xCEAD71AC 0xC8F60CDA)
        , testOverflow (Int64.fromInt32s 0xC3D97C7F 0x2FC0D60D) (Int64.fromInt32s 0x42BC0C7A 0x3ACC02E5) (Int64.fromInt32s 0x069588F9 0x6A8CD8F2)
        , testOverflow (Int64.fromInt32s 0x11478882 0x6506F13D) (Int64.fromInt32s 0x72421E70 0x8DA93FC9) (Int64.fromInt32s 0x8389A6F2 0xF2B03106)
        , testOverflow (Int64.fromInt32s 0x8B27CAD7 0xF9485BCD) (Int64.fromInt32s 0xE72F0F35 0x686A3030) (Int64.fromInt32s 0x7256DA0D 0x61B28BFD)
        , testOverflow (Int64.fromInt32s 0xE7CBFF8E 0xB3FF5E4C) (Int64.fromInt32s 0x7242A252 0x2D4C4C5F) (Int64.fromInt32s 0x5A0EA1E0 0xE14BAAAB)
        , testOverflow (Int64.fromInt32s 0xF1E85F3C 0x990206B9) (Int64.fromInt32s 0x8292E126 0x07F10E62) (Int64.fromInt32s 0x747B4062 0xA0F3151B)
        , testOverflow (Int64.fromInt32s 0xFBBA0112 0x6CD58934) (Int64.fromInt32s 0x87689699 0xD483781D) (Int64.fromInt32s 0x832297AC 0x41590151)
        , testOverflow (Int64.fromInt32s 0xA6E4AE85 0xCCE781BB) (Int64.fromInt32s 0x2EC020C2 0xDD18E2A5) (Int64.fromInt32s 0xD5A4CF48 0xAA006460)
        , testOverflow (Int64.fromInt32s 0xA7292E78 0xA8D60379) (Int64.fromInt32s 0x53414919 0x2F27CAE7) (Int64.fromInt32s 0xFA6A7791 0xD7FDCE60)
        , testOverflow (Int64.fromInt32s 0x8CD64027 0x205E5BA4) (Int64.fromInt32s 0x44AB4E91 0xF1038E0E) (Int64.fromInt32s 0xD1818EB9 0x1161E9B2)
        , testOverflow (Int64.fromInt32s 0x9A8787CC 0x4AB5ED40) (Int64.fromInt32s 0xA955541A 0x7854CAB9) (Int64.fromInt32s 0x43DCDBE6 0xC30AB7F9)
        , testOverflow (Int64.fromInt32s 0xF2A1111C 0x867F2BA7) (Int64.fromInt32s 0x1FC8D36F 0xCBA38516) (Int64.fromInt32s 0x1269E48C 0x5222B0BD)
        , testOverflow (Int64.fromInt32s 0xC14FB395 0x3138D1F1) (Int64.fromInt32s 0xDEBD64D3 0x72106F1C) (Int64.fromInt32s 0xA00D1868 0xA349410D)
        , testOverflow (Int64.fromInt32s 0x9DA6A6F0 0x21B1042D) (Int64.fromInt32s 0x9683FDC0 0xA39EE4C9) (Int64.fromInt32s 0x342AA4B0 0xC54FE8F6)
        , testOverflow (Int64.fromInt32s 0x81C32507 0xD24B5750) (Int64.fromInt32s 0x969416EE 0xBD6CE70E) (Int64.fromInt32s 0x18573BF6 0x8FB83E5E)
        , testOverflow (Int64.fromInt32s 0x74685AFE 0x025A1A79) (Int64.fromInt32s 0xE73F8F5B 0x11769E0A) (Int64.fromInt32s 0x5BA7EA59 0x13D0B883)
        , testOverflow (Int64.fromInt32s 0x57C04DA2 0x43FEB5D1) (Int64.fromInt32s 0x535F8FC0 0xDE5021C4) (Int64.fromInt32s 0xAB1FDD63 0x224ED795)
        , testOverflow (Int64.fromInt32s 0x3C644F06 0xE67AD5FA) (Int64.fromInt32s 0x91A95E03 0x3F4EFD7A) (Int64.fromInt32s 0xCE0DAD0A 0x25C9D374)
        , testOverflow (Int64.fromInt32s 0xE4ADEECD 0x1974FF01) (Int64.fromInt32s 0x6F06D071 0x699FB197) (Int64.fromInt32s 0x53B4BF3E 0x8314B098)
        , testOverflow (Int64.fromInt32s 0xC6BDF25D 0x97C50553) (Int64.fromInt32s 0x8DF64FA9 0x75A0522F) (Int64.fromInt32s 0x54B44207 0x0D655782)
        , testOverflow (Int64.fromInt32s 0x0F7F4C4F 0x531906AF) (Int64.fromInt32s 0x674BB4B5 0x77D030F6) (Int64.fromInt32s 0x76CB0104 0xCAE937A5)
        , testOverflow (Int64.fromInt32s 0x52491445 0xFA971875) (Int64.fromInt32s 0xAA355F8E 0x1B533594) (Int64.fromInt32s 0xFC7E73D4 0x15EA4E09)
        , testOverflow (Int64.fromInt32s 0x7139B7F1 0xDDC66D92) (Int64.fromInt32s 0x99FDA06F 0x567989DF) (Int64.fromInt32s 0x0B375861 0x343FF771)
        , testOverflow (Int64.fromInt32s 0x253AC6E9 0xE666F6ED) (Int64.fromInt32s 0x27A5B13F 0xA88DBFAF) (Int64.fromInt32s 0x4CE07829 0x8EF4B69C)
        , testOverflow (Int64.fromInt32s 0x9FAB5A18 0x33D534AA) (Int64.fromInt32s 0xE2044054 0x42E9F41F) (Int64.fromInt32s 0x81AF9A6C 0x76BF28C9)
        , testOverflow (Int64.fromInt32s 0x9B428D21 0xF9C9BF53) (Int64.fromInt32s 0xBE015A8F 0xFF0966C8) (Int64.fromInt32s 0x5943E7B1 0xF8D3261B)
        , testOverflow (Int64.fromInt32s 0xA60DDB77 0x58B733E3) (Int64.fromInt32s 0x60A2033C 0xEB0C3E16) (Int64.fromInt32s 0x06AFDEB4 0x43C371F9)
        , testOverflow (Int64.fromInt32s 0xFF960385 0xD9E73726) (Int64.fromInt32s 0x1A36A4CF 0x485789D3) (Int64.fromInt32s 0x19CCA855 0x223EC0F9)
        , testOverflow (Int64.fromInt32s 0xF85B8870 0x4A34B6CC) (Int64.fromInt32s 0x44E4DB7E 0xB1477373) (Int64.fromInt32s 0x3D4063EE 0xFB7C2A3F)
        , testOverflow (Int64.fromInt32s 0x34CC386A 0x26A47DAB) (Int64.fromInt32s 0xE9ECEBB9 0xBBEAC793) (Int64.fromInt32s 0x1EB92423 0xE28F453E)
        , testOverflow (Int64.fromInt32s 0x9514F607 0xA508570A) (Int64.fromInt32s 0xC96C1138 0x59F6871E) (Int64.fromInt32s 0x5E81073F 0xFEFEDE28)
        , testOverflow (Int64.fromInt32s 0xC82FB71F 0x1351CF9B) (Int64.fromInt32s 0x535E7172 0x91DF2C08) (Int64.fromInt32s 0x1B8E2891 0xA530FBA3)
        , testOverflow (Int64.fromInt32s 0x34699EE5 0x331FA079) (Int64.fromInt32s 0x0B27F145 0xDB8C3080) (Int64.fromInt32s 0x3F91902B 0x0EABD0F9)
        , testOverflow (Int64.fromInt32s 0xDDB93778 0x1E08C3CF) (Int64.fromInt32s 0x5C15ABA0 0x0F2FF170) (Int64.fromInt32s 0x39CEE318 0x2D38B53F)
        , testOverflow (Int64.fromInt32s 0xB49AB6B6 0x804328A5) (Int64.fromInt32s 0xD61BCE92 0xAA83D767) (Int64.fromInt32s 0x8AB68549 0x2AC7000C)
        , testOverflow (Int64.fromInt32s 0x46BA3510 0x93632BCD) (Int64.fromInt32s 0x9F3C4962 0x26F13FF6) (Int64.fromInt32s 0xE5F67E72 0xBA546BC3)
        , testOverflow (Int64.fromInt32s 0xD9A3AA27 0x15CF807E) (Int64.fromInt32s 0x62C8A5FB 0x2562144B) (Int64.fromInt32s 0x3C6C5022 0x3B3194C9)
        , testOverflow (Int64.fromInt32s 0xEA07D632 0x983678E6) (Int64.fromInt32s 0x81415F3A 0xF3184266) (Int64.fromInt32s 0x6B49356D 0x8B4EBB4C)
        , testOverflow (Int64.fromInt32s 0xF53B9380 0xD6D19AE5) (Int64.fromInt32s 0x79A2DF40 0x1EEF8098) (Int64.fromInt32s 0x6EDE72C0 0xF5C11B7D)
        , testOverflow (Int64.fromInt32s 0x4B1C73EA 0x9074390F) (Int64.fromInt32s 0x5979B13B 0xDFEBDFEF) (Int64.fromInt32s 0xA4962526 0x706018FE)
        , testOverflow (Int64.fromInt32s 0x8A5BBA88 0x371B5ED2) (Int64.fromInt32s 0x48A8B73D 0x4203DFB4) (Int64.fromInt32s 0xD30471C5 0x791F3E86)
        , testOverflow (Int64.fromInt32s 0xA5E22441 0x26539203) (Int64.fromInt32s 0x83472CAD 0xEDE2E0C0) (Int64.fromInt32s 0x292950EF 0x143672C3)
        , testOverflow (Int64.fromInt32s 0xD2CF83AB 0x4F0D744E) (Int64.fromInt32s 0xC727DD79 0xD65FB6E6) (Int64.fromInt32s 0x99F76125 0x256D2B34)
        , testOverflow (Int64.fromInt32s 0xA29CECA6 0x37E4ED2B) (Int64.fromInt32s 0x248F6E87 0x2D33B97C) (Int64.fromInt32s 0xC72C5B2D 0x6518A6A7)
        , testOverflow (Int64.fromInt32s 0xB6319CB0 0xED11F078) (Int64.fromInt32s 0xDE5B8997 0xC365808B) (Int64.fromInt32s 0x948D2648 0xB0777103)
        , testOverflow (Int64.fromInt32s 0x4D1DE45E 0x26C41113) (Int64.fromInt32s 0xAB78F2B6 0x84064DFA) (Int64.fromInt32s 0xF896D714 0xAACA5F0D)
        , testOverflow (Int64.fromInt32s 0x0285A546 0x3AD1F24F) (Int64.fromInt32s 0xBF7828AF 0x558A70C4) (Int64.fromInt32s 0xC1FDCDF5 0x905C6313)
        , testOverflow (Int64.fromInt32s 0x7E802A57 0x006F32BA) (Int64.fromInt32s 0x814328FA 0x34276FC7) (Int64.fromInt32s 0xFFC35351 0x3496A281)
        , testOverflow (Int64.fromInt32s 0x690360FC 0xD55DA7C3) (Int64.fromInt32s 0xB8BB0C60 0x670BC757) (Int64.fromInt32s 0x21BE6D5D 0x3C696F1A)
        , testOverflow (Int64.fromInt32s 0xAB1F72E0 0x3855F12D) (Int64.fromInt32s 0x6275A44B 0x2074F56A) (Int64.fromInt32s 0x0D95172B 0x58CAE697)
        , testOverflow (Int64.fromInt32s 0x77C213F5 0xD6221F87) (Int64.fromInt32s 0x9DD192D0 0xC10DFB84) (Int64.fromInt32s 0x1593A6C6 0x97301B0B)
        , testOverflow (Int64.fromInt32s 0xF2FADB27 0xC86820DA) (Int64.fromInt32s 0xF1FEBDCD 0xEC948585) (Int64.fromInt32s 0xE4F998F5 0xB4FCA65F)
        , testOverflow (Int64.fromInt32s 0xE9C55A8C 0x73CD9100) (Int64.fromInt32s 0xA2B12DF9 0x4C32E046) (Int64.fromInt32s 0x8C768885 0xC0007146)
        , testOverflow (Int64.fromInt32s 0x396D8C94 0xCDE99F10) (Int64.fromInt32s 0xA64D900A 0x2D888BEE) (Int64.fromInt32s 0xDFBB1C9E 0xFB722AFE)
        , testOverflow (Int64.fromInt32s 0x4237D90C 0xAAD55A0C) (Int64.fromInt32s 0xD27DB34B 0x69DC17DA) (Int64.fromInt32s 0x14B58C58 0x14B171E6)
        , testOverflow (Int64.fromInt32s 0x81D98BEF 0xB42A923E) (Int64.fromInt32s 0x12186777 0xDA0C8EDE) (Int64.fromInt32s 0x93F1F367 0x8E37211C)
        , testOverflow (Int64.fromInt32s 0x2F9985DB 0xAC25C758) (Int64.fromInt32s 0x956C018D 0xF515BF8A) (Int64.fromInt32s 0xC5058769 0xA13B86E2)
        , testOverflow (Int64.fromInt32s 0xEF655D67 0x1FBA0BD2) (Int64.fromInt32s 0x9127E0AD 0xDD5FCA9C) (Int64.fromInt32s 0x808D3E14 0xFD19D66E)
        , testOverflow (Int64.fromInt32s 0x203396B7 0x2B145C2A) (Int64.fromInt32s 0xB221F40C 0xA0F21EF7) (Int64.fromInt32s 0xD2558AC3 0xCC067B21)
        , testOverflow (Int64.fromInt32s 0x68D5E744 0xA41BEAE3) (Int64.fromInt32s 0xE883B7BD 0x71442966) (Int64.fromInt32s 0x51599F02 0x15601449)
        , testOverflow (Int64.fromInt32s 0x7898E3A2 0x6F2F04AA) (Int64.fromInt32s 0x894351A6 0x168C4C09) (Int64.fromInt32s 0x01DC3548 0x85BB50B3)
        , testOverflow (Int64.fromInt32s 0x68A5CAB0 0xC78D0DDC) (Int64.fromInt32s 0x749904D8 0x92DB0412) (Int64.fromInt32s 0xDD3ECF89 0x5A6811EE)
        , testOverflow (Int64.fromInt32s 0x044B2246 0x7EA02E54) (Int64.fromInt32s 0xF9289170 0xD732E4E8) (Int64.fromInt32s 0xFD73B3B7 0x55D3133C)
        , testOverflow (Int64.fromInt32s 0x39A02A3E 0xCB4730B4) (Int64.fromInt32s 0xE1EE2B3B 0x839F85A1) (Int64.fromInt32s 0x1B8E557A 0x4EE6B655)
        , testOverflow (Int64.fromInt32s 0x063CEC85 0xD7193DD7) (Int64.fromInt32s 0x5E36CF29 0x2820496F) (Int64.fromInt32s 0x6473BBAE 0xFF398746)
        , testOverflow (Int64.fromInt32s 0xB1F6A364 0x669918D7) (Int64.fromInt32s 0x32EAAFEC 0x2012295A) (Int64.fromInt32s 0xE4E15350 0x86AB4231)
        , testOverflow (Int64.fromInt32s 0x570A3A0A 0x5A9C8069) (Int64.fromInt32s 0x89679393 0x8696540B) (Int64.fromInt32s 0xE071CD9D 0xE132D474)
        , testOverflow (Int64.fromInt32s 0x23718369 0xF62FDAE0) (Int64.fromInt32s 0x72C195F6 0x5E3D12C4) (Int64.fromInt32s 0x96331960 0x546CEDA4)
        , testOverflow (Int64.fromInt32s 0xE5CBFD35 0xB8889B3B) (Int64.fromInt32s 0xA24AB66B 0x504BFA87) (Int64.fromInt32s 0x8816B3A1 0x08D495C2)
        , testOverflow (Int64.fromInt32s 0x05BBB850 0xE9A2125E) (Int64.fromInt32s 0x7C4E496B 0xEF0D58EF) (Int64.fromInt32s 0x820A01BC 0xD8AF6B4D)
        , testOverflow (Int64.fromInt32s 0x3E15CE12 0x5F3406BC) (Int64.fromInt32s 0x1DFD50BF 0xCB6802D3) (Int64.fromInt32s 0x5C131ED2 0x2A9C098F)
        , testOverflow (Int64.fromInt32s 0x845B25CE 0x0432941A) (Int64.fromInt32s 0xCF4D1467 0xAE2B97FB) (Int64.fromInt32s 0x53A83A35 0xB25E2C15)
        , testOverflow (Int64.fromInt32s 0x1BB512CC 0xE89A47B3) (Int64.fromInt32s 0xB586CC7B 0x4D83A334) (Int64.fromInt32s 0xD13BDF48 0x361DEAE7)
        , testOverflow (Int64.fromInt32s 0x3305D62C 0x282B2206) (Int64.fromInt32s 0x8314DC76 0x0F43D181) (Int64.fromInt32s 0xB61AB2A2 0x376EF387)
        , testOverflow (Int64.fromInt32s 0xCFE2CD34 0xF87B7FB1) (Int64.fromInt32s 0x8B4A16FB 0xDE6DAFDB) (Int64.fromInt32s 0x5B2CE430 0xD6E92F8C)
        , testOverflow (Int64.fromInt32s 0x80B240A3 0x39601A7B) (Int64.fromInt32s 0xC0AA505A 0x0194BCEE) (Int64.fromInt32s 0x415C90FD 0x3AF4D769)
        , testOverflow (Int64.fromInt32s 0x7193DD58 0xD780F15D) (Int64.fromInt32s 0x5DDECAFF 0x3E2F9A8A) (Int64.fromInt32s 0xCF72A858 0x15B08BE7)
        , testOverflow (Int64.fromInt32s 0x100B26D4 0x8CDEC3CB) (Int64.fromInt32s 0x56324679 0x3827AFDE) (Int64.fromInt32s 0x663D6D4D 0xC50673A9)
        , testOverflow (Int64.fromInt32s 0xB52FE20C 0xC14D29E8) (Int64.fromInt32s 0x4BD59D9F 0x97447D87) (Int64.fromInt32s 0x01057FAC 0x5891A76F)
        , testOverflow (Int64.fromInt32s 0x0136BECF 0x9DC53FD8) (Int64.fromInt32s 0xFB0D6489 0xE90C3B3E) (Int64.fromInt32s 0xFC442359 0x86D17B16)
        ]


shiftRightZfBy =
    describe "shiftRightZfBy"
        [ test "works for n < 32" <|
            \_ ->
                Int64.fromInt32s 0xCAFEBABE 0xFFFFFFFF
                    |> Int64.shiftRightZfBy 28
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0x0C 0xAFEBABEF |> Int64.toHex)
        , test "works for n == 32" <|
            \_ ->
                Int64.fromInt32s 0xCAFEBABE 0xFFFFFFFF
                    |> Int64.shiftRightZfBy 32
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0 0xCAFEBABE |> Int64.toHex)
        , test "works for n > 32" <|
            \_ ->
                Int64.fromInt32s 0xCAFEBABE 0xFFFFFFFF
                    |> Int64.shiftRightZfBy 36
                    |> Int64.toHex
                    |> Expect.equal (Int64.fromInt32s 0 0x0CAFEBAB |> Int64.toHex)
        ]
