# elm-int64

An efficient 64-bit integer with correct overflow.

Bitwise operators in javascript can only use 32 bits. Sometimes, external protocols use 64-bit integers. This package implementes such integers with "correct" overflow behavior.

This is a low-level package focussed on speed. The 64-bit integers are represented as a 2-tuple of 32-bit numbers.

```elm
import Int64 

Int64.add (Int64.fromInt 42) (Int64.fromInt 10)
    |> Int64.toUnsignedString
    --> "52"

Int64.subtract (Int64.fromInt 10) (Int64.fromInt 42)
    |> Int64.toSignedString
    --> "-32"

Int64.xor 
    (Int64.fromParts 0xDEADBEEF 0xBAAAAAAD) 
    (Int64.fromInt 42)
    |> Int64.toHex
    --> "deadbeefbaaaaa87"

```

## Performance 

Performance is roughly half for subtraction, addition and the bitwise operators. That is great considering int64 is really two 32-bit integers, so we're doing operations twice.


## Reliabiltiy

This package is extensively tested with fuzz tests. Nonetheless, the logic is extremely tricky, so there might still be bugs.
