---
name: cbor-encoding-decoding
description: Guidance for encoding/decoding CBOR and interpreting cbor-hex/diag using cbor-diag.
---

# CBOR Encoding/Decoding Skill

Use this skill when you need to interpret, validate, or transform CBOR payloads and their
representations (hex, diagnostic notation, annotated hex). When cbor-hex is unclear, use the
locally installed `cbor-diag` tool for authoritative decoding.

## References
- RFC 7049 diagnostic notation: https://datatracker.ietf.org/doc/html/rfc7049#section-6

## Tooling
`cbor-diag` converts between bytes, hex, and diagnostic notation.

Supported inputs:
- `--from auto|hex|bytes|diag`
- `--to annotated|hex|bytes|diag|compact|debug`
- `--seq` for CBOR sequence (cbor-seq)

## Workflow
1. Identify the representation you have (raw bytes, hex string, or diagnostic notation).
2. If you have cbor-hex, decode it to diagnostic notation:
   - `cbor-diag --from hex --to diag`
3. If you have diagnostic notation and need canonical bytes/hex:
   - `cbor-diag --from diag --to hex`
4. If you need a readable byte-level explanation:
   - `cbor-diag --from hex --to annotated`
5. For multiple concatenated CBOR items, add `--seq`.

## Interpretation Notes
- Hex input ignores whitespace and `#` comments; keep payloads clean but comments are allowed.
- Diagnostic notation follows RFC 7049; arrays, maps, tags, and byte strings are expressed
  explicitly in diag form.
- Use `--to compact` if you need a minimal diagnostic string for round-tripping or tests.

## Quick Recognition (Appendix A/B)
Use these Appendix A examples to quickly identify common CBOR blobs before decoding:

Integers and simple values:
- `00` => `0`
- `01` => `1`
- `0a` => `10`
- `17` => `23`
- `18 18` => `24`
- `18 19` => `25`
- `18 64` => `100`
- `19 03 e8` => `1000`
- `18 ff` => `255`
- `19 ff ff` => `65535`
- `1a 00 0f 42 40` => `1000000`
- `1b 00 00 00 e8 d4 a5 10 00` => `1000000000000`
- `1b ff ff ff ff ff ff ff ff` => `18446744073709551615`
- `c2 49 01 00 00 00 00 00 00 00 00` => `18446744073709551616`
- `20` => `-1`
- `29` => `-10`
- `38 63` => `-100`
- `39 03 e7` => `-1000`
- `3b ff ff ff ff ff ff ff ff` => `-18446744073709551616`
- `c3 49 01 00 00 00 00 00 00 00 00` => `-18446744073709551617`
- `f4` => `false`
- `f5` => `true`
- `f6` => `null`
- `f7` => `undefined`
- `f0` => `simple(16)`
- `f8 18` => `simple(24)`
- `f8 ff` => `simple(255)`

Floating-point values:
- `f9 00 00` => `0.0`
- `f9 80 00` => `-0.0`
- `f9 3c 00` => `1.0`
- `f9 3e 00` => `1.5`
- `fb 3f f1 99 99 99 99 99 9a` => `1.1`
- `f9 7c 00` => `Infinity`
- `f9 7e 00` => `NaN`
- `f9 fc 00` => `-Infinity`

Strings and bytes:
- `60` => `""`
- `61 61` => `"a"`
- `64 49 45 54 46` => `"IETF"`
- `40` => `h''`
- `44 01 02 03 04` => `h'01020304'`

Arrays and maps:
- `80` => `[]`
- `83 01 02 03` => `[1, 2, 3]`
- `82 61 61 a1 61 62 61 63` => `["a", {"b": "c"}]`
- `98 19 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 18 18 19`
  => `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]`
- `a0` => `{}`
- `a2 01 02 03 04` => `{1: 2, 3: 4}`
- `a2 61 61 01 61 62 82 02 03` => `{"a": 1, "b": [2, 3]}`
- `a5 61 61 61 41 61 62 61 42 61 63 61 43 61 64 61 44 61 65 61 45`
  => `{"a": "A", "b": "B", "c": "C", "d": "D", "e": "E"}`

Indefinite-length items:
- `5f 42 01 02 43 03 04 05 ff` => `(_ h'0102', h'030405')`
- `7f 65 73 74 72 65 61 64 65 6d 69 6e 67 ff` => `(_ "strea", "ming")`
- `9f ff` => `[_ ]`
- `9f 01 82 02 03 9f 04 05 ff ff` => `[_ 1, [2, 3], [_ 4, 5]]`
- `bf 61 61 01 61 62 9f 02 03 ff ff` => `{_ "a": 1, "b": [_ 2, 3]}`
- `bf 63 46 75 6e f5 63 41 6d 74 21 ff` => `{_ "Fun": true, "Amt": -2}`

Tags:
- `c0 74 32 30 31 33 2d 30 33 2d 32 31 54 32 30 3a 30 34 3a 30 30 5a`
  => `0("2013-03-21T20:04:00Z")`
- `c1 1a 51 4b 67 b0` => `1(1363896240)`
- `c1 fb 41 d4 52 d9 ec 20 00 00` => `1(1363896240.5)`
- `c2 42 01 02` => `2(h'0102')`
- `c4 82 21 19 6a b3` => `4([-2, 27315])`
- `c5 82 20 03` => `5([-1, 3])`
- `d7 44 01 02 03 04` => `23(h'01020304')`
- `d8 18 45 64 49 45 54 46` => `24(h'6449455446')`
- `d8 20 76 68 74 74 70 3a 2f 2f 77 77 77 2e 65 78 61 6d 70 6c 65 2e 63 6f 6d`
  => `32("http://www.example.com")`

Appendix B notation cues:
- Byte strings use `h'...'` (hex) or `b64'...'` (base64).
- Tags are `tag(value)`.
- Indefinite-length items use `_`, e.g. `[_ 1, 2]` or `(_ "a", "b")`.
- Unnamed simple values use `simple(value)`.

Appendix B jump table clues (common prefixes):
- `0x00..0x17`: small unsigned ints (0..23)
- `0x18/19/1a/1b`: unsigned ints with 1/2/4/8 following bytes
- `0x20..0x37`: small negative ints (-1..-24)
- `0x38/39/3a/3b`: negative ints with 1/2/4/8 following bytes
- `0x40..0x57`: byte strings (0..23 bytes)
- `0x58/59/5a/5b`: byte strings with 1/2/4/8 length bytes
- `0x5f`: byte string (indefinite length, terminated by `ff`)
- `0x60..0x77`: text strings (0..23 bytes)
- `0x78/79/7a/7b`: text strings with 1/2/4/8 length bytes
- `0x7f`: text string (indefinite length, terminated by `ff`)
- `0x80..0x97`: arrays (0..23 items)
- `0x9f`: array (indefinite length, terminated by `ff`)
- `0xa0..0xb7`: maps (0..23 pairs)
- `0xbf`: map (indefinite length, terminated by `ff`)
- `0xc0..0xdb`: tags (major type 6)
- `0xf4..0xf7`: false/true/null/undefined
- `0xff`: break for indefinite-length items

Common tag IDs (major type 6):
- `0`: date/time string (RFC 3339)
- `1`: epoch-based date/time
- `2`: positive bignum (byte string)
- `3`: negative bignum (byte string)
- `4`: decimal fraction (array [exp, mantissa])
- `5`: bigfloat (array [exp, mantissa])
- `21/22/23`: expected base64url/base64/base16 conversion
- `24`: embedded CBOR data item (byte string)
- `32`: URI
- `33/34`: base64url/base64 text
- `35`: regular expression
- `36`: MIME message
- `55799`: self-describe CBOR

## Examples
Decode hex to diag:
```bash
cbor-diag --from hex --to diag <<< '83010203'
```
Expected output:
```text
[1, 2, 3]
```

Encode diag to hex:
```bash
cbor-diag --from diag --to hex <<< '["a", {"b": "c"}]'
```
Expected output:
```text
826161a161626163
```

Annotate hex for explanation:
```bash
cbor-diag --from hex --to annotated <<< 'a26161016162820203'
```
Expected output:
```text
a2       # map(2)
   61    #   text(1)
      61 #     "a"
   01    #   unsigned(1)
   61    #   text(1)
      62 #     "b"
   82    #   array(2)
      02 #     unsigned(2)
      03 #     unsigned(3)
```

## Common Pitfalls
- Confusing CBOR hex (encoded bytes) with a hex string contained inside CBOR. Decode to diag to
  confirm types.
- Treating concatenated CBOR items as a single item; use `--seq` if the input is a sequence.

## Additional Guidance
- Canonicalization: the same logical value can be encoded multiple ways; decoding to diag and
  re-encoding may not preserve original bytes if encoding indicators differed.
- Sequences: `--seq` only works with raw bytes input; avoid `--from` when using it.
- Tags: preserve `tag(value)` even when the semantic meaning is unknown; tags are optional hints.

Sequence example (raw bytes):
```bash
printf '\x01\x02\x03' | cbor-diag --to diag --seq
```
Expected output:
```text
1

2

3
```
