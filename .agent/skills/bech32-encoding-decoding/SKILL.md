---
name: bech32-encoding-decoding
description: Encode and decode bech32 strings using the local bech32 CLI.
allowed-tools:
  - Bash(bech32:*)
  - Read
disable-model-invocation: false
user-invocable: true
---

# Bech32 Encoding/Decoding Skill

Use this skill to convert between hex/base58 inputs and bech32 strings, or to decode bech32 back to base16. The local `bech32` binary reads data from standard input and writes to standard output.

## When to use
- When you need to inspect or transform bech32 strings for debugging or tests
- When you need to encode raw bytes (hex/base58) to a bech32 prefix
- When you need to decode a bech32 string to base16

## Tooling
- `bech32 [PREFIX]` converts to/from bech32 strings
- Input is read from `stdin`
- Output is printed to `stdout`
- Supported input formats: Base16, Bech32, Base58

## Operating rules (must follow)
- Confirm the target human-readable prefix (HRP) before encoding
- Prefer piping input via `<<<` or `printf` so the payload is explicit
- For decoding, omit the prefix argument to ensure base16 output
- Never alter user data; report exact output from the tool

## Execution workflow

### Step 1: Identify the input type
- Base16 hex string (preferred for raw bytes)
- Base58 string
- Bech32 string

### Step 2: Encode to bech32
```bash
bech32 <prefix> <<< <hex-or-base58-input>
```

### Step 3: Decode from bech32 to base16
```bash
bech32 <<< <bech32-input>
```

## Examples

Encode hex to bech32:
```bash
bech32 addr <<< 71ab222c7f223c4c0e297d11fbf6e6a082eb41db5e8539fcfb822c76c3
```
Expected output:
```text
addr1wx4jytrlyg7ycr3f05glhahx5zpwkswmt6znnl8msgk8dsccv8766
```

Decode bech32 to hex:
```bash
bech32 <<< addr1wx4jytrlyg7ycr3f05glhahx5zpwkswmt6znnl8msgk8dsccv8766
```
Expected output:
```text
71ab222c7f223c4c0e297d11fbf6e6a082eb41db5e8539fcfb822c76c3
```

Convert from base58 to bech32:
```bash
bech32 base58_ <<< Ae2tdPwUPEYy
```
Expected output:
```text
base58_1p58rejhd9592uusa8pzj2
```

## Common pitfalls
- Using the wrong HRP when encoding; bech32 is prefix-sensitive
- Passing bech32 input with a prefix argument (this re-encodes instead of decoding)
- Including whitespace or newlines inside the input payload
- Prefixing hex input with `0x`; the tool expects raw hex only

## Error signals
- `Unable to detect input encoding` usually means the input contains a prefix (`0x`) or non-hex characters

## Diagnosis workflow (quick)
1. Confirm input type and remove any `0x` prefix for hex.
2. If encoding: verify the HRP you intend to use.
3. If decoding: ensure you omit the prefix argument.
4. Re-run with a minimal test case to validate the tool:
   - `bech32 base16_ <<< 706174617465`
   - `bech32 <<< base16_1wpshgct5v5r5mxh0`

## References
- `bech32 --help`
