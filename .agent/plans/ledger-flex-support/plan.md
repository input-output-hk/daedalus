# Ledger Flex pairing implementation plan

## Evidence and technical context

Investigated checkout `22440487d` on 2026-10-21. The user reports a freshly
reset Flex, no export approval prompt, an error on the device, and a hanging
Daedalus UI. Host OS and exact device error text remain unconfirmed. Nano X
works in the reported comparison.

The reported Cardano app **7.3.0** is accepted by SDK 7.1.4. The app-8.x gap
below does not explain that reproduction. The user explicitly requested the
full SDK upgrade and confirmed Secure Element firmware upgraded to **1.7.0**.
The initial 5.8.3 report was a bootloader/microcontroller version. Do not claim
a root-cause fix until hardware validation succeeds.

Post-update hardware configuration confirmed by the user: Secure Element
**1.7.0**, Cardano app **8.0.9**. Physical pairing results remain pending.

The configured agentic-nix embedding search failed; the user authorized local
source inspection. Existing Spec Kit specification is retained in this directory.

- `getHardwareWalletChannel.ts` calls `getVersion`, then exports account
  `1852'/1815'/0'` and reads the serial number. The same UI state covers both
  export and serial retrieval.
- Published SDK 7.1.4 `dist/interactions/getVersion.js` accepts only app versions
  2.2 through 7.x. Daedalus checks only its own minimum version. A modern app can
  reach the export screen before the SDK rejects it without an approval prompt.
- Published SDK 8.1.0 (upstream revision
  `9ce065887bfca145f7a2f46df2ea41b1b47b9640`) retains the pairing API and routes
  key export to distinct v7/v8 implementations by app version. Its public type
  changes are additive for Daedalus's calls. Pin this version; validate the
  existing signing conversions because the SDK is shared with signing.
- Main-process logging JSON-serializes errors, losing non-enumerable `message`.
  Electron Error cloning also does not preserve custom status fields reliably.
  Explicit plain error records are needed at the Ledger pairing boundary.
- App v8 rejection is `0x6985`; legacy rejection is `0x6e09`. Existing renderer
  cancellation handling recognizes only the latter.

The app-version mismatch is a separate compatibility defect. Diagnostics and
physical retesting are still needed to diagnose the reported app-7.3.0 failure.

## Implementation

1. Pin Cardano Ledger SDK 8.1.0 and regenerate the Yarn lock through Nix.
2. Add a Ledger operation wrapper for version, key export, and serial reads.
   Log start/completion without return values so a stalled request is identifiable.
   Return successes unchanged. On failure log and throw an allowlisted plain
   record with operation, name, message, code and numeric status. Never copy
   transport objects, request data, response buffers, or key material.
   Normalize the SDK's numeric `code` to `statusCode` for existing renderer
   recovery, retaining `code` as well.
3. Record app version/compatibility in main logs. Preserve renderer cancellation
   behavior for both v7 and v8 rejection codes, including address verification.
4. Add Flex to model types/classification without claiming support for other
   new models or changing derivation, serial identity, or transaction formats.
   Use transport identifier `europa`, verified against pinned Ledger devices.
5. Add meaningful tests using the actual SDK with a fake APDU transport for v7
   and v8 export/serial, rejection/retry, and operation error serialization.

The SDK no longer exposes `utils.hex_to_buf`; import its validated converter
from `dist/utils/serialize` at the existing main/renderer call sites. Numeric
app-version fields now have explicit types at the IPC boundary. Rejection
constants come from `dist/errors/deviceStatusError`, not the package root.

## Validation and completion gates

Run focused Jest tests, existing Shelley Ledger conversions, TypeScript and
format checks in the project Nix shell. If a check fails, explain it and ask
before implementing a different approach. No commit or push is implied.

Physical validation remains required: record OS, Daedalus commit, firmware and
Cardano app versions; pair Flex, verify an address on-device, reconnect without
duplicate creation, reject/retry and disconnect/reconnect/retry; repeat pairing
on Nano X. Automated protocol tests cannot certify device approval or USB
behavior. Broader signing certification remains outside this milestone.

## Constraints review

No key derivation changes, secret handling, new transport, or Bluetooth support.
Nix supplies tools. Reuse the existing branch/spec; no feature number allocation
or worktree creation. No subagents. Preserve the existing generic UI text until
physical diagnostics establish any additional cause-specific guidance needed.

## Rebase onto `nodejs-deps-update`

The new base already uses SDK 8.0.0 and routes IPC through
`source/main/hardware/HardwareWalletService.ts`. Keep that service and its
connection generations, cancellation, and signing validation; apply the pairing
operation diagnostics inside its existing `withLedgerOperation` calls. Keep the
base's local validated hex converters and string-valued version IPC contract.
The dependency delta is now SDK 8.0.0 → 8.1.0; retain every other dependency
update from the base.

Adapt the diagnostics test to correlated IPC request/response envelopes. Preserve
archived 8.0.0 capability evidence; check installed 8.1.0 against the current
lock, SHA-1/SRI-verified package tarball, and source bytes instead of changing
archived hashes or certification claims. Regenerate ignored SCSS declarations
with `yarn compile` after rebasing before running TypeScript checks.
