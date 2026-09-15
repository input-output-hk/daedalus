# Task task-005: Compute CIP-14 asset fingerprints locally

## Task ID and Title

- ID: `task-005`
- Title: `Compute CIP-14 asset fingerprints locally`

## Why Chosen Now

`task-005` has no dependencies in the task graph. It is the first task of phase 2 and the only one
in that phase that is renderer-only: no cache, no SQLite, no IPC channel, no network call and no new
dependency.

It is load-bearing rather than an optimisation. `fingerprint` is today a field of the assets
endpoint response (`api/assets/types.ts:18`), and the asset pill renders it as the token's identity
whenever no name resolves (`components/assets/Asset.tsx:213-217`). `task-018` removes that endpoint.
Without a local computation the token row loses its primary identifier the moment the endpoint goes,
and `task-002` has already made rows survive with `fingerprint` undefined, so the gap is open on the
branch right now.

It is also the task that `task-010` depends on and that `task-016` needs in order to attach a
fingerprint to a merged row, so it unblocks both halves of the plan.

## Interaction Mode

- Mode: `agent_execution`

The acceptance criteria are eight published golden vectors asserted in a Jest spec and the four Nix
checks. Nothing needs a running node, a network fetch or an operator.

## Scope

- A new `source/renderer/app/utils/assetFingerprint.ts` exporting one function that takes a
  hex-encoded policy id and a hex-encoded asset name and returns the CIP-14 fingerprint.
- Input validation, so that a transposed or malformed argument is refused rather than hashed into a
  plausible-looking fingerprint for the wrong subject.
- A colocated `assetFingerprint.spec.ts` asserting all eight golden vectors from `cardano-wallet`
  plus the rejection cases.

Revertible on its own. Nothing imports the new module yet; `task-016` is where it is wired in.

## Non-Goals

- No call site. This task adds the function and its tests and changes no existing file. Attaching the
  fingerprint to a merged row is `task-016`'s, and replacing the `'unknown fingerprint'` literal at
  `utils/transactionsCsvGenerator.ts:191` goes with it.
- No new dependency and no change to `package.json`.
- No move of the computation into the main process. It is a pure function of data the renderer
  already has, it must work with a cold cache, and putting it behind IPC would add a round trip to
  render a row.
- No caching or memoisation. One blake2b digest over at most 60 bytes per rendered row is not a cost
  worth engineering against, and a cache would need an invalidation story for no measured benefit.

## Dependencies

- None in the task graph. `task-005` has `"dependencies": []`.
- Practical dependency: Nix, for the four checks.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the CIP-14 reference at `:149`,
  the local-computation decision at `:816`, and the testing strategy at `:1453`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-005` entry, plus
  `task-010` and `task-016`, which are its consumers.
- `cardano-wallet` at `3e623efdd3b2652288607536b58b3959717a9608`, working clone at
  `/home/adam/daedalus-master/cardano-wallet`, file
  `lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78`.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map.
- Skills: none apply. No message, no style, no store registration, no IPC channel.

## Live Repo Findings Verified For Planning

Verified at `0a043cea8` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The golden vectors, read from the clone rather than from the PRD.**
`lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78` holds exactly
eight `goldenTestCIP14` calls, each a base16 policy id, a base16 asset name and a bech32 fingerprint.
Three have an empty asset name, two have the six-byte name `504154415445`, two have a 28-byte name
and one has the 32-byte name of all zero bytes. The helper at `:80-90` shows the inputs are decoded
from hex before hashing, which is the detail a spec copying the hex strings verbatim could get
wrong.

**Both candidate hash implementations reproduce all eight.** Measured in this tree with the
repository's Node (v22.23.1), concatenating the hex-decoded policy id and the hex-decoded asset name,
taking blake2b with a 20-byte digest, and bech32-encoding the result with the human-readable part
`asset`:

- `blake2b(20).update(bytes).digest()` from the `blake2b` package: 8 of 8.
- `blakejs.blake2b(bytes, null, 20)`: 8 of 8.

**The packages are declared and pinned.** `package.json:207` is `"bech32": "2.0.0"` and
`package.json:210` is `"blake2b": "2.1.3"`, both exact rather than ranged, and both resolve in
`yarn.lock`. `package.json:211` is `"blakejs": "1.1.0"`.

**Which of the two to use, and the bundling question behind it.** The task graph names the `blake2b`
package. Today that package is imported only from main-process modules,
`source/main/governance/AnchorVerificationService.ts:1` and `source/main/utils/restoreKeystore.ts:2`,
while the renderer's existing blake2b usage goes through `blakejs`
(`utils/crypto.ts:3`, `api/utils/index.ts:2`, `utils/dataSerialization.ts:2`). So this is the first
renderer use of `blake2b`, and the question worth settling before choosing it is whether it survives
the renderer's webpack build. It does: `node_modules/blake2b-wasm/blake2b.js` is a single
twelve-kilobyte JavaScript file with the WebAssembly module embedded as a base64 literal and decoded
at runtime by a local `__toBinary` helper, so no wasm loader and no asset emission is involved, and
`node_modules/blake2b/index.js` carries a complete JavaScript implementation that it uses until the
WebAssembly module resolves. Under an ES import the binding is captured before that swap, so the
JavaScript path is what runs; it produced all eight vectors above. The task graph's choice is
therefore taken as written.

**Bech32 length.** `bech32@2.0.0` exports `{ bech32, bech32m }` and its `encode` takes an optional
limit defaulting to 90 characters. A CIP-14 fingerprint is `asset` plus a separator plus 32 data
characters plus a six-character checksum, 44 in total, so the default limit is never in play and no
limit argument is passed.

**Where it will be rendered.** `components/assets/Asset.tsx:213-217` renders
`fingerprint` directly when `small` is false and `ellipsis(fingerprint || '', ...)` otherwise, so an
absent fingerprint renders as empty rather than throwing. That is the current state on this branch
after `task-002`, and it is what this task closes once `task-016` supplies the value.

**Argument transposition is a live hazard in this codebase, not a hypothetical.** Two of the eight
golden vectors are the same pair of 28-byte values in both orders, `:65-68` and `:70-73`, and they
produce different fingerprints, both well-formed. One commit ago `task-004` deleted `getUniqueId`
precisely because it spelled a subject key in the opposite order to every live site. A function
whose two arguments are both hex strings, where swapping them yields a plausible wrong answer, needs
a shape check that catches the swap.

**Existing test coverage.** There is no `assetFingerprint.ts` and no spec for one.
`grep -rn "assetFingerprint" source tests storybook` returns nothing. `jest.config.js:156` picks up a
colocated `*.spec.ts` with no config change.

**A note on the Nix checks.** `perSystem/checks.nix:15-32` builds from `srcWithoutNix`, derived from
`inputs.self`, so both new files must be `git add`ed before any check can see them.

## Files Expected To Change

- `source/renderer/app/utils/assetFingerprint.ts` — new.
- `source/renderer/app/utils/assetFingerprint.spec.ts` — new.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-005` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-005*.md` — this plan and its two review logs.

No deviation from the task graph's `targetPaths`. Both are listed and nothing else is touched.

## Implementation Approach

1. **The function.**

   ```ts
   export const assetFingerprint = (policyId: string, assetName: string): string
   ```

   Decode both from hex, concatenate in that order, hash with `blake2b(20)`, and bech32-encode with
   the human-readable part `asset`. That is CIP-14 in four lines.

2. **Validation, and what it is for.** Both arguments must be well-formed hex: an even number of
   characters, drawn from `0-9a-fA-F`. `Buffer.from(x, 'hex')` stops at the first non-hex pair and
   truncates an odd-length input rather than throwing, which is the same trap `task-001` documented
   for asset names, and here it would yield a well-formed fingerprint for the wrong bytes.

   In addition, the policy id must be 28 bytes and the asset name at most 32 bytes. Both are
   consensus limits rather than preferences, and together they are what catches a transposed pair of
   arguments: an asset name in the policy-id position is rejected unless it happens to be exactly 28
   bytes. This is deliberately more than the task graph's "an odd-length or non-hex input is
   rejected", and the reason is under Live Repo Findings: two of the eight golden vectors are a
   transposed pair that both produce valid fingerprints.

3. **Rejection is a throw, not a null.** The declared return type in the task graph is `string`, and
   a function that sometimes returns `null` puts an empty identity on a token row and hides a broken
   contract. The inputs are the wallet API's own hex fields, so a value that is not hex is a contract
   violation rather than a user-facing condition. The rendering-resilience decision belongs at the
   call site, which is `task-016`; this is recorded in Risks and in the handoff so that task guards
   rather than discovers it.

4. **Each rule throws its own message.** A single generic error would leave both the spec and the
   caller unable to tell a malformed string from a transposed argument, and telling those apart is
   the entire reason the length rules exist. Four distinct messages: not hex, wrong policy id length,
   asset name too long, and the empty policy id folded into the length rule.

5. **Spec.** `assetFingerprint.spec.ts`, colocated, the eight vectors driven from a table so that
   each one names itself in the test output, plus the rejection cases.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes with all eight vectors asserted.** Run as
   `nix build '.#checks.x86_64-linux.jest' --no-link`. The eight are transcribed from the clone, not
   from the PRD, and the spec names the source file and the commit they were read at.
2. **No new entry in `package.json`.** Settled by `git diff -- package.json yarn.lock` being empty.

Four criteria this task adds to its own closure:

3. The empty asset name produces a fingerprint rather than throwing. It is three of the eight
   vectors, so this is asserted by them and not only by a separate case.
4. An odd-length input, a non-hex input, a policy id of the wrong length and a transposed pair are
   each rejected, with a case each.
5. `compile`, `lint` and `i18n` are green from `nix build`, not from host tooling.
6. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

Repository verification already done for planning is under Live Repo Findings. Execution
verification:

- The eight golden vectors, each asserted individually so a failure names which one:

  | Policy id | Asset name | Expected |
  |---|---|---|
  | `7eae28af…dcc373` | empty | `asset1rjklcrnsdzqp65wjgrg55sy9723kw09mlgvlc3` |
  | `7eae28af…dcc37e` | empty | `asset1nl0puwxmhas8fawxp8nx4e2q3wekg969n2auw3` |
  | `1e349c9b…1df209` | empty | `asset1uyuxku60yqe57nusqzjx38aan3f2wq6s93f6ea` |
  | `7eae28af…dcc373` | `504154415445` | `asset13n25uv0yaf5kus35fm2k86cqy60z58d9xmde92` |
  | `1e349c9b…1df209` | `504154415445` | `asset1hv4p5tv2a837mzqrst04d0dcptdjmluqvdx9k3` |
  | `1e349c9b…1df209` | `7eae28af…dcc373` | `asset1aqrdypg669jgazruv5ah07nuyqe0wxjhe2el6f` |
  | `7eae28af…dcc373` | `1e349c9b…1df209` | `asset17jd78wukhtrnmjh3fngzasxm8rck0l2r4hhyyt` |
  | `7eae28af…dcc373` | 32 zero bytes | `asset1pkpwyknlvul7az0xx8czhl60pyel45rpje4z8w` |

  The sixth and seventh are the transposed pair and are asserted to differ from each other, which is
  the property a spec that only checks eight equalities would not state.

- Rejection cases, driven explicitly rather than implied:
  - An odd-length policy id.
  - A policy id containing a non-hex character, using a string that `Buffer.from` would truncate to a
    valid prefix rather than reject.
  - An asset name containing a non-hex character.
  - A policy id that is not 28 bytes, asserted per boundary: 27 bytes and 29 bytes reject, 28 bytes
    accepts.
  - An asset name at its boundary: 32 bytes accepts, which is also the eighth golden vector, and 33
    bytes rejects.
  - The two arguments given in the wrong order, using a 32-byte name that cannot be a policy id.
  - An empty policy id.
  Each asserts that the call throws **with the message belonging to the rule that should have
  fired**, not merely that it throws. A bare `toThrow()` would pass if the wrong rule rejected the
  input, and distinguishing a malformed string from a transposed argument is the reason the length
  rules exist.

- Casing: the same vector asserted with an upper-case policy id, since hex from an external source
  is not guaranteed lower-case and `Buffer.from` accepts both.

Commands:

- `nix build '.#checks.x86_64-linux.jest' --no-link`, with both new files staged.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff -- package.json yarn.lock` must be empty.

If a check reports a substituted result rather than a local build, it is re-run with `--rebuild`.

## Risks and Open Questions

1. **The function throws on malformed input and nothing guards the call site yet.** Nothing calls it
   in this commit, so the risk is not live; it becomes live in `task-016`, which computes a
   fingerprint per rendered row. If a malformed subject ever reached it there, the throw would take
   the token list down rather than leaving one row without an identity. The guard belongs in
   `task-016` and is named in the handoff. The alternative, returning `null` here, was considered and
   rejected: it would hide a broken API contract behind an empty identity, and `Asset.tsx:216`
   already renders an absent fingerprint as empty, so a caller that wants that behaviour can produce
   it explicitly.
2. **The validation is stricter than the task graph's wording.** Two byte-length rules are added
   beyond hex shape. They encode consensus limits, not preferences, and they exist to catch a
   transposed argument pair, which the golden vectors themselves demonstrate produces a valid-looking
   wrong answer. If a future ledger change altered either limit this function would reject a legal
   subject, which is a loud failure rather than a silent wrong answer.
3. **The `blake2b` package swaps its own export once its WebAssembly module resolves.** Under an ES
   import the binding is captured first, so the JavaScript implementation is what runs. That is
   slower and identical in output; all eight vectors were produced by it. Named because a reader
   seeing `WASM_LOADED` in that package might assume otherwise.
4. **This is the first renderer use of the `blake2b` package.** The bundling question is settled
   under Live Repo Findings, but it cannot be *demonstrated* until something imports the module,
   which is `task-016`. `blakejs`, already in the renderer bundle at three sites, reproduces the same
   eight vectors and is the fallback if a bundling problem ever appears. Named so that fallback is a
   decision already reasoned about rather than a discovery.
5. No open questions for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-005`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-005-plan-review.md` and `task-005-impl-review.md` as the cycle requires.
- No PRD change. Its CIP-14 sections describe exactly what is built.
- One task-graph inconsistency recorded rather than edited from inside this task: `task-016`'s
  implementation notes require the locally computed fingerprint from `task-005`, but `task-005` is
  not in `task-016`'s `dependencies`, which lists `task-001`, `task-002`, `task-014` and `task-015`.
  First recorded under `task-002` and repeated here because this is the task it points at.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-005-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-005-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-005` complete. Reviewed and approved in `task-005-impl-review.md`.
- `assetFingerprint(policyId, assetName)` reproduces all eight published CIP-14 golden vectors, read
  from the `cardano-wallet` clone at `3e623efdd3b2652288607536b58b3959717a9608` rather than copied
  from the PRD.
- Validation goes beyond the task graph's wording by two byte-length rules, which is what catches a
  transposed argument pair. The golden vectors themselves contain such a pair and it produces a
  well-formed fingerprint for a subject that does not exist.
- One finding the plan did not anticipate: `blake2b` guards its input with a realm-sensitive
  `instanceof Uint8Array`, which rejects a Node `Buffer` under the jsdom test environment. The module
  builds its input as a `Uint8Array` in its own scope, which is correct in both. Carried forward for
  every later module in this plan that hands bytes to that package.
- Checks, all from `nix build` and all built locally: `compile` exit 0, `lint` exit 0, `i18n` exit 0,
  `jest` 76 suites and 1008 tests with 1005 passed and 3 skipped. `package.json` and `yarn.lock` are
  unchanged.
- Nothing imports the module yet. `task-016` attaches the fingerprint to a merged row, and it is the
  place to decide what a row does if the computation ever throws.

## Self-Review

- The vectors were read from the `cardano-wallet` clone at a named commit, not copied from the PRD,
  which is the point of the finding rather than a formality.
- The one place the plan exceeds the task graph, two byte-length rules, is argued from a property of
  the golden vectors themselves and from a defect deleted one commit ago.
- The decision to throw rather than return `null` is stated with its consequence and the task that
  inherits it.
