Planner: Iteration 1
Timestamp: 2026-09-14T19:08:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-005.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. The acceptance criteria are eight published golden vectors and the four Nix checks.
- Two new files and no change to any existing one: a pure function and its colocated spec.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the CIP-14 reference at `:149`, the local-computation decision at `:816` and the testing strategy at `:1453`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-005` entry plus `task-010` and `task-016`.
- `cardano-wallet` at `3e623efdd3b2652288607536b58b3959717a9608`, `lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78`.
- `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.

Repo-Verified Findings Used To Shape The Plan:
- The eight vectors were read from the clone rather than from the PRD. Three have an empty asset name, two a six-byte name, two a 28-byte name and one a 32-byte name of zero bytes. The helper at `:80-90` shows both inputs are hex-decoded before hashing.
- Measured in this tree with Node v22.23.1: `blake2b(20).update(bytes).digest()` and `blakejs.blake2b(bytes, null, 20)` each reproduce all eight.
- `package.json:207` and `:210` pin `bech32` and `blake2b` exactly, and both resolve in `yarn.lock`.
- `blake2b` is today imported only from main-process modules. `blake2b-wasm/blake2b.js` embeds its WebAssembly module as a base64 literal in a single JavaScript file, so the renderer's webpack build needs no wasm loader, and `blake2b/index.js` carries a complete JavaScript implementation used until that module resolves.
- `bech32@2.0.0`'s `encode` defaults to a 90-character limit; a CIP-14 fingerprint is 44 characters, so no limit argument is needed.
- Two of the eight vectors are the same pair of 28-byte values transposed, and both produce valid fingerprints. `task-004` deleted `getUniqueId` one commit ago for the same class of key-order confusion.
- `grep -rn "assetFingerprint" source tests storybook` returns nothing.

Planned Approach:
- Decode, concatenate, blake2b with a 20-byte digest, bech32-encode with the human-readable part `asset`.
- Validate hex shape, a 28-byte policy id and an asset name of at most 32 bytes, and throw rather than return `null`.

Scope Guard / Self-Review:
- No call site, no dependency, no main-process work, no memoisation.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T19:10:00Z

Three gaps, in descending severity.

1. Every rejection case asserts only that the call throws. The plan's whole argument for adding two byte-length rules beyond the task graph's wording is that they distinguish a transposed argument from a malformed string. A spec that asserts `toThrow()` cannot tell which rule fired, so it would pass if the hex check rejected a correctly-shaped transposed pair for the wrong reason, and it would pass if every rule collapsed into one generic error. The rules need distinct messages and the spec needs to assert on them.

2. The length rules are stated without boundaries. `task-001` set the standard in this plan workspace: a predicate with a range is asserted immediately either side of each edge. Here that means 27, 28 and 29 bytes for the policy id and 32 and 33 for the asset name, with the 32-byte case doubling as the eighth golden vector.

3. A factual slip in the plan's own text: the non-goals section says "two blake2b calls over 60 bytes per rendered row". It is one digest, over at most 60 bytes. Small, and a plan that miscounts its own arithmetic invites a reader to check everything else by hand.

Nothing in the plan is wrong. Settling the bundling question before choosing the package, by reading how `blake2b-wasm` ships its module rather than assuming, is the right order to do it in, and naming `blakejs` as an already-reasoned fallback is worth more than silently picking it.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T19:11:30Z

Plan Updates In Response To Critique:
- Added a fourth implementation step requiring a distinct error message per rule, and changed the Verification Plan so every rejection case asserts the message belonging to the rule that should have fired rather than a bare throw.
- Replaced the two length rejection cases with per-boundary cases: 27, 28 and 29 bytes for the policy id, 32 and 33 for the asset name.
- Corrected the arithmetic in Non-Goals to one digest over at most 60 bytes.

Resulting Approved Plan Shape:
- Two new files, eight golden vectors read from the clone at a named commit, and rejection cases that name which rule rejected them.
- One recorded excess over the task graph's wording, the two byte-length rules, argued from a property of the vectors themselves.

Scope Guard / Self-Review:
- The revision tightens assertions and fixes a count. It changes nothing about what is built.

Outcome: Canonical task plan revised after critique and approved for build execution
