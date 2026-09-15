Implementation: Iteration 1
Timestamp: 2026-09-14T19:14:00Z

Changes made:
- `source/renderer/app/utils/assetFingerprint.ts`: new. One exported function, `assetFingerprint(policyId, assetName)`, which validates both arguments, concatenates their decoded bytes, takes blake2b with a 20-byte digest and bech32-encodes the result with the human-readable part `asset`.
- `source/renderer/app/utils/assetFingerprint.spec.ts`: new. Nineteen cases: the eight golden vectors driven from a table so each names itself, a transposition case, an upper-case-hex case, and nine rejection cases each asserting the message of the rule that should have fired.

Files touched:
- `source/renderer/app/utils/assetFingerprint.ts`
- `source/renderer/app/utils/assetFingerprint.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-005.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-005-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-005-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

No existing file changed, and `git diff HEAD -- package.json yarn.lock` is empty.

One finding the plan did not anticipate, and it is the reason the module is not four lines:

The first implementation built the hashed input with `Buffer.concat` and every one of the eight vectors failed with `input must be Uint8Array or Buffer` thrown from `blake2b/index.js:214`. The guard there is `assert(input instanceof Uint8Array, ...)`, and `instanceof` is realm-sensitive. `jest.config.js:147` sets `testEnvironment: 'jest-environment-jsdom'`, so the `Buffer` global injected into the test context and the `Uint8Array` that `blake2b` resolves in its own scope come from different realms, and a correct Buffer is rejected. The module now copies the decoded bytes into a `Uint8Array` constructed in its own scope, which is realm-independent and therefore correct in the renderer as well as under the test environment. The comment at `assetFingerprint.ts:17-21` records why, so the copy is not removed later as redundant.

This is the kind of thing the bundling question under Live Repo Findings was asked to catch and did not: reading how the package ships its WebAssembly settled that webpack can bundle it, and said nothing about how it validates its arguments.

Verification run:

- `node_modules/.bin/jest source/renderer/app/utils/assetFingerprint.spec.ts` — 19 passed. All eight golden vectors produce their published fingerprints. The two vectors that are the same pair of 28-byte values transposed produce different fingerprints, asserted directly as well as by their individual expectations.
- Rejection cases, each asserting the message belonging to its rule: an odd-length policy id and a policy id ending in `zz` both reject as not hex, where `Buffer.from` alone would have hashed a truncated prefix; an asset name containing a non-hex character rejects as not hex; the policy id length rule rejects 0, 27 and 29 bytes and accepts 28; the asset name length rule accepts 32 bytes, which is also the eighth golden vector, and rejects 33; and the two arguments given in the wrong order reject on the policy id length rule, which is what those two rules exist for.

Checks, all four through Nix on a dirty tree with both new files staged, each built locally rather than substituted:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built `0bs4135cr2ypl3j6yicalphj81ix1sfg-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `f9864njlywyyvy7aimhxy19hsbrmjvib-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built `vh2rcbfg5298a1ga26davrgkc5x4nch3-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 76 suites passed, 1008 tests with 1005 passed and 3 skipped, exit 0. The previous state of this branch was 75 suites and 989 tests, so one suite and nineteen tests were added and nothing else moved.

No new `@ts-ignore` and no new `@ts-expect-error`. `blake2b` ships no type declarations and none are added: `tsconfig.json` runs with `strict: false`, so an untyped module import is implicitly `any` and `compile` passes, which is the same footing `source/main/governance/AnchorVerificationService.ts:1` already stands on.

Deviations from the approved plan:
- None in scope or approach. The two byte-length rules were recorded in the plan as an excess over the task graph's wording and were implemented as recorded.
- One detail decided during implementation: `decodeHex` returns a `Uint8Array` rather than a `Buffer`, for the realm reason above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T19:16:30Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes with all eight vectors asserted.* Met, through `nix build '.#checks.x86_64-linux.jest'`. The eight are transcribed from `cardano-wallet` at `3e623efdd3b2652288607536b58b3959717a9608`, `lib/primitive/test/spec/Cardano/Wallet/Primitive/Types/TokenFingerprintSpec.hs:40-78`, and the spec names that file and commit at the top of the table so a reader can re-derive them rather than trust them.

2. *No new entry in `package.json`.* Met. `git diff HEAD -- package.json yarn.lock` produces nothing.

3. *The empty asset name produces a fingerprint rather than throwing.* Met by three of the eight vectors, which is stronger than a separate case asserting only that it does not throw.

4. *An odd-length input, a non-hex input, a policy id of the wrong length and a transposed pair are each rejected.* Met, with boundaries on both length rules and with each case asserting the message of the rule that should have fired. A bare `toThrow()` would have passed for the transposed pair if the hex rule had rejected it, which would have been the wrong reason.

5. *`compile`, `lint` and `i18n` green from `nix build`.* Met, all three built locally.

6. *No new `@ts-ignore` and no new `@ts-expect-error`.* Met.

The finding worth carrying: `blake2b` validates its input with a realm-sensitive `instanceof`, and under `jest-environment-jsdom` that rejects a Node `Buffer`. Any later module in this plan that hands bytes to that package from the renderer will hit the same wall, and the fix is to construct the `Uint8Array` locally rather than to pass a `Buffer`. `task-010` and the verification modules in phase 2 are the ones that will need it.

Summary: One pure function, eight published vectors reproduced exactly, and validation that refuses the one input class a caller is most likely to get wrong, which is the argument order. The token row's identity no longer depends on the endpoint being reachable, which is what the rest of the plan needs before it can remove it.

Decision: approved
