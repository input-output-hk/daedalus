Implementation: Iteration 1
Timestamp: 2026-09-16T21:00:00Z

Changes made:
- `source/main/assets/chainPointerVerification.realfs.spec.ts`: sixteen cases.
- `source/main/assets/immutableBlockReader.realfs.spec.ts`: eight cases and six
  more fixture options.
- `source/main/assets/koiosClient.spec.ts`: ten cases.
- `source/main/assets/cborSpan.spec.ts`: one case.
- `source/main/assets/assetRegistryClient.spec.ts`: two cases.
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.spec.tsx`:
  three cases.
- `source/main/assets/chainPointerVerification.ts`: one source change, recorded
  below.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.

Files touched:
- the seven source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

**The one source change, and why it is not a test accommodation.** A CIP-25
payload the reader cannot interpret used to reject the whole pointer, because
`cip25For` let a `CborError` escape into the outer handler. That is wrong on its
own terms: the transaction's integrity is already established by the three hash
checks, and the payload is read opportunistically for a name. Refusing the
pointer left a genuinely minted asset with no row at all, so a minter who wrote
an unusual metadata shape would lose their asset's identity rather than its name.
It now returns no name. The case that found it drives a metadata payload that is
not a map.

Testing Strategy, phase 7's four items, each against the spec that covers it:

1. **The metadata source URL validator.** `config/assetsConfig.spec.ts`, nine
   cases: the Koios default with its path prefix, a port, a port and a path, a
   trailing slash, the literal `direct`, an `http://` URL, a query string, a host
   carrying a character outside the class, and the empty string.
2. **`getAssetMetadataSourceIdFromUrl`.** `utils/assets.spec.ts`, five cases.
3. **Pointer resolution over a recorded pair, and the three local checks.**
   `chainPointerVerification.realfs.spec.ts`, against a real preprod block, with
   a tampering case for each of the three.
4. **The chain-row assertion.** `assetMetadataResolver.realfs.spec.ts`, all six
   columns, plus `chainPointerToRow` driven directly for the CIP-68 and
   array-name forms.

Coverage over the phase 7 modules, before and after:

| Module | Statements before | after | Branches before | after |
|---|---|---|---|---|
| `chainPointerVerification.ts` | 84.86 | 94.21 | 68.13 | 84.94 |
| `immutableBlockReader.ts` | 89.06 | 97.65 | 70.21 | 89.36 |
| `koiosClient.ts` | 92.00 | 96.00 | 82.60 | 92.39 |
| `cborSpan.ts` | 99.08 | 99.54 | 96.36 | 98.18 |
| `AssetMetadataSettings.tsx` | 94.64 | 100 | 71.42 | 75.00 |

Four lines remain unreached and each is recorded rather than chased:

- `chainPointerVerification.ts:269` and `:397`, both catches around
  `Buffer.from(value, 'hex')`. That call does not throw on invalid hex; it stops
  at the first bad character. Reaching either would mean changing the module.
- `chainPointerVerification.ts:356`, the CBOR decode inside the closure check.
  The bytes reaching it have already been walked by the structural reader and
  have already hashed to the policy id, so a decode failure there would need a
  script that is structurally valid, hashes to the right policy, and still fails
  to decode.
- `cborSpan.ts:200`, the `default` arm over a major type. The value is three bits
  and all eight are handled above it.
- `httpTransport.ts:113`, the rejection handler on `readResponse`, which resolves
  on every path and never rejects.

`immutableBlockReader.ts:291` is the outer catch, reached only by a filesystem
error the inner guards do not already answer for. Left, and named.

Verification run:

- `yarn jest source/main/assets ... --coverage` for the table above, over the
  same module set both times.
- The eleven rejection reasons `confirmChainPointer` can return: nine have a case
  asserting that exact reason, and the two that do not are the `Buffer.from`
  catches above.
- The reader's fail-closed paths: eight of nine driven, each asserting its own
  reason string, so a change that collapsed them into one would fail eight cases.
- No spec here opens a socket except `assetRegistryClient.spec.ts`, which starts
  its own loopback server, as it already did before this task.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 96 suites passed,
  1624 tests with 1621 passed and 3 skipped, exit 0. The branch stood at 96
  suites and 1581 tests, so this adds forty-three tests and no suite.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

One earlier lint run failed on two findings introduced here, a duplicated test
title and a nested ternary in a fixture, both fixed before the run above.

`nix fmt` was run and changed files before each check run.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- One source file changed, which the plan allowed as a finding and which is
  recorded above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T21:10:00Z

Acceptance criteria, each against the evidence:

1. *Every phase 7 Testing Strategy item has a spec, named.* Met, four items.

2. *Every fail-closed path in the reader is driven or recorded.* Met, eight of
   nine, with the ninth named.

3. *Every rejection reason is driven or recorded as unreachable.* Met, nine of
   eleven, with the two argued.

4. *The response cap asserted as a parameter.* Met, two calls against one server
   with different caps.

5-8. *All six checks, no network, the source change recorded, no suppressions.*
   Met.

Two judgements worth naming.

**The source change is the most valuable thing this task produced, and it came
from a coverage gap rather than from a test.** An unreadable metadata payload was
rejecting the whole pointer. Nothing in the suite failed, no criterion in any
earlier task covered it, and the effect would have been an NFT with an unusual
payload losing its row entirely rather than losing its name. Asking why a line
was unreached is what found it.

**The remaining unreached lines are the right ones to leave.** Two are catches
around a call that does not throw, one is a default over an exhausted union, one
is a rejection handler on a promise that resolves on every path. Making any of
them reachable would mean weakening the code around it, which is the failure mode
a coverage target produces when it is treated as a goal.

What this task does not establish: that any of this works against a real chain
and a real index outside this machine. Every case here runs against a fixture,
a stub or a temporary directory. `task-038` is the part that cannot be automated,
and it is the only place the three platforms and a live instance are exercised.

Decision: approved
