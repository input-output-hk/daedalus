# Task task-008: Verification steps one and two: policy binding and key binding

## Task ID and Title

- ID: `task-008`
- Title: `Verification steps one and two: policy binding and key binding`

## Why Chosen Now

`task-007` landed at `49a0ed3b3`, which satisfies this task's only dependency. The registry client now
returns normalised entries carrying `policy` and, per property, the public keys that signed it, so
this task verifies a real shape rather than a hand-written one.

It is also the first half of the only part of this plan that carries real uncertainty. `verified`
exists to mean that the metadata is cryptographically bound to the minting policy; steps one and two
establish the binding and step three, `task-009`, establishes the attestation. Nothing downstream may
depend on `verified` until both are in.

## Interaction Mode

- Mode: `agent_execution`

Everything here is a pure function of bytes. The fixtures are real registry entries captured from
mainnet, and every acceptance criterion is a Jest assertion.

## Scope

- A new `source/main/assets/assetVerification.ts` holding the native script model, the CBOR decoder
  for it, the reference evaluator, the policy digest, the key hash, and the one function that
  produces a verdict from both steps together.
- A colocated `source/main/assets/assetVerification.spec.ts`.

`task-009` extends the same two files with step three. This task leaves no verdict that phase 4 could
read as `verified`, because the function it exports is explicitly named for the two steps it
performs.

## Non-Goals

- No signature verification. That is `task-009`, and the verdict this task produces is not
  sufficient for `verified`.
- No database write and no network call. This module is pure: bytes in, verdict out.
- No slot or clock. Time-lock nodes evaluate to `True` unconditionally, per the reference
  implementation, and a module that took the current time would be asserting something the registry
  does not.
- No CIP-25 or CIP-68 script handling. Plutus scripts are not native scripts and a policy that is
  one fails to decode, which is the correct outcome here.

## Dependencies

- `task-007`, complete at `49a0ed3b3`. A type-only import of `RegistryEntry` and
  `RegistrySignature`, so the verifier reads the shape the client actually produces.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the three classes of `verified`
  at `:399-412`, the reference evaluator at `:414-432`, the time-lock rule at `:434-440`, the
  measured script shapes at `:442-448`, and the two worked digests at `:762-778`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-008` entry, plus
  `task-009` and `task-010`.
- The reference implementation named in the PRD,
  `token-metadata-creator/src/Cardano/Metadata/Types.hs:253-287`, `evaluatePolicy`.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.
- Workflows: `.agent/workflows/test.md` for the Jest invocation.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `49a0ed3b3` on branch `docs/asset-metadata-cache-plan`, 2026-09-14. The corpus figures
below come from 600 mainnet subjects captured from the live registry, 405 of which carry a policy.

**The two-byte strip is right for every policy-bearing entry in the corpus, and the alternative is
needed for none.** Both candidate forms were computed for all 405: hashing `0x00` concatenated with
the policy bytes after dropping two leading bytes, and hashing `0x00` concatenated with the whole
policy field. The first reproduces the subject's policy id for 405 of 405; the second for 0 of 405.
So the strip is a rule rather than a heuristic, and no fallback form is warranted.

**The two leading bytes are an array header and a small integer, and the integer takes two values.**
Decoding the whole policy field gives a two-element array in every case, whose first element is 1 in
370 entries and 0 in 35. Both encode in one byte, which is why a fixed two-byte strip is safe for
both.

**Every script tag that occurs, with counts.** Walking all 405 decoded scripts: tag 0, a key hash
requirement, 413 occurrences; tag 1, all-of, 374; tag 3, at-least, 1; tag 4, a lower time bound, 2;
tag 5, an upper time bound, 370. Tag 2, any-of, does not occur in this sample. It is implemented
regardless, because the registry accepts any native script and the evaluator's correctness cannot
depend on which shapes happen to be popular.

**Scripts are shallow.** Maximum nesting depth across the 405 is 2, and the largest script holds 10
nodes.

**Step two passes for every property of every bound entry in the corpus.** Taking blake2b-224 of each
public key that signed a property, and evaluating the decoded script against that set: 1,682 of 1,682
property-level evaluations return true, across 405 of 405 entries.

**One entry in 405 evaluates to true against an empty key set, and it is not a defect in the
evaluator.** Subject `84e7bef00924708ab746b79b94a3e3659244854c1acf9119c288e581436654657374436f696e`
carries the script `atLeast 2 of [ timeBefore 600, sig c04cc33b…, timeAfter 500 ]`. Time-lock nodes
evaluate to `True` unconditionally under `evaluatePolicy`, so two of the three branches are satisfied
with no signature at all and the threshold is met. This is what the registry's own rule does, and
matching it is the instruction. It is recorded under Risks with its measured frequency rather than
silently reproduced.

**Two real fixtures for the shapes the PRD calls for.** Beyond the worked example in the PRD:

| Subject | Script |
|---|---|
| `84e7bef0…436654657374436f696e` | `atLeast 2 of [ timeBefore 600, sig, timeAfter 500 ]` |
| `a90d1702…4d414e45` | `all of [ timeAfter 75846431, timeBefore 112500909, sig ]` |

The second carries both time bounds in one script, and its upper bound is a slot long past, which is
the case the PRD warns an implementer who checks the clock would fail.

**The digest primitive is already in the main process.** `source/main/governance/AnchorVerificationService.ts:1`
imports `blake2b`, declared at `package.json:210` as `2.1.3`. `blake2b(28)` gives the 224-bit digest.
`cbor` is declared at `package.json:218` as `5.0.2` and is already imported in the main process at
`source/main/utils/restoreKeystore.ts:1`.

**The realm trap from `task-005` applies here.** `blake2b` guards its input with
`assert(input instanceof Uint8Array)`, which is realm-sensitive, and the default Jest environment is
jsdom (`jest.config.js:147`). This spec declares `@jest-environment node`, and the module builds its
hash input as a `Uint8Array` constructed in its own scope regardless, so it is correct under both.

**The narrowing trap from `task-007` applies here.** `tsconfig.json` runs with `strict: false`, under
which a discriminated union narrows by equality on its discriminant and not by truthiness. Result
types in this module are discriminated and are narrowed with `===`.

**Nothing collides.** `grep -rn "assetVerification\|evaluateNativeScript" source tests` returns
nothing.

## Files Expected To Change

- `source/main/assets/assetVerification.ts` — new.
- `source/main/assets/assetVerification.spec.ts` — new.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-008` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-008*.md` — this plan and its two review logs.

No deviation from the task graph's `targetPaths`.

## Implementation Approach

1. **The native script model is a closed union.**

   ```ts
   export type NativeScript =
     | { kind: 'sig'; keyHash: string }
     | { kind: 'all'; scripts: Array<NativeScript> }
     | { kind: 'any'; scripts: Array<NativeScript> }
     | { kind: 'atLeast'; required: number; scripts: Array<NativeScript> }
     | { kind: 'timeAfter'; slot: number }
     | { kind: 'timeBefore'; slot: number };
   ```

   Decoding produces one of these or `null`. There is no permissive branch and no passthrough of an
   unrecognised tag, so a script the model does not cover fails rather than being evaluated as
   something else.

2. **Decode validates, it does not cast.** Each tag is checked for its own arity and element types: a
   28-byte byte string for `sig`, an array for `all` and `any`, a non-negative integer threshold and
   an array for `atLeast`, an integer slot for the two time bounds. A depth cap of 32 bounds the
   recursion. The measured maximum in the corpus is 2, so the cap cannot affect a real policy, and it
   removes unbounded recursion as a question rather than leaving it to be argued about.

3. **The digest is taken before the decode, and that ordering is the point.** Step one hashes bytes;
   step two decodes them. Running the hash first means the decoder only ever sees bytes that already
   hash to the subject's own policy id, which an attacker controlling the response cannot produce for
   a policy they do not control. The decoder is therefore not an attack surface reachable with
   arbitrary input.

4. **Step one, stated so the failure is diagnosable.**

   ```ts
   export type PolicyBindingResult =
     | { ok: true; script: NativeScript }
     | { ok: false; reason: PolicyBindingFailure; expectedPolicyId?: string; actualPolicyId?: string };
   ```

   `reason` is one of `absent`, `malformed`, `digest-mismatch` or `not-a-script`. A digest mismatch
   carries both digests, which is what the task graph's "the failure names which digest differed"
   asks for. `absent` is the ordinary state of a large share of the registry, so it is not logged as
   an error and not logged at all from inside this pure module.

5. **Step two is the reference evaluator, transcribed.**

   ```
   sig h        -> keyHashes has h
   all xs       -> every x satisfied
   any xs       -> some x satisfied
   atLeast n xs -> count of satisfied x >= n
   timeAfter _  -> true
   timeBefore _ -> true
   ```

   Six cases, one line each. The two time-lock cases take no argument and consult no clock. A
   key-hash lookup is deliberately not used in their place: it is undefined for `any` and `atLeast`,
   where no single key is required and one signer does not satisfy the policy, and marking such an
   entry verified would reintroduce the operator trust `verified` exists to remove.

6. **The attesting key set comes from the signatures of one property, and a value that is not a key
   does not enter it.** An ed25519 public key is 32 bytes, so a `publicKey` that is not exactly 64
   hex characters is left out rather than hashed. `Buffer.from(x, 'hex')` truncates at the first
   pair it cannot read instead of refusing, so without the check a malformed key would contribute a
   well-formed digest of a prefix: the entry would fail closed and nothing would look wrong, and
   `task-009` would hand the same string to ed25519 verification. The set holds `blake2b-224` of each
   surviving key, lower-cased. Verification is per property, because a subject may carry a verified
   ticker and an unverified decimals.

7. **Hex is compared case-insensitively, and a subject that is not a subject is refused.** Both the
   computed digest and the subject's leading 56 characters are lower-cased before comparison, because
   hex from an external source is not guaranteed lower case. A subject that is not at least 56
   hexadecimal characters fails with `malformed` rather than being sliced and compared, so the reason
   reported is the true one rather than `digest-mismatch` by accident.

8. **One function produces the verdict, and it performs both steps.**

   ```ts
   export const verifyPolicyBinding = (
     subject: string,
     policy: string | null,
     signatures: Array<RegistrySignature>
   ): PolicyVerificationResult
   ```

   It returns `{ bound: false }` without evaluating when step one fails, and otherwise
   `{ bound: true; satisfied: boolean; script }`. No other export returns a boolean that could be
   read as a verdict: `evaluateNativeScript` takes a key set rather than signatures, so a caller
   cannot reach it with registry data without having decoded a script, which only step one produces.

9. **Names say which steps they cover.** The exported verdict is `PolicyVerificationResult` with a
   `satisfied` field, not `verified`. `task-009` adds the attestation and `task-010` is the only
   place the three are combined into the column.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **Policy binding: a fixture whose policy field digests to a different policy id returns false, and
   the failure names which digest differed.** Driven with a real policy paired with the wrong
   subject, asserting `reason`, `expectedPolicyId` and `actualPolicyId`.
2. **Script evaluation follows `evaluatePolicy`: `all`, `any` and `atLeast` are each covered by a
   fixture, including an `atLeast(2 of 3)` satisfied by one key, which must return false.**
3. **Time-lock nodes evaluate to true regardless of the current clock, asserted with a policy whose
   lock expired in 2022.** The real fixture `a90d1702…4d414e45` carries `timeBefore 112500909`; the
   spec asserts the result is the same with the system clock moved far past it, using fake timers so
   the assertion is about the code rather than about when the suite runs.
4. **No path through the module can return true without both the policy digest and the script
   evaluation having been performed.** Settled by the export surface: one verdict function, which
   returns `bound: false` before reaching the evaluator, plus a case asserting that a failed binding
   yields no `satisfied` field at all.
5. **`yarn test:jest` passes.**

Five this task adds to its own closure:

6. The live-captured subject from the PRD passes both steps, reproducing the two digests it records.
7. Every script tag measured in the corpus is covered by a fixture, and so is `any`, which the corpus
   does not contain.
8. A malformed policy, a policy that is not hex, a policy shorter than three bytes, a policy that
   decodes to something that is not a native script, and a script nested past the depth cap are each
   rejected with their own reason.
9. `compile`, `lint` and `i18n` are green from `nix build`, and `package.json` and `yarn.lock` are
   unchanged.
10. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

**Step one, policy binding.**
- The PRD's subject `c76ef54…42544544` with its real policy binds, and the computed digest equals
  `c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9`.
- The same policy against a different subject fails with `digest-mismatch`, and both digests are
  present and different in the result.
- A null policy fails with `absent`.
- A policy that is not hex fails with `malformed`.
- A policy of two bytes or fewer fails with `malformed`, because stripping two bytes would leave
  nothing to hash.
- A subject of fewer than 56 hexadecimal characters fails with `malformed`, not with
  `digest-mismatch`.
- The same fixture with an upper-case subject binds, because both sides are lower-cased before
  comparison.
- A policy whose stripped bytes are valid CBOR but not a native script, an integer for instance,
  fails with `not-a-script` after the digest matched, which requires a fixture constructed so that
  the digest does match: the subject is computed from the bytes rather than fixed.
- Both real fixtures, `84e7bef0…` and `a90d1702…`, bind.

**Step two, the evaluator.** Driven against `evaluateNativeScript` directly with synthetic scripts,
because the corpus does not contain every shape:
- `sig` with the key hash present returns true, absent returns false.
- `all` of two satisfied returns true; one unsatisfied returns false; an empty `all` returns true,
  which is what `all` over an empty list means and is worth pinning rather than discovering.
- `any` of two with one satisfied returns true; none satisfied returns false; an empty `any` returns
  false.
- `atLeast 2 of 3` with one key satisfied returns false, which is the criterion the task graph names
  and the case a key-hash shortcut would get wrong.
- `atLeast 2 of 3` with two keys satisfied returns true.
- `atLeast 0` returns true with no keys.
- `atLeast 4 of 3` returns false however many sub-scripts are satisfied.
- A negative threshold, and a non-integer one, are refused by the decoder rather than interpreted by
  the evaluator.
- A nested `all` containing an `any` containing a `sig` resolves through both levels.
- `timeAfter` and `timeBefore` each return true alone, with an empty key set.
- The real `a90d1702…` script returns true for its signing key with the clock advanced past its
  upper bound, and returns false for a key that is not in it.

**The clock.** `jest.useFakeTimers` with the system time set to 2038 for the time-lock case, and the
same assertion repeated with it set to 1970, so the result is asserted to be independent of the clock
rather than merely correct at one instant.

**The combined verdict.**
- A subject whose policy is absent yields `bound: false` and no `satisfied` field.
- A signature whose `publicKey` is not 64 hex characters is excluded from the attesting set: the
  same fixture with one malformed key alongside the real one still satisfies, and with the malformed
  key alone does not.
- A subject that binds but whose signing key is not required yields `bound: true, satisfied: false`.
- The PRD's subject yields `bound: true, satisfied: true` for each of its five properties.
- The `atLeast 2 of 3` real fixture yields `satisfied: true` with an empty signature list, which is
  the measured property under Risks, asserted so that a change in that behaviour is a failing test
  rather than a silent shift.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`, with both new files staged.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff -- package.json yarn.lock` must be empty.

## Risks and Open Questions

1. **The registry's own rule admits a script that no key satisfies, and one entry in 405 is such a
   script.** `atLeast 2 of [ timeBefore, sig, timeAfter ]` evaluates to true with an empty key set,
   because both time-lock branches are unconditionally true. For that subject `verified` would mean
   "the policy field hashes to the policy id and the script evaluates", with the signature step still
   to come in `task-009`, but with no requirement that the signer be anyone in particular. Measured
   frequency: 1 of 405 policy-bearing entries in the corpus, 0.25 percent. The cost of the stricter
   alternative, requiring at least one satisfied `sig` node, is that it would diverge from the
   registry's own definition of a valid attestation, so a subject the registry considers correctly
   attested would show as unverified in Daedalus and nowhere else. **This is a decision for the
   project owner and is surfaced rather than taken.** The implementation matches the reference, and
   the case is pinned by a test so that changing it is deliberate.
2. **Tag 2, `any`, does not occur in the corpus.** It is implemented and unit-tested against
   synthetic scripts. That is the best available evidence: there is no live example to check against,
   and leaving it out would mean the first `any` policy ever published silently failed to verify.
3. **The corpus is 600 subjects of roughly 7,977, and it is a sample.** The counts above describe the
   sample. `task-009`'s corpus gate runs the whole registry and is where the full-population figures
   come from.
4. **`cbor@5.0.2` decodes into Buffers and plain arrays.** The decoder validates element types rather
   than trusting them, and a `Buffer` check is realm-sensitive in the same way the `blake2b` input
   check is. The key-hash branch accepts any `Uint8Array` of 28 bytes, which is true of a `Buffer` in
   either realm.
5. No open questions for the project owner beyond item 1, which is one.

## Required Docs, Research, and Tracking Updates

- Update `task-008`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-008-plan-review.md` and `task-008-impl-review.md` as the cycle requires.
- No PRD change. The corpus measurements agree with it and add counts it does not carry.
- `task-009` inherits the corpus gate and should report, alongside its signature figures, how many
  policy-bearing subjects across the whole registry evaluate to true against an empty key set. This
  plan measured 1 in 600; the full-population figure is what a decision on item 1 needs.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-008-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-008-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-008` complete. Reviewed and approved in `task-008-impl-review.md`.
- The two-byte strip is a measured rule rather than a carried assumption: it reproduces the policy id
  for 405 of 405 policy-bearing mainnet subjects and the alternative form for none.
- The evaluator is the registry's own, six cases, consulting no clock. Time locks are satisfied
  unconditionally, which is asserted at two system times sixty-eight years apart rather than once.
- The digest runs before the decoder, so the decoder is only ever handed bytes that already hash to
  the subject's own policy id.
- One measured consequence of the reference rule is pinned by a test and handed to the project owner:
  one subject in 405 carries an `atLeast 2 of 3` whose two time-lock branches meet the threshold with
  no signature at all. `task-009`'s corpus run supplies the full-population figure.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 79 suites and
  1112 tests with 1109 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.

## Self-Review

- The two-byte strip is measured against 405 real entries with its alternative measured alongside,
  so it is a finding rather than a rule copied forward.
- The evaluator is transcribed from the reference implementation and its one uncomfortable
  consequence is measured, named, priced and handed to the owner rather than quietly improved.
- Every shape the corpus contains has a real fixture, and the one shape it does not contain is
  covered synthetically with that gap stated.
