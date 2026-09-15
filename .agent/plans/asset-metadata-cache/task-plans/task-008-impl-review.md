Implementation: Iteration 1
Timestamp: 2026-09-14T22:50:00Z

Changes made:
- `source/main/assets/assetVerification.ts`: new. The native script model as a closed union, a validating decoder with a depth cap, the policy digest, the attesting key set, the reference evaluator, and one verdict function that performs both steps.
- `source/main/assets/assetVerification.spec.ts`: new. Thirty-four cases across six groups, with three real mainnet fixtures.

Files touched:
- `source/main/assets/assetVerification.ts`
- `source/main/assets/assetVerification.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-008.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-008-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-008-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

A sub-script that does not decode fails the whole script rather than being dropped from its parent. Dropping it would silently turn `all of [ sig, something unrecognised ]` into `all of [ sig ]`, which is a weaker policy than the one the chain committed to, and `atLeast 2 of 3` into `atLeast 2 of 2`. There is a case for it.

`decodeNativeScript` is exported and takes an already-decoded CBOR value rather than bytes. That keeps the module's one entry point, `verifyPolicyBinding`, the only place that owns the byte layout, and it lets the spec drive decoder rejections with hand-built values instead of hand-built CBOR. The bytes-to-value step is `cbor.decodeFirstSync` inside the verdict function, wrapped so a decode failure is `not-a-script` rather than a throw.

Verification run:

- `jest source/main/assets/assetVerification --coverage=false` — 34 passed.
- The three real fixtures decode to exactly the scripts expected: `all of [ timeBefore 50588484, sig ]`, `atLeast 2 of [ timeBefore 600, sig, timeAfter 500 ]`, and `all of [ timeAfter 75846431, timeBefore 112500909, sig ]`.
- `any`, which does not occur in the corpus, is driven through a hand-encoded fixture so the branch has coverage despite having no live example.
- Decoder rejections, each with its own case: an unknown tag, a key hash that is not 28 bytes, a key hash that is not bytes at all, a negative threshold, a fractional threshold, a string threshold, a non-integer slot, a script nested forty deep against a cap of thirty-two, a sub-script that does not decode, and values that are not arrays.
- Evaluator: a signature requirement satisfied only by its own key hash; an all-of needing every branch; an empty all-of true and an empty any-of false; an any-of needing one; `atLeast 2 of 3` false with one key and true with two, which is the case the task graph names and a key-hash shortcut would get wrong; `atLeast 4 of 3` false with every key present; `atLeast 0` true with none; a nested all-of over an any-of resolving through both levels; and both time-lock forms true with an empty key set.
- The clock: the fixture whose upper bound is slot 112500909 binds and satisfies with the system time set to 2038 and again with it set to 1970, using fake timers, so the assertion is that the result does not depend on the clock rather than that it happens to be right today.
- Policy binding: the worked example binds and reproduces `c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b9`; the same fixture in upper case binds; a policy paired with a different subject fails with both digests present and different, and carries no `satisfied` field at all; an absent policy is `absent` for null, undefined and empty string; a non-hex policy, a policy of exactly two bytes and a policy of odd length are each `malformed`; a subject of eight characters and a subject of sixty-four non-hex characters are each `malformed` rather than `digest-mismatch`.
- `not-a-script` is driven with bytes that genuinely hash to the subject's policy id: the subject is computed from the bytes with the module's own digest function, so the case reaches the decoder rather than stopping at the digest.
- A malformed public key alongside a real one is ignored and the entry still satisfies; the malformed key alone does not satisfy.
- The at-least entry whose time-lock branches alone meet its threshold is pinned, with and without its signing key, and the comment above the assertion records the measured frequency and why the behaviour is not quietly tightened.

Corpus measurements backing the module, over 600 captured mainnet subjects of which 405 carry a policy:
- The two-byte strip reproduces the subject's policy id for 405 of 405. Hashing the whole policy field reproduces it for 0 of 405.
- Script tags occurring: 0 appears 413 times, 1 appears 374, 3 once, 4 twice, 5 appears 370. Tag 2 does not occur.
- Maximum nesting depth 2, largest script 10 nodes, against a decoder cap of 32.
- Evaluating each bound script against the key hashes that signed each property returns true for 1,682 of 1,682 property-level evaluations.
- Exactly one entry evaluates true against an empty key set.

Checks, all four through Nix with both new files staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 79 suites passed, 1112 tests with 1109 passed and 3 skipped, exit 0. The previous state of this branch was 78 suites and 1078 tests, so one suite and thirty-four tests were added and nothing else moved.

`nix fmt` was run and changed the two new files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing. `strict: false` narrowing was handled with `===` on every discriminated result, per the finding carried from `task-007`.

Deviations from the approved plan:
- None in scope or approach.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T22:58:00Z

Acceptance criteria, each against the evidence:

1. *A fixture whose policy field digests to a different policy id returns false, and the failure names which digest differed.* Met. The assertion is an exact equality on the whole result object, so an extra field or a missing digest fails it, and it separately asserts the absence of `satisfied`.

2. *`all`, `any` and `atLeast` each covered, including an `atLeast(2 of 3)` satisfied by one key returning false.* Met, with both sides of that boundary and the threshold-above-branches case as well.

3. *Time-lock nodes evaluate to true regardless of the clock, asserted with a policy whose lock expired.* Met, and met better than asked: the same fixture is driven at two system times far apart, so the test states clock-independence rather than correctness at one moment.

4. *No path can return true without both steps having been performed.* Met by construction and by assertion. `verifyPolicyBinding` returns before the evaluator on every failure branch, `evaluateNativeScript` takes a decoded script and a key hash set rather than registry data, and the only way to obtain a decoded script from a policy field is through the function that hashes first.

5. *`yarn test:jest` passes.* Met.

6-10. *The worked example, every corpus tag plus `any`, the rejection cases, the checks, and no new suppressions.* All met.

The decision to fail a script whose sub-script does not decode, rather than dropping it, is the right one and worth naming: dropping would weaken a policy rather than reject it, and `atLeast 2 of 3` becoming `atLeast 2 of 2` is the clearest form of that. It is the same instinct as refusing a public key that is not 64 hex characters instead of hashing a truncated prefix.

The item for the project owner stands as the plan wrote it: one policy-bearing subject in 405 carries a script that the registry's own evaluator satisfies with no signature, because two of its three branches are time locks and time locks are unconditionally true. The implementation matches the reference, the case is pinned by a test, and `task-009`'s corpus run will give the full-population figure that a decision needs.

Summary: Two of the three steps behind `verified` are in, and neither can be reached without the other. The digest runs before the decoder, which means the decoder is never handed bytes that do not already hash to the subject's own policy id. The evaluator is the registry's, transcribed in six lines, consulting no clock, and the one uncomfortable consequence of that rule is measured rather than argued about.

Decision: approved
