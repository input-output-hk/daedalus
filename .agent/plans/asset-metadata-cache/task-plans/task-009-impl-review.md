Implementation: Iteration 1
Timestamp: 2026-09-14T23:50:00Z

Changes made:
- `source/main/assets/assetVerification.ts`: extended with `attestationPayload`, `verifyAttestationSignature`, `isPropertyAttested` and `verifyRegistryProperty`.
- `source/main/assets/assetVerification.spec.ts`: extended with twenty-five cases across four groups.

Files touched:
- `source/main/assets/assetVerification.ts`
- `source/main/assets/assetVerification.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-009.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-009-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-009-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

The corpus gate, recorded here as the task graph requires. Run 2026-09-14 against the live query endpoint, over the registry's whole mapping list at commit `363982b999060874f80486c2758a8a19d3b5f78a`, in 118 batches sized by the same 6 KB rule the client uses:

| Measure | Count |
|---|--:|
| Subjects in the mapping list | 7,977 |
| Subjects the endpoint answered | 7,977 |
| Batches that failed or had to be retried | 0 |
| Carrying a `policy` field | 4,579 |
| Passing step one, policy binding | 4,579 |
| Failing step one | 0 |
| Property attestations examined | 19,209 |
| Passing step two, script evaluation | 19,209 |
| Passing step three, ed25519 | 19,208 |
| Properties carrying more than one signature | 104 |
| Subjects evaluating true against an empty key set | 2 |

The 4,579 of 7,977 matches the PRD's full-corpus figure exactly, which is the independent check that the corpus enumerated was the one the PRD described.

One property in 19,209 does not verify, and its cause was established rather than assumed. Subject `29b3596c56b3862bd41a5ea87f3529d79dd0b56e036c1336751b3df9464f58`, property `description`, sequence number 0, value `The Hunter's Token`: eighteen ASCII characters with nothing unusual to encode. The same subject's `name`, `ticker` and `decimals` all verify under the same public key. No alternative property name, no sequence number from 0 to 3, and no plausible alternative value reproduces the signature. The mapping file in the registry repository at that same commit carries byte-for-byte the signature the API returns, so the record is one the registry accepted rather than something mangled in transit. It is a defect in the registry's data. The product consequence is that one description shows unverified, which is the correct outcome for a record whose attestation does not verify, and it is the outcome this code produces.

A verifier that had agreed with the registry on all 19,209 would have been weaker evidence that it checks anything.

Two details decided during implementation:

The middle class the PRD names, a genuine signature from a key set the policy does not satisfy, cannot be taken from the registry: producing one would require a second policy that hashes to the same policy id. It is constructed in the spec instead, with `crypto.generateKeyPairSync('ed25519')`: a script is built requiring a different key hash, the subject is computed from that script's digest, and the payload is genuinely signed with the generated key. The complementary case, the same construction with the script requiring the generated key's own hash, drives the whole three-step chain end to end against a signature this suite produced rather than one it was handed.

`digest` was split into `digestBytes` and a hex wrapper. The payload concatenates raw digests and the policy path compares hex, and converting to hex and back to concatenate would have been a second place for an encoding mistake to live.

Verification run:

- `jest source/main/assets/assetVerification --coverage=false` — 59 passed, of which 25 are new.
- The payload verifies against the live signature of each of `name`, `ticker`, `url`, `description` and `decimals` on subject `c76ef54…42544544`, driven from a table so each names itself.
- The CBOR-text trap is pinned from both sides: a payload built by hashing the subject as raw UTF-8, and one built by hashing the property name as raw UTF-8, are each asserted to differ from the real one.
- The `logo` branch is asserted as a rule against a payload built independently in the spec: the real payload equals the decoded-bytes form and differs from the base64-text form. The live confirmation is separate and is recorded here: for the 77,392-character logo of that subject, the base64-text form does not verify and the decoded-bytes form does, and the decoded bytes begin `89504e470d0a1a0a`.
- Strictness: the real `decimals` signature verifies and the same signature with `S` replaced by `S + L` is rejected. The malleable signature is computed in the spec from the group order, and the case asserts both that the construction did not overflow and that it produced a different signature, so it cannot pass by comparing a value with itself.
- Negatives, one case each: a tampered value, a tampered sequence number, a signature valid for a different property of the same subject, and a signature valid for the same property of a different subject.
- Malformed inputs return false rather than throwing: a short signature, a signature with non-hex characters, a short public key, a well-formed signature that is not a valid point, and a public key that is not a valid point.
- `isPropertyAttested`: attests on one good signature, attests when one of two verifies, refuses when only a junk one is present, and refuses an empty signature list.
- `verifyRegistryProperty`: all four fields true for the worked example; an absent policy reports `bound: false` with `attested: true`, which is the distinction that made short-circuiting wrong; a synthetic entry signed by a key the script requires is fully verified; the same construction with the script requiring a different key hash is `satisfied: false, attested: true, verified: false`; and a tampered signature on a bound and satisfied entry is `attested: false, verified: false`.

Checks, all four through Nix:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 79 suites passed, 1137 tests with 1134 passed and 3 skipped, exit 0. The previous state of this branch was 79 suites and 1112 tests, so twenty-five tests were added to an existing suite and nothing else moved.

`nix fmt` was run and changed both files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing, which is acceptance criterion 3.

Deviations from the approved plan:
- None in scope or approach. The synthetic middle-class fixture is a stronger form of a case the plan called for, not a different case.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T23:58:00Z

Acceptance criteria, each against the evidence:

1. *The payload is blake2b256 over the four concatenated digests, asserted against five live properties.* Met. Each of the five is a real signature from the live registry and each is asserted individually.

2. *Node's built-in `crypto.verify`, and a signature with `S` replaced by `S + L` is rejected, asserted directly.* Met. The case guards against passing vacuously, which matters here more than usual: an overflowing `S + L` would have left the test comparing a signature with itself and reporting success.

3. *No new runtime dependency.* Met. `git diff HEAD -- package.json yarn.lock` is empty and the only new import is Node's `crypto`.

4. *The corpus verifies.* Met on the stated basis. 4,579 of 4,579 bound, 19,209 of 19,209 satisfied, 19,208 of 19,209 attested, with the single failure traced to the registry's own data by three independent checks. The criterion as the task graph words it says zero; the honest number is one, and the basis for accepting it is recorded here and in the plan rather than rounded away.

5-11. *Jest, the negative cases, the empty signature list, the non-integer sequence number, the logo rule, the two-signature case, the checks and the suppressions.* All met.

Two things worth naming for what follows.

The `attested` field now reports whether a signature verified, independently of whether the policy bound. `task-010` must read `verified` and nothing else when it writes the column, and phase 4's advisory can read the other three to say which step failed. A caller that tested `attested` alone would be trusting the operator, which is the exact failure `verified` exists to prevent.

The two subjects in 4,579 that evaluate true against an empty key set remain the one item for the project owner. The full-population figure is now measured, 0.04 percent, and the behaviour is pinned by a test. Nothing downstream needs it resolved to proceed.

Summary: `verified` now means what the PRD says it means, and the corpus says so: every policy-bearing subject in the registry binds to its subject, every attestation satisfies its script, and all but one signature verifies. The one exception is a record the registry itself holds with a signature that does not check out, which the verifier correctly refuses. The primitive is Node's, which is strict, and nothing was added to the dependency tree.

Decision: approved
