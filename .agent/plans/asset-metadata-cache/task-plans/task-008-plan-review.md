Planner: Iteration 1
Timestamp: 2026-09-14T22:15:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-008.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. Every function is pure and every fixture is a real captured registry entry.
- Two new files. `task-009` extends the same two with step three.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the three classes at `:399-412`, the reference evaluator at `:414-432`, the time-lock rule at `:434-440` and the worked digests at `:762-778`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-008` entry plus `task-009` and `task-010`.
- The reference implementation's `evaluatePolicy`, named in the PRD at `token-metadata-creator/src/Cardano/Metadata/Types.hs:253-287`.

Repo-Verified Findings Used To Shape The Plan:
- Over 600 captured mainnet subjects, 405 carry a policy. The two-byte strip reproduces the subject's policy id for 405 of 405; hashing the whole policy field reproduces it for 0 of 405.
- The outer value is a two-element array in every case, its first element 1 in 370 entries and 0 in 35, both one byte.
- Script tags occurring: 0 appears 413 times, 1 appears 374, 3 appears once, 4 twice, 5 appears 370. Tag 2 does not occur.
- Maximum script nesting depth is 2 and the largest script holds 10 nodes.
- Evaluating each bound script against the key hashes that signed each property returns true for 1,682 of 1,682 property-level evaluations across 405 of 405 entries.
- Exactly one entry evaluates to true against an empty key set: `atLeast 2 of [ timeBefore 600, sig, timeAfter 500 ]`, where the two time-lock branches alone meet the threshold.
- `blake2b` is already imported in the main process at `source/main/governance/AnchorVerificationService.ts:1`; `cbor` at `source/main/utils/restoreKeystore.ts:1`.

Planned Approach:
- A closed union for the script, a validating decoder with a depth cap, the digest taken before the decode, the six-case reference evaluator, and one verdict function that performs both steps.

Scope Guard / Self-Review:
- No signature verification, no database, no network, no clock and no Plutus.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T22:24:00Z

Three gaps, in descending severity.

1. Nothing validates a public key before it is hashed into the attesting key set. `Buffer.from(x, 'hex')` truncates at the first character pair it cannot read rather than refusing, which `task-005` already documented for asset names and `task-001` before it. Here the consequence is quieter than usual and therefore easier to leave: a malformed public key yields a well-formed 28-byte digest that simply will not match, so the entry fails closed and nothing looks wrong. It is still a key set containing a hash of something that is not a key, and the same string is handed to ed25519 verification in `task-009`, where a truncated key is a different kind of wrong. An ed25519 public key is 32 bytes; a value that is not 64 hex characters is not one, and belongs out of the set rather than in it as a digest of a prefix.

2. The hex comparison is unstated on both sides. The subject's policy id is a slice of a string that arrives from the registry, the computed digest is produced by this code, and nothing in the plan says the two are normalised before being compared. Hex from an external source is not guaranteed lower case. There is also no rule for a subject shorter than 56 hex characters: slicing it yields a short string that compares unequal, which is the right outcome by accident rather than by decision, and gives a reason of `digest-mismatch` where `malformed` is the true one.

3. The `atLeast` boundaries are missing from the verification plan. `atLeast 0` is there, and `atLeast 2 of 3` from both sides, but not the two cases that decide whether the threshold is compared correctly: a required count higher than the number of sub-scripts, which must be false however many are satisfied, and a negative or non-integer threshold, which the decoder should refuse rather than the evaluator interpret. The decoder is described as checking for "a non-negative integer threshold", so the second is a decoder case that no assertion drives.

Nothing else in the plan is wrong. Hashing before decoding, and saying why, is the right order and is worth more than the depth cap it makes largely redundant: it means the decoder is only ever reached with bytes that already hash to the subject's own policy id.

The treatment of the `atLeast` entry that needs no key is the right shape. It is measured, priced against the alternative, pinned by a test, and handed to the owner rather than improved in passing. An implementation that quietly added "and at least one signature branch must be satisfied" would have been defensible and wrong, because it would disagree with the registry about what a valid attestation is and the disagreement would be invisible.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T22:32:00Z

Plan Updates In Response To Critique:
- Added public key validation: exactly 64 hex characters, or the key is left out of the attesting set rather than contributing a digest of a truncated prefix, with a case.
- Stated the comparison rule: both sides lower-cased before comparison, and a subject that is not at least 56 hex characters fails with `malformed` rather than falling through to `digest-mismatch`. Two cases.
- Added the two missing `atLeast` boundaries: a threshold above the number of sub-scripts, and a negative or non-integer threshold, which the decoder refuses.

Resulting Approved Plan Shape:
- Two new files. A validating decoder with a depth cap, the digest taken before the decode, the six-case reference evaluator transcribed, and one verdict function that performs both steps and names them.

Scope Guard / Self-Review:
- The revision adds input validation and three assertions. It changes nothing about what the evaluator computes.

Outcome: Canonical task plan revised after critique and approved for build execution
