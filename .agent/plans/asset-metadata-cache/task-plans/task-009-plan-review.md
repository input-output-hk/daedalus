Planner: Iteration 1
Timestamp: 2026-09-14T23:15:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-009.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`. The corpus gate was run before the plan was approved, not after the code was written.
- Two existing files extended, no new file, no dependency change.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the payload at `:780-795`, the logo exception at `:797-802`, the primitive table at `:804-812` and the strictness table at `:814-826`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-009` entry and `task-010`.
- The registry's own implementation, named in the PRD at `token-metadata-creator/src/Cardano/Metadata/Types.hs`.

Repo-Verified Findings Used To Shape The Plan:
- The payload construction verifies live for five properties of `c76ef54…42544544` using only `blake2b`, `cbor` and Node's `crypto.verify`.
- The logo of that subject, 77,392 base64 characters, verifies from the decoded bytes as a CBOR byte string and not from the base64 text. The decoded bytes begin with a PNG header.
- A real signature with `S` replaced by `S + L` is rejected by Node's verifier.
- The corpus gate, run over the registry's whole mapping list at commit `363982b999060874f80486c2758a8a19d3b5f78a`: 7,977 subjects asked and answered, 4,579 carrying a policy, 4,579 passing step one, 19,209 property attestations of which 19,209 pass step two and 19,208 pass step three.
- The single step-three failure is a defect in the registry's data. The mapping file in the repository at that commit carries the identical signature, the subject's three sibling properties verify under the same key, and no alternative property name, sequence number or value reproduces it.
- Two subjects in 4,579 evaluate to true against an empty key set. 104 properties carry more than one signature.

Planned Approach:
- One payload function with a single `logo` branch, one ed25519 verifier over a DER-wrapped raw key, and one function that runs all three steps for one property.

Scope Guard / Self-Review:
- No new dependency, no database, no network in the module, no `cardano-crypto.js`, no re-derivation of the payload by search.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T23:24:00Z

Three gaps, in descending severity.

1. The result object lies when it short-circuits. The plan computes `attested` only when `bound` and `satisfied` are both true, and then reports `attested: false` for a property whose signature was never checked. A field named for a fact, carrying the value that means the fact is absent, when what actually happened is that nobody looked, is the kind of thing a later reader acts on: phase 4's advisory has to say why a property is not verified, and "the signature did not verify" and "we never checked the signature" are different sentences. The cost of computing all three independently is one ed25519 verification on data that already failed, which the corpus run puts at microseconds. `verified` stays the conjunction, so nothing about the guarantee changes. Compute all three.

2. The acceptance criteria claim the corpus gate passed with zero failures while the findings section records one. Putting the exception only in Risks leaves the criteria section stating something the same document contradicts two pages later. The criterion should say what was measured, 19,208 of 19,209, and say on what basis the one is accepted, so that a reader who checks only the criteria gets the true number.

3. Two input classes have no case. A property with an empty `signatures` array must be `attested: false`, and it is the whole unattested class, which is most of the registry for subjects that carry no policy. And `sequenceNumber` is not validated: a non-integer encodes as a CBOR float rather than an integer and produces a different payload. Both fail closed, which is why neither is urgent and why neither will ever be noticed if it is wrong.

Nothing else in the plan is wrong. Running the corpus gate before approving the plan rather than after writing the code is the right order, and it paid for itself: the single failure is now a finding with an established cause rather than something to be explained away at the end under pressure to declare the task done.

The observation that a verifier which accepted everything would be the failure mode to fear is worth keeping in the plan rather than trimming. A run reporting 19,209 of 19,209 would be weaker evidence that the check is real.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T23:31:00Z

Plan Updates In Response To Critique:
- All three steps are now computed independently and each field reports its own fact. `verified` remains the conjunction and remains the only field a caller should test.
- Rewrote acceptance criterion 4 to state the measured counts and the basis on which the single failure is accepted, instead of claiming zero.
- Added the empty-signature-list case and a sequence number that is not an integer, which the payload function refuses.

Resulting Approved Plan Shape:
- Two files extended. A payload function with one branch, a strict ed25519 verifier behind a length check on both inputs, and one per-property function reporting three independent facts and their conjunction.

Scope Guard / Self-Review:
- The revision removes a short-circuit, corrects a criterion to match the evidence, and adds two input checks. It changes nothing about what is verified.

Outcome: Canonical task plan revised after critique and approved for build execution
