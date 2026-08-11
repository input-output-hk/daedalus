Implementation: Iteration 1
Timestamp: 2026-08-11T17:02:46Z

Changes made:
- Added immutable pinned-ledger exact-CBOR fixtures, fixed envelope/body/witness/isValid/auxiliary/output/collateral spans, true Blake2b-256 evidence, a machine-readable Conway/Dijkstra/CBOR/protocol-context manifest, and targeted strict-policy tests.
- Added one parameterized SDK probe and normalized results for installed `@cardano-sdk/core@0.41.4` and isolated candidate `0.47.0`.
- Recorded the retain-0.41.4 dependency decision, conditional Conway readiness, blocked Dijkstra readiness, downstream ownership, and durable reproduction evidence.

Files touched:
- `.agent/plans/dapp-browser-cip30/task-plans/task-004.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-004-plan-review.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-004-impl-review.md`
- `.agent/plans/dapp-browser-cip30/research/04-exact-cbor-era-coverage.md`
- `scripts/exact-cbor-sdk-probe.cjs`
- `source/common/cardano/exactCborValidation.spec.ts`
- `source/common/cardano/fixtures/exact-cbor/manifest.json`
- `source/common/cardano/fixtures/exact-cbor/conway-regression.json`
- `source/common/cardano/fixtures/exact-cbor/alonzo-golden.json`
- `source/common/cardano/fixtures/exact-cbor/sdk-0.41.4-results.json`
- `source/common/cardano/fixtures/exact-cbor/sdk-0.47.0-results.json`

Verification run:
- Focused Jest: 11 tests passed.
- `yarn compile`: passed.
- Focused ESLint and direct Prettier checks: passed.
- Installed and candidate SDK probe regeneration matched committed results byte-for-byte.
- Fixture JSON parsing, candidate tarball SHA-1/SRI, temporary lock SHA-256, and `cbor-diag --to diag --seq` decoding passed.

Deviations from the approved plan:
- Pinned package tags superseded planning-time moving-branch Dijkstra discovery: the accepted pinned inventory adds body keys 23, 25, and 26 and protocol-update keys 34 through 37, rather than broader provisional sets. This correction is documented in the canonical outcome, manifest, and research.
- The historical Alonzo golden is span-only evidence and intentionally fails Conway because it includes retired body key 6; it is not represented as a Conway-positive fixture.
- No production cursor/model, package update, sibling change, or exhaustive fuzz corpus was introduced.

User interaction required: no.

Outcome: Implementation is complete for iteration 1 and ready for comprehensive code review.

Code Review: Iteration 1
Timestamp: 2026-08-11T17:07:07Z

The implementation has the correct fail-closed direction, truthful era statuses, and narrow production scope, but the committed evidence does not yet satisfy the approved source-inventory, policy-coverage, provenance, or SDK-reproducibility gates.

Blocking findings:
1. **Critical: the claimed source-derived Conway/Dijkstra inventory is incomplete and its test is tautological.** `source/common/cardano/fixtures/exact-cbor/manifest.json:116-180` records only summary lists, not every body/witness/output field with required/optional status, cardinality, union/discriminant alternative, tag, embedded-CBOR location, and accepted legacy form required by the approved plan. Dijkstra coverage also omits pinned deltas such as sub-transaction body key 24, auxiliary-data key 5, protocol-version changes, and detailed guard/sub-transaction unions. `source/common/cardano/exactCborValidation.spec.ts:348-419` merely compares the manifest with another hand-written list in the same test; it never extracts from or compares against the pinned CDDL, so source omissions cannot fail the test. Commit a complete machine-readable inventory and a reproducible source-derived extraction/comparison against the pinned Conway and Dijkstra artifacts.
2. **High: the frozen wire policy is mostly declarative and the strict-policy test helper does not enforce the policy it claims to prove.** The seven cases at `manifest.json:326-369` omit targeted evidence for definite/indefinite container exceptions, non-minimal values and lengths, reordered maps, exact versus semantic duplicates, untagged sets, tag 24 including embedded trailing data, other permitted/wrong-location tags, and most accepted legacy forms. `validateStrictEnvelope` at `exactCborValidation.spec.ts:298-318` checks only root arity, body type, body keys, and one input-tag condition; it does not validate witness type, boolean `isValid`, auxiliary-data shape, tag locations, embedded full consumption, or source-specific container restrictions. The scanner also accepts nested indefinite string chunks even though CBOR requires indefinite strings to contain definite chunks. Add the approved targeted policy vectors and make their strict oracle enforce the relevant decision without expanding into task-800 fuzz breadth.
3. **High: the positive span corpus and independent-offset evidence are incomplete.** Both committed fixtures use Alonzo array outputs; there is no ledger-proven Babbage map-output vector despite `task-004.md:133,175` requiring both output forms. There is likewise no positive untagged-set distinction. The only explanation for offsets is that they came from unspecified “byte annotation” (`research/04-exact-cbor-era-coverage.md:59-61`), with no committed annotation, command, or independently reviewable extraction metadata. The offsets and Blake2b-256 hashes reproduce through the test, but that scanner is the same mechanism asserting the expected boundaries. Add the missing narrow ledger-proven forms and record a reproducible independent offset derivation.
4. **High: candidate SDK evidence is not reproducible or identity-verified to the approved standard.** The reproduction section at `research/04-exact-cbor-era-coverage.md:150-159` assumes `/tmp/cardano-sdk-core-0.47.0` already exists and provides no download, SHA-1/SRI verification, extraction, exact dependency installation, or lock generation commands. The temporary lock is represented only by a hash at `manifest.json:397`; neither that lock nor an equivalent complete immutable dependency manifest is committed. The candidate path was absent during review, so the documented command could not run. The harness checks only `version` and `gitHead` (`scripts/exact-cbor-sdk-probe.cjs:21-30`), while `strictFullConsumption` is inferred from the mutation name rather than measured (`scripts/exact-cbor-sdk-probe.cjs:98`). Its `representedFields` uses all `Object.keys(core.body)`, including undefined fields; for the installed positive fixture only seven of the reported twenty fields are actually defined. Supply end-to-end candidate setup and integrity verification, preserve the complete dependency identity, measure full consumption, and normalize actual represented values before relying on the comparison for the dependency decision.
5. **Medium: the canonical plan contains stale Dijkstra conclusions that contradict the accepted implementation artifacts.** `task-004.md:69` still claims body keys 23-27 and protocol parameters 34-39, and `task-004.md:106` still mentions later-source pledge and pool-margin additions. The implementation evidence at `task-004.md:240`, research, and manifest instead correctly use pinned keys 23, 25, 26 and protocol-update keys 34-37. Resolve this internal documentation drift before approval while preserving the intended post-approval PRD/tracker synchronization boundary.

Non-blocking observations:
- Fixture SHA-256 values, fixed spans, and true 32-byte Blake2b results reproduce; the focused Jest suite passes all 11 tests.
- The immutable ledger tags resolve to the recorded commits, and the Conway regression bytes match the pinned source extraction.
- `package.json` and `yarn.lock` are byte-identical to `HEAD`; retaining `@cardano-sdk/core@0.41.4` remains a valid possible outcome after the probe evidence is corrected.
- Conditional Conway readiness and Dijkstra `unsupported/readiness-blocked` are represented without claiming backend, parser, or product support.
- The exact-chain-point `W/G/P` protocol-context authority and downstream owner matrix have the correct architecture and make no arbitrary live-network parameter claim.
- No production parser/model, backend, IPC, renderer, hardware, Nix, translation, or exhaustive fuzz implementation was introduced. No sensitive wallet, network, key, or origin data appears in the artifacts.

Approval bar:
Provide a complete source-derived Conway and Dijkstra inventory with a real pinned-source comparison; add the missing narrow policy, Babbage-output, and representation probes; make the test-local strict oracle accurately enforce those cases; preserve independently reproducible span-offset evidence; make candidate SDK setup, integrity, dependency identity, full-consumption measurement, and semantic representation results reproducible; and remove the stale Dijkstra contradictions from the canonical plan. Approval does not require a dependency upgrade, production parsing, exhaustive semantic fixtures, fuzzing, live protocol-parameter fetching, or PRD/tracker completion synchronization.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-08-11T17:23:57Z

Changes made:
- Added `scripts/extract-cardano-cddl-inventory.cjs` and generated `source-inventory.json` with complete relevant pinned CDDL definition blocks plus structured body, witness, output, auxiliary, sub-transaction, and protocol-parameter maps; regeneration compares byte-for-byte.
- Expanded the strict oracle and manifest with 15 targeted policy vectors and complete envelope/witness/isValid/auxiliary/output/set checks; fixed nested indefinite-string chunk rejection.
- Added deterministic Babbage map-output and untagged body-set fixtures derived from the pinned Conway regression, reproduced in Jest, and accepted as Conway by `cardano-cli debug transaction view`. Added independently hashed `cbor-diag` span annotations.
- Committed the complete candidate npm lock and added an empty-directory candidate setup script with `npm ci`, tarball SHA-1/SRI, version, git-head, and lock verification. The SDK probe now measures one-root consumption and reports only defined body fields.
- Corrected stale canonical Dijkstra claims to pinned top-level keys 23/25/26, sub-transaction key 24, auxiliary key 5, protocol encoding changes, and protocol-update keys 34-37.

Files additionally touched:
- `scripts/extract-cardano-cddl-inventory.cjs`
- `scripts/setup-exact-cbor-sdk-candidate.cjs`
- `source/common/cardano/fixtures/exact-cbor/source-inventory.json`
- `source/common/cardano/fixtures/exact-cbor/span-annotations.json`
- `source/common/cardano/fixtures/exact-cbor/conway-babbage-outputs.json`
- `source/common/cardano/fixtures/exact-cbor/conway-untagged-sets.json`
- `source/common/cardano/fixtures/exact-cbor/sdk-0.47.0-package-lock.json`
- Existing task-004 test, manifest, SDK results, research, probe, and canonical plan were revised.

Verification run:
- Focused Jest: 32 tests passed.
- `yarn compile`, focused ESLint, direct Prettier, script syntax checks, JSON parsing, and git whitespace checks passed.
- Pinned CDDL inventory regeneration matched the committed output byte-for-byte.
- Candidate setup succeeded from a new absent directory and verified lock, package, tarball, and installed identities.
- Installed/candidate probe regeneration matched committed results byte-for-byte; trailing-root measurement is false and semantic field lists contain only defined values.
- All four `cbor-diag` annotation hashes reproduced. Babbage and untagged text envelopes decoded successfully through cardano-cli 11 Conway transaction view.

Deviations from the approved plan:
- The narrow derived Babbage/untagged fixtures are decoder-accepted deterministic transformations of the immutable regression rather than additional upstream golden files; provenance, transformation, cardano-cli revision, exact tx IDs, and reproduction are recorded.
- No production parser/model, dependency update, sibling change, or exhaustive fuzz corpus was introduced.

User interaction required: no.

Outcome: All Code Review iteration-1 blockers are addressed and implementation iteration 2 is ready for focused review.

Code Review: Iteration 2
Timestamp: 2026-08-11T17:27:46Z

Iteration 2 resolves candidate reproducibility, positive span evidence, corrected Dijkstra facts, and dependency/scope integrity, but two iteration-1 blockers remain incomplete.

Blocking findings:
1. **Critical: the generated pinned-source inventory remains incomplete.** `scripts/extract-cardano-cddl-inventory.cjs:19-82` extracts a manually selected definition list rather than the complete dependency closure required by the approved plan. Consequently, `source-inventory.json` omits source definitions for certificate discriminants, governance actions, native-script alternatives, Plutus data, metadata, withdrawals, mint, required signers, and witness structures even though these definitions exist in the pinned Conway CDDL. The certificate and governance tag lists in `manifest.json:146-166` remain hand-maintained and are not compared with source-derived discriminants. Regeneration is byte-for-byte reproducible, but it can reproduce the same omission and therefore does not satisfy iteration-1 blocker 1.
2. **High: targeted strict-policy evidence still does not cover the approved policy families accurately.** `manifest.json:442-532` has no non-minimal length case, source-location rejection for tag 24 or other permitted tags, empty nonempty-set case, or probes for flat/map redeemers, raw metadata, and Allegra auxiliary arrays. The generic validators in `exactCborValidation.spec.ts:456-534` validate standalone tag shapes rather than CDDL-declared locations, while `validateStrictEnvelope` does not validate redeemer structure or those legacy alternatives. This leaves iteration-1 blocker 2 unresolved; exhaustive task-800 breadth is not required.

Non-blocking observations:
- The pinned Conway/Dijkstra inventory regeneration matched the committed artifact byte-for-byte.
- Babbage map-output and untagged-set fixtures were accepted by `cardano-cli 11.0.0.0`; their transaction IDs and exact spans are consistent.
- All four `cbor-diag` annotation hashes reproduced, providing independent span evidence.
- Candidate setup succeeded from an absent directory and verified the lock hash, tarball SHA-1/SRI, version, and git head. Installed and candidate probe results regenerated byte-for-byte, measured root consumption, and report only defined fields.
- Corrected Dijkstra keys and protocol facts are consistent across the canonical plan, manifest, inventory, and research.
- Focused Jest passed all 32 tests and `yarn compile` passed.
- `package.json` and `yarn.lock` remain unchanged. Scope boundaries and conditional Conway/blocked Dijkstra status remain correct.
- PRD/tracker completion synchronization correctly remains deferred to the post-approval Scribe pass.

Approval bar:
Complete the generated source inventory so pinned CDDL union/discriminant alternatives and relevant referenced definitions cannot be omitted silently, and add the remaining narrow policy vectors with location-aware enforcement for the policy families explicitly identified in iteration 1. No dependency upgrade, production parser, exhaustive semantic corpus, fuzz suite, live protocol-parameter fetch, or PRD/tracker completion synchronization is required.

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-08-11T17:33:13Z

Changes made:
- Replaced the manually selected CDDL definition list with deterministic extraction of every top-level definition from both pinned era sources. Added source-derived certificate, governance-action, and native-script discriminants, manifest comparisons, and explicit coverage checks for Plutus data, metadata, withdrawals, mint, required signers, and witness structures.
- Added narrow policy vectors for non-minimal lengths, empty non-empty sets, tag 24 at valid and invalid output locations, tag 30 at an invalid output-value location, flat-array and map redeemers, raw metadata, and Allegra auxiliary arrays.
- Tightened the test-local strict oracle with location-aware transaction-output, redeemer, auxiliary-data, and non-empty-set validation.

Verification run:
- Focused Jest: 41 tests passed.
- `yarn compile`: passed.
- Focused ESLint, direct Prettier, script syntax checks, fixture JSON parsing, and git whitespace checks passed.
- Pinned Conway/Dijkstra inventory regeneration matched the committed artifact byte-for-byte.

Deviations from the approved plan:
- None. No production parser/model, dependency update, sibling change, or exhaustive fuzz corpus was introduced.

User interaction required: no.

Outcome: Both Code Review iteration-2 blockers are addressed and implementation iteration 3 is ready for focused review.

Code Review: Iteration 3
Timestamp: 2026-08-11T17:34:58Z

Iteration 3 adds the requested policy cases and source comparisons, and the reported verification reproduces, but the two iteration-2 blockers are not yet fully resolved because the extractor still misses a real top-level CDDL definition and the strict location-aware policy does not compose non-empty-set and embedded-CBOR checks at transaction locations.

Blocking findings:
1. **Critical: the generated inventory still does not extract every top-level definition.** `scripts/extract-cardano-cddl-inventory.cjs:17-34` requires a generic definition name and its closing `>` to occur on one line. Both pinned CDDL files define `constr<a0` and close the generic header on the following line before `> =`; the generated `source-inventory.json:49` therefore folds `constr<a0>` into the `plutus_data` string instead of recording it as its own definition. Byte-for-byte regeneration and the presence checks at `source/common/cardano/exactCborValidation.spec.ts:738-748` reproduce rather than detect this omission, so the implementation and research claim that every top-level definition is preserved remains false.
2. **High: location-aware set and tag-24 enforcement remains incomplete.** `validateStrictEnvelope` at `source/common/cardano/exactCborValidation.spec.ts:532-545` applies the same shape-only rule to body key 0 (`set`) and keys 13/18 (`nonempty_set`) and accepts an empty array at the latter locations. The `empty-nonempty-set` vector in `manifest.json:503-508` exercises only the standalone `nonempty-set` validator, not a transaction-body location. Likewise, `validateTransactionOutput` at `exactCborValidation.spec.ts:424-450` checks that datum/script-reference tag 24 wraps a byte string but does not require the payload to contain exactly one complete embedded item; the standalone embedded-CBOR validator is not composed into valid output locations. Thus an empty collateral/reference-input set or an inline datum/script reference with embedded trailing CBOR can pass the strict location-aware oracle despite the frozen policy.

Non-blocking observations:
- Manifest certificate and governance-action tags are now compared with generated source discriminants, native-script discriminants are derived, and the named Plutus-data, metadata, withdrawal, mint, required-signer, and witness definitions are present.
- The new non-minimal length, wrong-location tag 24/tag 30, redeemer-form, raw-metadata, and Allegra auxiliary-array probes cover the requested narrow families apart from the composition gaps above.
- Focused Jest passed all 41 tests; `yarn compile`, script syntax checks, `git diff --check`, and pinned Conway/Dijkstra inventory regeneration passed independently.
- Candidate/installed SDK evidence, fixture spans and provenance, dependency retention, era statuses, and task scope show no new regressions or support overclaims.

Approval bar:
Make top-level definition extraction handle the pinned multiline generic header and add a completeness assertion that would fail for this omission. Enforce and probe non-empty sets at Conway body keys 13/18 and exact embedded-CBOR consumption at valid output datum/script-reference locations. No broader semantic parser, exhaustive corpus, dependency update, or PRD/tracker synchronization is required.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-08-11T17:38:03Z

Changes made:
- Extended top-level CDDL extraction to support multiline generic headers and regenerated the inventory with `constr<a0>` as a distinct definition. Added an explicit completeness assertion for that pinned-source case.
- Composed non-empty cardinality enforcement into Conway body keys 13 and 18 and added full-envelope rejection vectors for empty collateral and reference-input sets.
- Composed exact embedded-CBOR consumption into inline-datum and script-reference output locations, with valid and embedded-trailing vectors for both location families.

Verification run:
- Focused Jest: 46 tests passed.
- `yarn compile`: passed.
- Focused ESLint, direct Prettier, fixture JSON parsing, and pinned inventory regeneration/comparison passed.

Deviations from the approved plan:
- None. No production parser/model, dependency update, sibling change, or exhaustive fuzz corpus was introduced.

User interaction required: no.

Outcome: Both Code Review iteration-3 blockers are addressed and implementation iteration 4 is ready for focused review.

Code Review: Iteration 4
Timestamp: 2026-08-11T17:39:58Z

Iteration 4 fully resolves both iteration-3 blockers without introducing regressions, scope drift, or false readiness/support claims.

Blocking findings:
None.

Non-blocking observations:
- Multiline generic CDDL headers are now normalized and extracted as distinct top-level definitions. Both generated era inventories contain `constr<a0>`, and the focused inventory test explicitly requires the Conway definition.
- The strict full-envelope oracle now distinguishes body key 0 cardinality from non-empty body keys 13 and 18. Dedicated empty collateral-input and reference-input envelope cases reject as required.
- Inline-datum and script-reference tag-24 locations now call the shared exact embedded-CBOR validator. Positive and embedded-trailing-item probes cover both declared output locations, while wrong-location rejection remains intact.
- Focused Jest passed all 46 tests; `yarn compile`, focused ESLint, direct task-scoped Prettier, JSON parsing, script syntax and whitespace checks, and deterministic pinned-source inventory regeneration all passed independently. The repository-wide `yarn prettier` wrapper still reports unrelated pre-existing files because it prepends `**/*.*`; the required direct focused check passes.
- Fixture provenance, SDK evidence and dependency retention, conditional Conway readiness, blocked Dijkstra status, and downstream task ownership remain consistent. No production parser/model or unrelated artifact was introduced.

Approval bar:
Satisfied. The complete pinned CDDL inventory now handles the multiline generic definition, and strict policy evidence enforces non-empty input sets and exact tag-24 embedded consumption at their declared transaction locations.

Decision: approved

