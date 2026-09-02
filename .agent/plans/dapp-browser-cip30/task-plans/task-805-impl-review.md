# Task 805 implementation review

Completed: 2026-08-31

## Scope and result

Reviewed guest authority and Electron lifecycle containment, transaction parsing/review/signing/submission integrity, software and hardware witness handling, grants and privacy, collateral, durable backend submission, extension conformance, and release gates. Four high-severity findings were remediated and independently re-reviewed. No critical or high internal finding remains open.

## Findings and remediation

| ID | Severity | Finding | Resolution |
| --- | --- | --- | --- |
| TASK805-GUEST-AUTH-001 | High | A trusted-renderer reload invalidated trusted IPC authority without synchronously revoking the active dApp route lease and guest. | Main-frame cross-document navigation now revokes the lease before guest teardown; regression coverage proves revocation precedes reload completion. Closed. |
| TASK805-TXI-001 | High | A software signer could return a valid but unrelated VKey witness because returned witnesses were not restricted to authenticated owned key hashes required by the transaction context. | The broker derives allowed and required hashes from authenticated `requiredProofs` and `owned_key` ownership, fails closed when a required proof lacks matching ownership, and rejects unrelated witnesses. Closed. |
| TASK805-TXI-002 | High | Single `signTx` and `submitTx` could reach consent for a transaction review marked unapprovable. | Both paths reject unapprovable reviews before consent, signing, or submission. Closed. |
| TASK805-HW-ACTIVATION-001 | High | Exact hardware `signTx` trusted renderer-supplied capability booleans and certification identifiers. | Main now requires the compiled connector matrix revision, vendor artifact and signTx row identity, static/product/physical gates, and the main-owned launch-policy allowlist before device access. No hardware transaction row is certified in the current release. Closed for the current release. |
| TASK805-TXI-003 | Medium | A whole-batch backend submission rejection does not identify the actual failing item; normalization defaults the public error index to zero. | Open. Preserve as a release-visible diagnostic limitation; resolve when the backend contract exposes an item index or per-item result. It does not release partial success or alter all-or-nothing signing. |

## Threat-model evidence

- Guest authority: route lease generation, trusted sender/frame checks, synchronous cross-document revocation, broker/consent teardown, and packaged sandbox/transport fixtures.
- Transaction integrity: exact body parsing and review, authenticated context snapshots, allowed/required witness-key binding, local VKey verification, unapprovable review rejection, exact-hash submit responses, and direct CIP-103 aligned rejection.
- Hardware: compiled certification matrix plus main-owned product policy gate all exact signTx access. Promotion remains blocked until a model/version-specific certified row is bound to main-observed device identity and physical evidence.
- Backend: the pinned cardano-wallet dApp context API, durable pending submission, replay, and privacy tests remain the authority; no cardano-wallet source change was required.
- Privacy and grants: canonical-origin-only persistence, fail-closed corruption handling, mode `0600`, structured-field omission, and log/analytics/crash redaction remain covered.
- Collateral: exact context review and shared Conway validation remain distinct from the soft collateral preference and its product lifecycle states.

Product limits are not security claims: preferred-catalog availability, Diagnostics availability, collateral readiness, hardware product activation, and external wallet interoperability remain release/product state. They do not weaken exact-byte, authority, witness, or submission invariants.

## Verification

- Focused Jest: 5 suites, 54 tests passed.
- Post-remediation broker Jest: 1 suite, 24 tests passed.
- Focused Cucumber: 12 scenarios, 50 steps passed.
- TypeScript: final `yarn compile` passed.
- Pinned backend: `cabal test cardano-wallet-api:dapp-context -O0 -v0` passed 35 examples.
- Broad internal review Jest selection: 30 of 31 suites passed, 295 of 296 tests passed. The sole failure was the frozen Trezor runtime package-tree identity after local `yarn install --frozen-lockfile`: file count matched but content hash differed. This is release-baseline evidence, not an application-source regression; task 807 must reproduce and record the exact release-candidate dependency tree rather than updating the frozen identity from this workstation.

## Remaining release gates

Internal gate 12 is closed. Release remains blocked by the external audit, current Electron/Chromium security review, exact release-candidate baseline, physical hardware promotion evidence, post-audit change control, and the platform/matrix coverage explicitly deferred by task 802. The dormant hardware batch helper still lacks production integration evidence; task 607/708 product activation remains gated, so this is not a current reachable signing path.

## Task 807 change-control re-review

Completed: 2026-09-02

The task-807 dependency, Electron, Nix/package-lifecycle, hardware-manifest, and final-artifact delta replayed the affected internal review scope. Exact CBOR/era coverage passed 46 examples, hardware capability/certification coverage passed 15 examples, trusted IPC and dApp session/egress/sandbox harnesses passed, and every supported final Linux package passed the hostile matrix. No critical or high internal security or transaction-integrity issue remains. The medium whole-batch failure-index diagnostic limitation is unchanged; hardware product activation remains disabled. Exact inputs, package hashes, platform evidence, and the macOS-only operator waiver are recorded in [the task-807 release candidate](../research/10-task-807-release-candidate.md).
