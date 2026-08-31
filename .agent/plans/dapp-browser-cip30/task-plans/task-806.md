# Task 806: Independent External Audit And Remediation

## Scope

Audit Daedalus baseline `2360b40daae9b126f0063900c2eed40ea9dc966b` and the exact cardano-wallet commit selected by the Daedalus pin. The independent review covers Electron containment; hostile guest-to-privileged IPC authority; route, origin, document, session, wallet, and network binding; approval binding to exact transaction context and bytes; Conway parsing and semantic review; CIP-30, CIP-95, and CIP-103; VKey and COSE verification; Ledger and Trezor adapters and activation gates; grants, collateral, persistence, recovery, and privacy; and cardano-wallet W/G/P context, durable submission, V6 migration, backup, and rollback.

Dependency/CVE currency and final release-candidate package certification remain task-807. Product-disabled catalog, hardware, CIP-104, and CIP-142 paths are reviewed as fail-closed gates, not treated as production capability evidence.

## Severity

- **Critical:** demonstrated privilege escape, unauthorized disclosure/signing/submission, approval-byte substitution, containment bypass, or incorrect/double durable submission.
- **High:** credible fail-open authority, signer-result, persistence, or activation-gate defect.
- **Medium:** bounded privacy, integrity-diagnostic, or defense-in-depth defect without a critical/high exploit.
- **Low:** evidence or maintainability gap without a demonstrated exploit.

Task 806 may complete only after independent re-review confirms no open critical/high finding. Every remediation must have a focused regression.

## Execution

1. Commission the separate `ExternalAudit806` security-reviewer and preserve its immutable report at `agent://ExternalAudit806`.
2. Review the exact Daedalus and pinned cardano-wallet identities and every scope boundary above.
3. Record each finding with severity, root cause, attack path, source evidence, remediation, regression, and disposition.
4. Fix findings at the shared authority, logging, parser, or persistence boundary; re-review every fix independently.
5. Run only focused regressions for remediated paths and retain exact commands/results.
6. Record residual accepted risks and exact source, dependency-lock, backend-pin, catalog, package-evidence, and test identities in `task-806-impl-review.md`.
7. Update the PRD and task graph only after closure confirms zero open critical/high findings.
