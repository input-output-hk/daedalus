# Task 806 independent external audit and remediation

Completed: 2026-08-31

## Independence, scope, and result

`ExternalAudit806`, a separate read-only security-reviewer agent, independently audited Daedalus baseline `2360b40daae9b126f0063900c2eed40ea9dc966b`, the task remediation, and cardano-wallet commit `bc9b5b9c62cbf526a4806857f7692c3c9d2d2f5e` selected by Daedalus. The durable full report is `agent://ExternalAudit806`.

The review covered Electron guest containment and connection-bound egress; hostile guest-to-privileged IPC; sender, frame, origin, document-generation, route-lease, wallet, network, consent, and teardown authority; approval binding to immutable full transaction context and envelope bytes; exact Conway parsing and semantic review; CIP-30, CIP-95, and CIP-103; VKey and COSE verification; Ledger/Trezor exact-body adapters and main-owned activation; grants, collateral, session persistence/recovery, and privacy; and the pinned cardano-wallet W/G/P context, context token, signing, durable submission/replay, V6 migration, backup, and rollback.

Final independent re-review found **zero open critical or high findings**.

## Findings and remediation

| ID | Severity | Finding | Disposition |
| --- | --- | --- | --- |
| EXT806-PRIVACY-001 | Medium | cardano-wallet API detail logging suppressed only transaction context/submission, leaving witness, data-signature, and CIP-95 key-state material eligible for logs. | Closed at `bc9b5b9c62cbf526a4806857f7692c3c9d2d2f5e`. One shared route predicate now suppresses both accepted prefixes for all five dApp endpoints. Independent source re-review confirmed closure; focused Hspec passed 6 examples with 0 failures. |
| EXT806-TXI-002 | Medium | A whole-batch backend signing rejection has no authenticated item index, so the public CIP-103 diagnostic defaults to item zero. | Accepted residual. It cannot release partial witnesses or change approved bytes; fix only when the backend contract can return an authenticated item index or aligned per-item result. |
| EXT806-EVIDENCE-003 | Low | The V6 migration, `.v5.bak`, malformed/conflicting-row rollback, and old-pin restore lack a retained checked-in focused migration artifact. | Accepted release-evidence gap. The auditor reviewed migration implementation/registration; task-807 must retain exact release-candidate migration/rollback evidence before rollout. |

## Remediation regression

```text
cabal test cardano-wallet-unit:unit --builddir=dist-task203 \
  --test-option=--match --test-option='dApp API log privacy' -O0 -v0
```

Result: 6 examples, 0 failures. Coverage includes all five dApp route names under `/v2/wallets/:id` and normalized `/wallets/:id`, plus an unrelated-wallet-route negative control.

The first clean build attempt exposed a pre-existing stale dynamic-link cache. After selecting the retained isolated build directory, the dependency fetch also showed that upstream force-moved the referenced `cardano-ledger-read` branch while the immutable commit remained fetchable. Neither condition changes application source or the passing focused result.

## Reviewed identities

| Identity | Reviewed value |
| --- | --- |
| Daedalus audit baseline | `2360b40daae9b126f0063900c2eed40ea9dc966b` |
| cardano-wallet remediation and selected pin | `bc9b5b9c62cbf526a4806857f7692c3c9d2d2f5e` |
| cardano-wallet Nix narHash | `sha256-cPs4H/6+vRdo42w0R1Hljlg9I9jAOW7a8sX3C/qAuKM=` |
| `yarn.lock` SHA-256 | `53d018a89212037968c4de3f501ee90c1275e174edd28f915656d61bb92fe889` |
| `flake.lock` SHA-256 after remediation pin | `16d6677b331f586024068145dba5345266c000f658e78a2fb3745176748f48c8` |
| `package.json` SHA-256 | `ee4ee6c887e0a48d2a4b3a9b032ce1bf96e256f430b4a00658ef7c19eb62e21a` |
| catalog | revision 1, zero entries; source SHA-256 `cb399bc9bbaf3b497da02234d7ba903b0fe249b7be40f69660f9441d7d760961` |
| packaged activation | global/catalog/Diagnostics disabled; CIP-104/CIP-142 revision 0; certified hardware-row set empty |
| candidate package hashes | unavailable in the audited repositories; none invented. Exact release-candidate `.deb`/`.rpm` hashes and reruns remain task-807. |
| focused remediation evidence | cardano-wallet Hspec, 6 examples, 0 failures |

The Daedalus task commit containing this statement is the remediation-tree identity layered on the audited baseline. Task-807 must record its immutable commit and exact candidate package hashes before release.

## Residual accepted risks

- The existing Node-enabled trusted renderer remains privileged legacy debt; navigation and authenticated IPC controls contain it but do not make it a hostile guest.
- VKey signatures bind the exact body hash, not outer `isValid`; review displays that fact.
- Ledger or wallet state may change after review; execution rechecks the frozen authority/context contracts where required.
- Process exit can leave later authorized CIP-103 submissions unattempted; durable per-item reconciliation prevents duplicate accepted submission.
- The medium batch failure-index limitation remains release-visible.
- The V6 retained migration-evidence gap remains a task-807 release gate.
- Hardware product activation, preferred catalog entries, CIP-104, and CIP-142 remain disabled/empty as documented.
- Dependency/CVE currency, exact package hashes, remaining platform/package lifecycle evidence, and post-audit baseline change control remain task-807 gates.
