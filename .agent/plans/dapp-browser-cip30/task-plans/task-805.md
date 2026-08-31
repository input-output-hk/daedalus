# Task 805: Internal Security And Transaction-Integrity Review

## Scope

Review baseline: Daedalus parent `576650300b6dee5e4328b919b7049ec528483f4d`; pinned and sibling cardano-wallet `0cbd4618f5b3ac76bcee52c57a7cd6067a87408e`. The review covers the hostile guest, connection-bound egress, IPC and broker authority, grants and consent, exact transaction context/review/signing/submission, CIP-103 ordering, Ledger/Trezor adapters and activation, collateral, backend durability, privacy, and release gates.

The review does not promote a catalog entry, hardware row, launcher switch, or production guest. External audit, dependency/CVE review, release-candidate package baselining, and post-pilot revalidation remain tasks 806, 807, and 903-a.

## Severity

- **Critical:** demonstrated privileged escape, unauthorized signing/submission, review-byte substitution, private-network/transport bypass, sensitive-key disclosure, or incorrect/double durable submission.
- **High:** credible fail-open authority, signer-result, containment, backend-consistency, or activation-gate defect.
- **Medium:** bounded contract or defense-in-depth defect without a confidentiality/integrity bypass.
- **Low:** non-material clarity or evidence defect.

Task 805 may complete only with no open critical/high finding. Product-disabled code is not a security control unless a main-owned fail-closed gate makes activation impossible.

## Threat-model evidence matrix

| Invariant | Code evidence | Test or retained evidence |
|---|---|---|
| Connection-bound HTTPS/WSS and no bypass transport | `source/main/dapp/DappEgressPolicy.ts`, `DappSessionPolicy.ts`, `urlPolicy.ts` | their focused specs; `tests/security/dapp-guest/main.js`; task-802 installed-package record |
| Exact hostile top frame, document, origin, session, and teardown | `DappBrowserManager.ts`, `DappRouteLease.ts`, `source/main/ipc/dappBrowser.ts` | manager/lease/controller specs; task-802 harness |
| Dedicated guest gateway and authenticated privileged IPC | `source/main/preloads/dapp.ts`, `source/main/cip30/Cip30Broker.ts`, `source/main/ipc/lib/MainIpcChannel.ts`, privileged IPC manifest | preload/gateway/broker/manifest specs; trusted-IPC harness |
| Main-owned immutable consent and executor requests | `ConsentCoordinator.ts`, `Cip30Broker.ts`, `source/main/ipc/cip30Wallet.ts` | coordinator and broker specs |
| Exact backend W/G/P context and result binding | `source/common/cardano/transactionContext.ts`; pinned `TransactionContext.hs` and `Server.hs` | context fixtures/specs; task-200–209 evidence |
| Exact parser, semantic review, witnesses, COSE, and submission envelope | `source/common/cardano/{transactionEnvelope,transaction,witnessSet,cip8,cose}.ts`, `Cip30Broker.ts` | exact-CBOR, semantic, differential/fuzz, witness, CIP-8, and broker specs |
| CIP-103 caller order, coherent overlay, staged signing, attempt-all submission | `source/common/cardano/transactionOverlay.ts`, `source/main/cip103/`, `source/main/cip30/extensions/cip103.ts` | overlay/context/software/hardware/submit specs and Cucumber journeys |
| Hardware exact-body and returned-proof validation | Ledger/Trezor adapters and `HardwareWalletService.ts` | adapter/service/capability specs; scoped task-607 Ledger evidence |
| Atomic grants/collateral, memory-only capabilities/signing, privacy | `GrantRepository.ts`, `SessionStore.ts`, `CollateralPreferenceStore.ts`, logging boundaries | repository/collateral/privacy specs; task-803 evidence |
| Packaged Chromium containment | sandbox availability/canary and package identity checks | task-005-b matrix plus task-802 Ubuntu 24.04 installed-package evidence; final release-candidate rerun remains task-807 |

## Product and lifecycle limitations

The 64 KiB request limit, 50-item CIP-103 limit, 100-entry page limit, and five-minute inactivity timeout are product/availability bounds, not signing or isolation proofs. Queue pressure, rejection, restart, and guest closure are ordinary lifecycle conditions. An already authorized submission continues against its frozen wallet/network while stale guest delivery is suppressed. CIP-104 remains omitted, CIP-142 remains packaged-policy gated, and uncertified hardware rows remain impossible to activate through the empty compiled certified-row set. Task-607 evidence covers only the recorded Nano X/Cardano app 7.3.0 cases; it is not Ledger-family or Trezor certification.

## Execution

1. Review each matrix row against source and existing evidence.
2. Record findings in `task-805-impl-review.md` with severity, root cause, remediation, regression, and disposition.
3. Fix every critical/high finding at the shared authority boundary.
4. Run focused Jest for affected boundaries, TypeScript compilation, focused lint/format checks, the complete transaction-integrity review set, and pinned cardano-wallet context tests.
5. Complete the task tracker and PRD only after re-review confirms no open critical/high finding.
