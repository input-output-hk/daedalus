# dApp Browser And CIP-30 Recovery Runbook

## Scope And Baseline

This runbook controls the already-audited dApp browser release. It does not authorize source, dependency, backend, package, catalog, resource-policy, hardware-row, or activation changes.

The immutable task-807 release-candidate identities and evidence are recorded in [the release-candidate gate](../plans/dapp-browser-cip30/research/10-task-807-release-candidate.md). Its packaged policy has the global, preferred-catalog, Diagnostics, CIP-104, CIP-142, and hardware controls disabled, and catalog revision 1 has no entries. The later recorded Windows x64 production artifact supersedes only that platform's global and Diagnostics values; preferred catalog, CIP-104, CIP-142, and hardware remain disabled. Linux and macOS remain disabled.

Every policy, catalog, backend-pin, or hardware-row change requires a reviewed release artifact and application restart. There is no remote or in-process policy service. Compare every proposed change with the task-807 baseline and rerun the affected package, security, backend, hardware, and interoperability gates before rollout.

## Owners

| Role | Decision and evidence owner |
|---|---|
| Release Owner | Approves the exact packaged launcher configuration and rollout manifest; verifies artifact identity; sequences disable, restart, and restore; rejects unexplained baseline drift. |
| Catalog Steward | Proposes catalog additions, updates, and removals with exact entry identity, network URLs, canonical origin, resource-origin allowlist, supported wallet kinds/extensions, compatibility evidence, and catalog revision. |
| Security Owner / Incident Commander | Classifies incidents; approves catalog origin/resource changes, emergency removal, and launch disablement; decides whether grants may be retained or need a separately reviewed migration. |
| Backend Owner | Triages backend incompatibility, owns pending-submission reconciliation, and approves rollback only when database and API compatibility or a rollback migration is proven. |
| Device Compatibility Owner | Triages device, application, firmware, transport, cancellation, and proof regressions against the certified matrix. |

The Catalog Steward proposes a catalog change. The Security Owner approves its security and emergency disposition. The Release Owner approves and publishes the exact reviewed artifact. One person may fill multiple roles, but each decision and evidence record must name the acting role and person.

## Independent Launcher Controls

The launcher supplies one immutable `dappBrowserPolicy`. Invalid or absent policy fails closed through [`DappLaunchPolicy`](../../source/main/dapp/DappLaunchPolicy.ts).

| Control | Effect | Baseline and recovery rule |
|---|---|---|
| `globalEnabled` | Master gate. Both preferred and Diagnostics launch require it. | Disable to stop all new guest launch. It does not delete durable grants, collateral metadata, or cardano-wallet submission evidence. |
| `preferredCatalogEnabled` | Gates only preferred-catalog launch. | Independent of Diagnostics. A disabled preferred mode does not remove catalog-bound grants; removed or identity-changed entries are pruned separately. |
| `diagnosticsEnabled` | Gates only arbitrary Diagnostics URL staging and launch. | Independent of preferred catalog. Staged Diagnostics URLs are main-memory, one-use values and disappear on teardown/restart. |
| `cip104Revision` | Proposed-extension policy input. | CIP-104 is terminal-disabled by the frozen [contract manifest](../../source/common/cip30/contracts/contract-manifest.json). No revision value currently enables or advertises it. Reopening requires a new interoperability task and security-reviewed release. |
| `cip142Revision` | Gates CIP-142 negotiation at the descriptor's required revision. | Independent of launch-mode controls. It never creates a guest or bypasses route, sandbox, grant, or consent checks. Task-807 baseline and the recorded Windows activation use revision 0. |
| `hardwareConnectorRows` | Enables only exact compiled, physically certified hardware rows. | Empty in the baseline and recorded Windows activation. Never substitute software signing or a different device row during recovery. |
| Per-entry network/resource policy | Limits a bundled catalog entry's network URLs and permitted destinations. | A catalog release change, not a remote switch. Origin or resource-policy changes create a new entry identity and require review. |

[`DappLaunchPolicy.spec.ts`](../../source/main/dapp/DappLaunchPolicy.spec.ts) proves the global, preferred, and Diagnostics controls are independent and malformed policy disables launch. Proposed-extension capability checks remain subordinate to the authenticated broker and registry.

## Emergency Disable And Restore

### Disable

1. Incident Commander records the affected platform, cluster, package hash, launcher-policy identity, catalog revision, backend pin, symptom, and time. Do not record URLs containing credentials, addresses, transaction CBOR, signatures, keys, passphrases, or other wallet material.
2. Release Owner selects the smallest containment:
   - all launch: set `globalEnabled=false`;
   - preferred only: set `preferredCatalogEnabled=false`;
   - Diagnostics only: set `diagnosticsEnabled=false`;
   - CIP-142 only: set `cip142Revision=0`;
   - hardware row only: remove the exact row from `hardwareConnectorRows`.
   CIP-104 is already terminal-disabled.
3. Produce and review a normal packaged launcher update. Do not patch a running process or introduce a remote flag.
4. Install the update and restart Daedalus. Restart tears down the prior process and guest. Confirm new launch attempts are refused for the disabled mode.
5. Preserve the grant repository, collateral preference, wallet database, and cardano-wallet pending-submission records. Do not use connection repair or delete wallet state as part of launch disablement.
6. Record the installed artifact/configuration hashes and the applicable focused or packaged verification results.

### Restore

1. Root cause and remediate through normal source/release change control.
2. Re-run every gate affected relative to task-807, including security re-review for a material security-boundary, package, dependency, backend, protocol-policy, catalog-origin, or resource-policy change.
3. Release Owner verifies the exact reviewed restore artifact and rollout manifest.
4. Restore only the required control, restart, and perform a bounded launch check. Do not infer that one platform, mode, extension, or device row enables another.
5. Existing unchanged grants may be reused only after the normal route, origin, wallet, network, catalog-identity, and capability checks pass.

## Incident Procedures

### Sandbox Failure

1. Disable global launch for the affected release variant and restart.
2. Preserve the privacy-safe sandbox error and exact package/configuration identity.
3. Do not retry with `--no-sandbox`, `--disable-setuid-sandbox`, a portable installer, a custom unsupported install root, or a different unreviewed Chromium path.
4. Keep ordinary wallet operation available only where the packaged fail-closed path supports wallet-only mode.
5. Re-enable only after the exact installed artifact passes the platform's sandbox probe, runtime canary, and hostile-renderer matrix.

The pre-launch canary and package identity checks are owned by [`dappSandboxAvailability`](../../source/main/sandbox/dappSandboxAvailability.ts); guest construction remains behind that gate in [`DappBrowserManager`](../../source/main/dapp/DappBrowserManager.ts).

### Guest Teardown

1. Close the active guest or restart into the disabled package.
2. Confirm the existing lifecycle path revoked broker sessions and pending pre-authorization consent before destroying the window.
3. Confirm the egress proxy closed and the random nonpersistent session cleared connections, storage, cache, and authentication state.
4. Distinguish live capability revocation from durable grant deletion. A normal close or kill-switch restart retains grants.
5. Treat any teardown failure as a sandbox/security incident; keep global launch disabled until the packaged hostile matrix passes.

The teardown order is implemented by [`DappBrowserManager`](../../source/main/dapp/DappBrowserManager.ts) and wired by [`dappBrowser`](../../source/main/ipc/dappBrowser.ts).

### Backend Incompatibility Or Rollback

1. Disable new global launch before changing the backend.
2. Preserve the wallet database, migration backup, backend pin identity, and pending-submission records.
3. Backend Owner determines whether the installed database and API are backward-compatible. Revert a pin only when compatibility is proven or a reviewed rollback migration/backup restoration exists.
4. Keep cardano-wallet as the sole pending-submission authority. Do not use proxy submission, add a Daedalus journal, reconstruct a transaction, or manually mutate pending state.
5. After rollback/recovery, reconcile pending state before restoring launch and rerun the affected backend integration, migration, and package gates.

The authoritative state machine and rollback constraints are frozen in [the backend contract](../plans/dapp-browser-cip30/research/03-cardano-wallet-backend-contract.md) and the PRD rollback section.

### Device Regression

1. Cancel the operation and disconnect the affected device if safe.
2. Disable the exact certified hardware row in a reviewed package; disable the affected launch mode or global launch if row isolation cannot be proven.
3. Do not fall back to software/passphrase signing, another model or firmware row, vendor COSE, reconstructed transaction bodies, or unverified witnesses.
4. Record only privacy-safe model/application/firmware/transport/error identities. Do not retain request bytes, addresses, signatures, or key associations.
5. Restore the row only after the Device Compatibility Owner supplies exact matrix, physical-certification, cancellation, stale-result, body-hash/witness, and packaged-activation evidence.

### Ambiguous Submission State

1. Do not manually resubmit and do not create a Daedalus recovery journal.
2. Record the privacy-safe transaction identity and wallet/backend release identities. Never copy transaction CBOR or credentials into an incident record.
3. Let cardano-wallet reconcile its durable `broadcasting`, `outcome_unknown`, `submitted`, `in_ledger`, `rejected`, or `expired` state against coherent node/mempool evidence.
4. An exact dApp retry may be used only through the normal wallet-scoped endpoint. Existing nonterminal state returns its recorded hash/status; only backend reconciliation may make the exact sealed bytes retry-eligible.
5. A process exit does not resume unattempted CIP-103 items. Reconcile attempted items; a later exact caller retry remains ordered and idempotent.
6. Keep launch disabled if backend identity or reconciliation behavior is incompatible. Restore only after Backend Owner approval and affected fault/recovery tests pass.

### Catalog Update Or Emergency Removal

1. Catalog Steward records the exact old/new catalog revision, entry identity, canonical origin, network URLs, resource origins, wallet/extension compatibility, and evidence. No remote catalog update is allowed.
2. Security Owner reviews every origin/resource-policy change and approves the emergency disposition.
3. For immediate containment, Release Owner disables preferred launch or global launch and restarts through a reviewed package update.
4. For durable removal, publish a reviewed bundled catalog release that removes or changes the entry. On startup, the broker prunes grants whose catalog entry is absent or whose identity changed.
5. Unchanged entries retain their grants. Restoring a removed entry requires normal review; never copy its former grants into a new identity.
6. Run affected catalog contract, origin/resource-policy, grant-pruning, package, and security checks before rollout.

Catalog identity and startup pruning are implemented by [`dappCatalog`](../../source/common/config/dappCatalog.ts), [`GrantRepository`](../../source/main/cip30/GrantRepository.ts), and [`Cip30Broker`](../../source/main/cip30/Cip30Broker.ts).

### Emergency Grant Invalidation

Use the narrowest existing operation:

- one saved connection: **Forget connection**; it cancels matching consent, revokes matching live capability, then deletes that grant;
- one elevated scope: revoke that scope; it cancels matching consent and live authority while preserving the base grant;
- one catalog entry: remove or identity-change it in a reviewed catalog release and restart so startup pruning removes only matching grants;
- corrupt repository: use **Repair** only for fail-closed corruption recovery; it revokes all live sessions and replaces the corrupt repository with an empty one.

Do not claim that a launcher switch deletes grants. There is no remote global durable-grant erase. A grant schema-version bump or new bulk-erasure mechanism is a separate source/migration/security change outside task-900. Existing behavior is implemented by [`DappConnectionService`](../../source/main/cip30/DappConnectionService.ts).

## Verification And Evidence Checklist

For each disable, recovery, catalog, backend, or device decision, retain:

- named acting owners and approvals;
- incident and decision timestamps;
- exact source, dependency, backend, package, launcher-policy, and catalog identities;
- affected platform, cluster, mode, extension, or certified device row;
- privacy-safe failure classification;
- focused and packaged checks performed and their result;
- restart/teardown confirmation;
- pending-submission reconciliation disposition;
- baseline comparison and required re-review disposition;
- exact restored artifact/configuration identity.

Minimum existing focused coverage for runbook behavior:

```bash
yarn test:jest \
  source/main/dapp/DappLaunchPolicy.spec.ts \
  source/main/ipc/dappBrowser.spec.ts \
  source/main/dapp/DappBrowserManager.spec.ts \
  source/main/sandbox/dappSandboxAvailability.spec.ts \
  source/main/cip30/ExtensionRegistry.spec.ts \
  source/main/cip30/GrantRepository.spec.ts \
  source/main/cip30/DappConnectionService.spec.ts
```

For a release artifact, also run its existing installed package lifecycle, sandbox probe, and `yarn test:dapp-security` matrix. Exercises must use disposable environments and restore the exact reviewed baseline afterward. Required rollout drills cover global disable with an open guest, separate preferred/Diagnostics disablement, CIP-104 omission, CIP-142 baseline refusal, sandbox unavailability, backend rollback/reconciliation, device cancellation/stale completion, ambiguous submission recovery, and catalog-entry grant pruning.
