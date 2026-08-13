# Embedded dApp Browser And CIP-30 Wallet Connector PRD

## Overview

Add a wallet-scoped, curated dApp browser to Daedalus with an isolated Electron guest window and a standards-conformant Cardano wallet connector. The connector exposes the current CIP-30 key-wallet API, active key-wallet extensions CIP-95 and CIP-103, and policy-gated proposed extensions CIP-104 and CIP-142. It supports Shelley software wallets, Ledger wallets, and Trezor wallets; Byron wallets are excluded.

Remote dApp content is treated as hostile. It never runs in the existing privileged renderer, never receives the existing preload or IPC surface, and never connects through an external-browser transport. A main-process capability broker authenticates the guest, origin, route-selected wallet, network, negotiated extensions, and exact request bytes. Trusted Daedalus UI owns connection, key-disclosure, signing, data-signing, and submission consent.

The feature also introduces full-ledger transaction context, witness-only software signing, arbitrary-CBOR hardware signing, ordered CIP-103 batch signing and submission, exact CIP-8 message signing, and wallet-level soft collateral management.

## Task Tracking

- Companion tasks JSON: [dapp-browser-cip30-tasks.json](./dapp-browser-cip30-tasks.json)
- This PRD and its companion task graph are the canonical plan for this feature.
- Implementation progress must update both files together.

## Problem Statement

Daedalus has no runtime dApp connector, embedded remote-content boundary, or CIP-30 provider. The existing `DappTransactionRequest` component is Storybook-only and cannot safely decode or review arbitrary current-era transactions.

The current Electron main renderer cannot host hostile content safely:

- `source/main/windows/main.ts` enables Node integration and disables context isolation.
- `source/main/preload.ts` exposes raw IPC, Node HTTP(S), paths, configuration, and logging capabilities.
- Existing IPC wrappers discard sender information and generally do not authenticate the calling renderer or frame.
- Popup and external-URL handling is not sufficient for a hostile renderer.
- Linux development and packaging currently use process-wide sandbox-disabling flags, and the historical portable self-extracting `.bin` installer cannot establish Chromium OS sandbox privileges required for hostile guests.

The existing wallet and transaction APIs are also incomplete for CIP-30:

- `/statistics/utxos` is histogram data, not serialized UTxOs.
- `/wallets/{walletId}/utxo` returns values without outpoints, address, datum, or reference scripts.
- cardano-wallet's reduced internal `TxOut` loses datum and reference-script information.
- The software sign endpoint returns a complete signed transaction rather than only newly generated witnesses.
- The current software signer cannot resolve an unsubmitted output produced by an earlier transaction in the same CIP-103 request.
- The existing metadata-signing endpoint is Catalyst-specific and is not CIP-8/CIP-30 `signData`.
- Current Ledger and Trezor adapters reconstruct a limited transaction from Daedalus coin-selection data instead of signing the exact dApp-supplied body.
- Hardware proxy submission does not persist the transaction in cardano-wallet's pending pool.
- Daedalus has no preferred collateral concept, and ordinary sends may consume any pure-ADA collateral candidate.

This work matters because a connector that is merely functional but not byte-exact, origin-bound, sender-authenticated, or ledger-complete could disclose wallet data, sign a transaction other than the one reviewed, lose collateral, or expose existing privileged IPC to a remote page.

## Goals

- Provide a curated, wallet-scoped dApp browser at `/wallets/:id/dapps`.
- Provide arbitrary HTTPS dApp launch through Daedalus Diagnostics with the same connector API on every configured Cardano network.
- Keep remote content in a separately managed, sandboxed, nonpersistent Electron window.
- Implement current CIP-30 for key-controlled Shelley wallets.
- Implement active CIP-30 extensions CIP-95 and CIP-103.
- Implement proposed CIP-104 and CIP-142 behind explicit policy and interoperability gates.
- Implement exact CIP-8 message signing for base CIP-30 and CIP-95 DRep signing.
- Support software, Ledger, and Trezor wallets through capability-checked paths.
- Return exact CIP-30 UTxO, value, address, witness-set, and COSE encodings.
- Preserve exact transaction-body bytes throughout parsing, review, software signing, hardware signing, and witness verification.
- Support ordered CIP-103 dependencies with conflict flagging, all-or-nothing witness disclosure, and attempt-all submission.
- Add a user-visible soft collateral preference with preparation and review warnings, without treating the preferred UTxO as locked or changing normal coin selection.
- Require internal security review, external audit, packaged sandbox verification, and physical hardware certification before general rollout.

## Non-Goals

- No Byron-wallet connector support.
- No browser extension, native-messaging connector, WebRTC connector, deep-link connector, or other external-browser transport.
- No remote dApp catalog in the initial release.
- No economic, smart-contract, token, or security endorsement of catalog entries.
- No native-script multisig wallet provider and no CIP-106 support.
- No Plutus/script-controlled wallet provider, secret export, or CIP-141 support.
- No implementation of unmerged CIP-144/CIP-147 successor connector proposals.
- No CIP-95 constitutional-committee or stake-pool key signing; CIP-95 key-wallet scope is payment, stake, and DRep keys.
- No CIP-8 payload hashing fallback under base `signData` or CIP-95 `signData`.
- No automatic signing or submission of a collateral-preparation transaction.
- No guarantee of equal transaction-feature support across software, Ledger, and Trezor wallets.
- No migration of the existing trusted main renderer to a fully sandboxed/context-isolated architecture as part of the minimum connector release, although its navigation and IPC boundaries must be hardened.
- No persistence or later-call resolution of signed-but-unsubmitted transaction outputs. CIP-103 dependencies are resolved within the current request or from cardano-wallet's normal pending-submission state.
- No Linux portable self-extracting `.bin`, AppImage, Flatpak, Snap, or other non-`.deb`/`.rpm` Linux product package for dApp-capable or general Linux shipping once the system-package migration lands.

## Inputs And Source Material

### Repository Documentation

- `.agent/readme.md`
- `.agent/system/architecture.md`
- `.agent/workflows/frontend.md`
- `.agent/workflows/ipc.md`
- `.agent/workflows/test.md`
- `.agent/plans/readme.md`
- `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan.md`

### Current Daedalus Code

- `source/main/index.ts`
- `source/main/windows/main.ts`
- `source/main/preload.ts`
- `source/main/webpack.config.js`
- `source/common/ipc/api.ts`
- `source/common/ipc/lib/IpcChannel.ts`
- `source/main/ipc/lib/MainIpcChannel.ts`
- `source/main/ipc/lib/MainIpcConversation.ts`
- `source/main/ipc/open-external-url.ts`
- `source/main/ipc/open-local-directory.ts`
- `source/main/ipc/getHardwareWalletChannel.ts`
- `source/common/types/hardware-wallets.types.ts`
- `source/renderer/app/api/api.ts`
- `source/renderer/app/api/utils/request.ts`
- `source/renderer/app/stores/WalletsStore.ts`
- `source/renderer/app/stores/TransactionsStore.ts`
- `source/renderer/app/stores/HardwareWalletsStore.ts`
- `source/renderer/app/stores/NetworkStatusStore.ts`
- `source/renderer/app/stores/UiDialogsStore.ts`
- `source/renderer/app/stores/AddressesStore.ts`
- `source/renderer/app/utils/dataSerialization.ts`
- `source/renderer/app/utils/shelleyLedger.ts`
- `source/renderer/app/utils/shelleyTrezor.ts`
- `source/renderer/app/routes-config.ts`
- `source/renderer/app/Routes.tsx`
- `source/renderer/app/App.tsx`
- `source/renderer/app/components/wallet/navigation/WalletNavigation.tsx`
- `source/renderer/app/components/dapp/DappTransactionRequest.tsx`
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
- `source/common/config/electron-store.config.ts`
- `source/common/types/electron-store.types.ts`
- `nix/internal/x86_64-linux.nix`
- `nix/internal/linux-self-extracting-archive.sh` (legacy portable path; rejected for shipping)
- Linux `.deb` / `.rpm` packaging outputs and postinst scripts (to be added)
- `flake.nix`
- `flake.lock`
- `package.json`

### Pinned Backend And Dependencies

- cardano-wallet `v2026-07-23`, revision `724be55dc66cf67bc4427e8f1a9657a9d1d33d71`
- cardano-wallet implementation work lands in the sibling `../cardano-wallet` checkout (`/home/westbam/Development/cardano-wallet` in the current workspace); Daedalus consumes only a reviewed commit through its Nix pin.
- Electron `41.3.0`
- `@cardano-foundation/ledgerjs-hw-app-cardano@7.1.4`
- `@trezor/connect@9.7.2`
- `@cardano-sdk/core@0.41.4`
- `cbor@5.0.2`
- `borc@2.1.2`

### Standards

- CIP-8: https://cips.cardano.org/cip/CIP-0008
- CIP-19: https://cips.cardano.org/cip/CIP-0019
- CIP-30: https://cips.cardano.org/cip/CIP-0030
- CIP-40: https://cips.cardano.org/cip/CIP-0040
- CIP-95: https://cips.cardano.org/cip/CIP-0095
- CIP-103: https://cips.cardano.org/cip/CIP-0103
- CIP-104: https://cips.cardano.org/cip/CIP-0104
- CIP-105: https://cips.cardano.org/cip/CIP-0105
- CIP-106: https://cips.cardano.org/cip/CIP-0106
- CIP-141: https://cips.cardano.org/cip/CIP-0141
- CIP-142: https://cips.cardano.org/cip/CIP-0142
- CIP-1694: https://cips.cardano.org/cip/CIP-1694
- CIP-30 extensions register: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0030/extensions-register.md

### Standards Fixtures

CIP documents are living standards; the plan does not pin document revisions. Instead, task 002 freezes golden fixtures and contract excerpts for every implemented wire contract, and conformance tasks validate against current ecosystem behavior (Lace, Yoroi, Cardano JS SDK, and Eternl/Typhon/Lucid-compatible clients where available). Any upstream behavioral delta discovered during conformance work is documented and resolved, or the affected capability remains disabled.

Task-002 freezes the phase-0 inputs consumed by later production validation:

| Artifact | Role |
|---|---|
| `source/common/cip30/contracts/contract-manifest.json` | Provider properties, JavaScript invocation rules, all public method paths, negotiation, errors, limits, consent expiry, and the internal result envelope |
| `source/common/cip30/contracts/schemas/` | JSON Schema draft-07 definitions for JSON-representable public values, errors, and clone-safe results |
| `source/common/cip30/contracts/fixtures/` | Provenance-backed CBOR, COSE, address, error, negotiation, limit, and gated-extension vectors |
| `source/common/cip30/contracts/contractFixtures.spec.ts` | Schema, coverage, exact-byte, signature, Bech32, limit, and structured-clone evidence |
| `research/02-cip30-wire-contract-evidence.md` | Source revisions, conflict resolutions, ecosystem comparison, reproduction commands, and residual gates |

These are contract inputs, not production validators or dispatch code. Task-300 must implement strict shared runtime validators and prove equivalence without silently changing the frozen behavior. CIP-104 remains omitted until task-404 proves one exact positive encoding.

### Collateral Comparisons

- Lace collateral flow: https://github.com/input-output-hk/lace/tree/main/packages/module/cardano-collateral-flow
- Lace collateral selection: https://github.com/input-output-hk/lace/blob/main/packages/contract/cardano-context/src/util.ts
- Nami collateral builder: https://github.com/input-output-hk/nami/blob/main/src/ui/app/components/transactionBuilder.jsx
- Nami wallet API: https://github.com/input-output-hk/nami/blob/main/src/api/extension/index.js
- Eternl collateral settings: https://github.com/Tastenkunst/eternl-wikibook/blob/main/content/using-eternl/settings/account-settings.md
- Eternl release notes: https://github.com/Tastenkunst/eternl-wikibook/blob/main/content/eternl-updates-release-notes.md

## Current Baseline

### Electron

- The trusted main renderer is Node-enabled and not context-isolated.
- The existing preload is privileged and cannot be reused by remote content.
- `webviewTag` is already disabled.
- There is no guest `BrowserWindow`, `WebContentsView`, `<webview>`, dApp preload, isolated session, permission policy, or dApp broker.
- Current global IPC does not provide the sender/frame authentication required for a hostile renderer.
- The global popup handler now denies requests without shell side effects; trusted UI external-link requests accept only parsed credential-free HTTPS URLs with awaited privacy-safe failures.
- Linux development and packaging pass `--disable-setuid-sandbox --no-sandbox`.
- Linux currently ships a home-directory self-extracting `.bin`; that model is product-rejected for ongoing shipping in favor of system `.deb` and `.rpm` packages (research `06`).

### Renderer

- `WalletsStore.active` is route-derived and may silently fall back to the first wallet for invalid wallet routes.
- No dApp route exists.
- Existing dialog stores do not provide a correlated, app-global request queue.
- `DappTransactionRequest` is static and cannot review arbitrary scripts, governance, collateral, minting, certificates, withdrawals, or batch dependencies.
- Network readiness is split between `NetworkStatusStore.isConnected` and `NetworkStatusStore.isSynced`.

### cardano-wallet

- Decode, full-transaction sign, wallet-scoped submit, and proxy submit endpoints exist.
- The public UTxO APIs cannot produce `transaction_unspent_output` CBOR.
- `getUTxOByTxIn` can retrieve full ledger outputs for recent eras and is the required full-output foundation.
- Existing wallet `TxOut` persistence is insufficient because it stores only address and token bundle.
- Existing software transaction signing does not provide a CIP-30 witness-only response or a validated earlier-transaction overlay within one batch request.
- Existing metadata signing is not general CIP-8.
- Current derivation APIs expose roles 0, 1, and 2 but not a dedicated CIP-105 DRep role.
- Stake-key registration state is not exposed with sufficient precision for CIP-95.
- Existing V2 signing has gaps for collateral and explicit required signers.

### Hardware

- Existing Daedalus hardware flows reconstruct a reduced transaction and ignore the device-returned body hash.
- The current common Ledger request omits many modern fields that the installed Ledger package can represent.
- Ledger 7.1.4 supports many Conway fields but not proposal procedures and limits current voting-procedure structure.
- Trezor Connect 9.7.2 supports fewer Conway/governance fields and cannot represent DRep registration/update/deregistration, governance voting procedures, proposals, treasury, or donation.
- Both installed libraries expose message signing, but Daedalus has no CIP-8 bridge or IPC.
- Ledger JS 7.1.4 does not accept Cardano app major version 8; Ledger JS 8.x requires a validation spike and likely upgrade.

## Locked Planning Decisions

- The dApp catalog route is `/wallets/:id/dapps`.
- The route-selected active wallet is the connector wallet.
- Invalid dApp wallet routes do not fall back to another wallet.
- Remote content opens in a separately managed sandboxed `BrowserWindow`.
- The guest uses a fresh random nonpersistent session and a dedicated preload.
- A dApp or origin switch destroys and recreates the guest.
- Trusted catalog, connection UI, approvals, settings, and transaction review remain in the main Daedalus renderer.
- The guest is hidden and disabled during trusted approval.
- Preferred dApps are compatibility-tested, not endorsed.
- Arbitrary production URLs are available through diagnostics over HTTPS.
- Development may permit HTTP loopback only through an explicit development policy.
- Guest networking is limited to policy-controlled HTTPS/WSS. WebRTC, STUN/TURN, WebTransport, QUIC, and other non-proxied transports are disabled unless a later compatibility review identifies a concrete required dApp and proves equivalent destination enforcement before enabling that transport.
- No external-browser connector or inbound connector is implemented.
- Linux product packages are system **`.deb`** and **`.rpm`** only, installing under a fixed `/opt/...` path with privileged postinst capable of SUID `chrome-sandbox` and/or AppArmor `userns` profiles. Portable self-extracting `.bin`, AppImage, Flatpak, Snap, and other Linux channels are rejected (research [06-linux-system-package-decision.md](./research/06-linux-system-package-decision.md)).
- Shelley software, Ledger, and Trezor wallets are in scope; Byron is excluded.
- Persistent grants cover connection/read scopes only.
- Every `signTx`, `signTxs`, `signData`, CIP-95 `signData`, `submitTx`, and `submitTxs` call requires trusted consent.
- Every submission call receives its own confirmation; signing does not grant an automatic submission waiver.
- CIP-103 partial submission failure rejects with the normative index-aligned mixed hash/error array.
- Once a submission call is confirmed, Daedalus attempts every authorized transaction even if the guest closes or its route lease is later revoked; stale guests receive no result. If the app exits mid-call, un-attempted items are not submitted; cardano-wallet pending-submission state is the reconciliation record and dApps may retry idempotently.
- No nonstandard resolved-results helper is exposed.
- Daedalus enforces documented product limits: 64 KiB maximum request CBOR for `signTx`/`submitTx` bodies and `signData` payloads, 50 items per CIP-103 batch, and a 100-entry maximum page size, each rejecting with `APIError.InvalidRequest` before side effects; pending consent auto-rejects after five minutes of user inactivity with the canonical declined/refused error.
- Platform, ledger, backend, and hardware intrinsic capability limits still produce typed failures.
- Key-wallet extensions in scope are CIP-95, CIP-103, CIP-104, and CIP-142.
- CIP-106 and CIP-141 are explicitly excluded and never advertised.
- CIP-104 and CIP-142 are labeled Proposed and remain policy-gated.
- CIP-8 is a base/CIP-95 message-signing format, not a negotiated extension.
- The dApp catalog is bundled and release-versioned initially.
- Collateral is a persisted soft preference, not a permanent lock.
- Normal coin selection is unchanged: ordinary sends may consume the preferred collateral UTxO like any other; transaction review warns when this happens, and Daedalus reconciles the preference afterwards.
- Preferred collateral remains included in CIP-30 `getUtxos` and `getBalance`.
- Default preferred collateral target is 5 ADA as an ecosystem convention, not a protocol maximum.
- Daedalus main owns the persisted collateral-preference record; cardano-wallet remains authoritative for UTxO and pending-submission state, from which collateral readiness is derived at read time.
- Chain advancement, rollback, or an input becoming spent after review does not trigger another review. Daedalus signs the exact approved bytes and ordinary node submission may fail.
- Signing state and staged witnesses are memory-only. Submission recovery relies on cardano-wallet pending-submission state; Daedalus keeps no durable submission journal.
- Feature and extension switches are packaged launcher-configuration values changed through the normal reviewed release process; this plan adds no remote runtime-policy service.
- Collateral-preparation transactions are explicit and user-approved.
- Internal security review and external audit are release gates.

## Scope

### Wallet Scope

| Wallet kind | Base CIP-30 | CIP-95 | CIP-103 | CIP-104 | CIP-142 |
|---|---|---|---|---|---|
| Shelley software | Yes | Yes | Yes | Gated | Gated |
| Ledger Shelley | Yes, capability checked | Yes, capability checked | Yes, sequential device confirmations | Gated | Gated |
| Trezor Shelley | Yes, capability checked | Yes, with transaction limitations | Yes, sequential device confirmations | Gated | Gated |
| Byron | No | No | No | No | No |
| Native-script multisig provider | No | No | No | No | No |
| Plutus/script wallet provider | No | No | No | No | No |

### Extension Scope

| CIP | Status | Register | Decision |
|---|---|---|---|
| CIP-8 | Active | Not an extension | Required by base and CIP-95 `signData` |
| CIP-95 | Active | Registered | Implement and advertise when fully available |
| CIP-103 | Active | Registered | Implement and advertise when fully available |
| CIP-104 | Proposed | Registered | Implement behind proposed-CIP and interoperability gates |
| CIP-106 | Proposed | Registered | Excluded; requires a native-script multisig wallet provider |
| CIP-141 | Proposed | Not registered | Excluded; requires a Plutus/script-wallet provider and has unresolved wire/security defects |
| CIP-142 | Proposed | Not registered | Implement behind proposed-CIP policy gate |

### Network Scope

- All Daedalus-configured networks are supported.
- `getNetworkId()` returns the CIP-30 network ID.
- Negotiated CIP-142 returns the configured network magic.
- Grants also bind the genesis identity so custom networks sharing a magic cannot reuse authority.
- Network changes invalidate the live session and return `APIError.AccountChange` to outstanding account-bound calls.

## User Journeys

### Curated dApp

1. User opens the active wallet's dApp tab.
2. Daedalus displays the bundled catalog and compatibility disclaimer.
3. User selects a catalog entry.
4. Main process resolves the entry by opaque catalog ID and creates the isolated guest.
5. DApp calls `enable` and requests extensions.
6. Daedalus presents wallet, origin, network, read scope, governance-key scope, and account-xpub scope as applicable.
7. Approval persists only the approved read/key-disclosure grant and creates an in-memory live capability.
8. Guest receives an API containing only the negotiated extension namespaces.

### Diagnostics dApp

1. User opens Daedalus Diagnostics.
2. User confirms or selects an eligible Shelley wallet; the current eligible active wallet is the default.
3. User enters an HTTPS URL.
4. Main validates and normalizes it, stores it only as an in-memory one-use pending launch, and never places the full URL in a route or grant.
5. Trusted navigation moves to `/wallets/{walletId}/dapps`.
6. Main waits for that exact route/wallet lease to commit, consumes the pending launch, and creates the guest.
7. The exact origin becomes the capability identity.
8. The launch is labeled untrusted and receives no preferred-dApp branding.
9. The same enable, grant, signing, submission, route-lease, and network rules apply; Diagnostics never bypasses wallet-scoped authority.

### Transaction Signing

1. Guest sends exact transaction CBOR.
2. Broker authenticates sender/frame/origin/generation/route lease and stores the immutable request.
3. Shared parser extracts exact body bytes and complete semantic effects.
4. Backend resolves every input and wallet ownership at a named chain point.
5. Trusted UI displays effects, dependencies, scripts, governance, collateral, and maximum risk.
6. User approves or rejects.
7. Original broker-owned bytes are sent to the selected software or hardware signer.
8. Returned body hash and signatures are independently verified.
9. Guest receives only newly generated witness-set CBOR.

### Batch Signing

1. Guest submits one ordered CIP-103 signing request.
2. Daedalus validates every transaction before side effects.
3. Backend supplies one coherent context for node state, cardano-wallet pending submissions, and earlier outputs in this request.
4. Daedalus builds dependency and conflict graphs without reordering.
5. Trusted UI presents one ordered review with per-item conflict flags.
6. Software or hardware signing proceeds in order.
7. Witnesses remain staged in memory until every item succeeds.
8. Any failure discards all staged results and rejects once with the first failing index in `info`.

### Collateral Preparation

1. Daedalus automatically adopts a suitable existing pure-ADA key-controlled UTxO as the preference when possible.
2. If no candidate exists, the dApp page shows `Prepare collateral`.
3. User reviews a normal self-transfer that creates a 5 ADA output plus fee.
4. Software user enters the spending password or hardware user confirms on device.
5. Preference is recorded only after backend observation of the created output.
6. Normal sends may spend it; the review states when preferred collateral will be spent.
7. Daedalus then adopts another candidate or returns to `not-ready`.

## Requirements

### Functional Requirements

- [ ] Add `/wallets/:id/dapps` and a wallet-navigation item hidden for Byron wallets.
- [ ] Add a bundled, release-versioned curated dApp catalog.
- [ ] Add arbitrary HTTPS launch through a one-use Diagnostics-to-wallet-dApp-route handoff.
- [ ] Create an isolated nonpersistent guest window and dedicated preload.
- [ ] Inject `window.cardano.daedalus` before remote page scripts execute.
- [ ] Implement provider metadata, `isEnabled`, `enable`, and extension negotiation.
- [ ] Persist exact-origin read grants in a main-owned repository.
- [ ] Add connection/key-disclosure revocation and wallet deletion cleanup.
- [ ] Implement base CIP-30 read methods and exact serialization.
- [ ] Implement deprecated `getCollateral` for compatibility while labeling it deprecated internally and in developer documentation.
- [ ] Implement exact CIP-30/CIP-8 `signData`.
- [ ] Implement witness-only `signTx` for software and hardware wallets.
- [ ] Implement separately reviewed `submitTx` through wallet-scoped submission.
- [ ] Implement CIP-95 key getters, DRep `signData`, Conway-aware `signTx`, and error extensions.
- [ ] Implement CIP-103 ordered batch signing and submission.
- [ ] Implement policy-gated CIP-104 account-xpub disclosure after wire interoperability is proven.
- [ ] Implement policy-gated CIP-142 network magic.
- [ ] Add complete transaction review for all supported current-era fields.
- [ ] Reconcile dApp submissions after restart through cardano-wallet pending-submission state.
- [ ] Add ordered batch review with per-item conflict flags.
- [ ] Add soft collateral preference, preparation, readiness, and replacement flows.
- [ ] Add Ledger and Trezor arbitrary-CBOR transaction and message signing.
- [ ] Add connection, approval, batch, collateral, offline, error, and settings UX in en-US and ja-JP.
- [ ] Add settings to inspect and forget dApp connections and elevated key grants.
- [ ] Add a feature kill switch and fail-closed sandbox availability gate.

### Non-Functional Requirements

- Guest compromise must not grant access to existing privileged IPC, Node APIs, TLS material, filesystem APIs, hardware channels, or electron-store.
- Every guest request and result must be sender/frame/origin/generation/route-lease authenticated.
- Every signing review must bind to exact transaction-body bytes and complete semantic effects.
- Unknown transaction fields or unsupported encodings must fail closed.
- Existing witness data must never be reported as newly generated wallet witnesses.
- Signatures must be independently verified before release.
- Network, wallet, origin, extension, or document changes must revoke live authority.
- No raw transaction, message, address, signature, xpub, key association, or passphrase may enter logs or analytics.
- The guest must leave no cookies, local storage, service workers, cache, or authentication state after teardown.
- Production guest DevTools must remain disabled.
- Linux packaged builds must prove active Chromium OS sandboxing.
- Every visible state must be theme-compatible and localized.
- Connector APIs must return canonical typed CIP errors rather than generic JavaScript errors.
- Wallet mutations may be queued for correctness; concurrent connector calls queue rather than reject.
- Documented product limits (request CBOR size, CIP-103 batch size, page size, consent inactivity timeout) fail fast with typed errors; within-limit availability robustness is ordinary QA, not a security gate.

## Public API Contract

### Provider

```ts
type Extension = { cip: number };

interface DaedalusProvider {
  apiVersion: '1';
  name: string;
  icon: string;
  supportedExtensions: Extension[];
  isEnabled(): Promise<boolean>;
  enable(options?: { extensions?: Extension[] }): Promise<DaedalusApi>;
}

window.cardano.daedalus: DaedalusProvider;
```

- Name and icon are local trusted assets.
- Unsupported requested extensions are omitted rather than making `enable` fail.
- `api.getExtensions()` is authoritative for the enabled set.
- Extension namespaces are absent unless negotiated.

### Base CIP-30

```ts
interface DaedalusApi {
  getExtensions(): Promise<Extension[]>;
  getNetworkId(): Promise<number>;
  getUtxos(
    amount?: cbor<value>,
    paginate?: { page: number; limit: number }
  ): Promise<cbor<transaction_unspent_output>[] | null>;
  getCollateral(params: {
    amount: cbor<Coin>;
  }): Promise<cbor<transaction_unspent_output>[] | null>;
  getBalance(): Promise<cbor<value>>;
  getUsedAddresses(paginate?: {
    page: number;
    limit: number;
  }): Promise<Address[]>;
  getUnusedAddresses(): Promise<Address[]>;
  getChangeAddress(): Promise<Address>;
  getRewardAddresses(): Promise<Address[]>;
  signTx(
    tx: cbor<transaction>,
    partialSign?: boolean
  ): Promise<cbor<transaction_witness_set>>;
  signData(addr: Address, payload: Bytes): Promise<DataSignature>;
  submitTx(tx: cbor<transaction>): Promise<hash32>;
}
```

Base semantics:

- `Address` inputs accept Bech32 or hex-encoded raw bytes; all `Address` results are hex-encoded raw address bytes.
- `transaction_unspent_output` is exact CBOR `[transaction_input, transaction_output]`.
- `getUtxos()` returns all controlled UTxOs, including the soft preferred collateral UTxO.
- `getBalance()` equals the sum of those UTxOs and excludes rewards.
- `getUtxos` `amount` is a minimum selection target; return `null` when no set covers it.
- Pagination is zero-indexed and ordered deterministically by outpoint.
- Page size is capped at 100 entries; larger `limit` values reject with `APIError.InvalidRequest`.
- Out-of-range pagination rejects with `PaginateError {maxSize}`, where `maxSize` describes the current result set and is not a Daedalus-defined policy limit.
- Request CBOR for `signTx`/`submitTx` bodies and `signData` payloads is limited to 64 KiB; larger requests reject with `APIError.InvalidRequest` before any side effect.
- Wallet mutation between calls may produce duplicates or omissions as permitted by CIP-30.
- `getCollateral` returns one or more pure-ADA wallet-controlled candidates totaling at least the request, or `null` when the current wallet UTxO cannot cover it without creating a transaction.
- `getCollateral` accepts any valid `cbor<Coin>` without a Daedalus-defined 5 ADA maximum. The 5 ADA value is only the default preparation target, and `getCollateral` never triggers preparation or another side effect.
- This `getCollateral` behavior intentionally follows the deprecated API's nullable return type and current ecosystem behavior where the CIP-30 prose is contradictory.
- `partialSign=false` requires complete key/native-script proof under the resolved transaction context.
- `partialSign=true` returns every newly producible wallet VKey witness but still requires complete input resolution; when the wallet controls no applicable key, it resolves with a canonical empty witness set rather than `ProofGeneration`.
- `submitTx` returns the transaction ID only after Daedalus accepts and attempts submission.

### Error Matrix

| Error | Code | Use |
|---|---:|---|
| `APIError.InvalidRequest` | -1 | Malformed API shape, hex, CBOR, pagination, network mismatch, unsupported request structure, product size/count limit violation |
| `APIError.InternalError` | -2 | Backend, node, parser, or unexpected infrastructure failure |
| `APIError.Refused` | -3 | No grant, revoked capability, disconnection, policy refusal |
| `APIError.AccountChange` | -4 | Wallet or network route lease changed |
| `PaginateError` | N/A | Out-of-range `getUtxos`/`getUsedAddresses`; carries current `maxSize` |
| `TxSignError.ProofGeneration` | 1 | Missing proof required for `partialSign=false`, wrong password, unsupported device field, unresolved required signer |
| `TxSignError.UserDeclined` | 2 | Host or device rejection |
| `TxSignError.DeprecatedCertificate` | 3 | CIP-95 Genesis/MIR certificate |
| `DataSignError.ProofGeneration` | 1 | Unowned key, unavailable key, unsupported device message |
| `DataSignError.AddressNotPK` | 2 | Selected credential is script-controlled |
| `DataSignError.UserDeclined` | 3 | Host or device rejection |
| `TxSendError.Refused` | 1 | User or policy refused submission |
| `TxSendError.Failure` | 2 | Node/backend rejected or could not submit transaction |

The main/preload protocol resolves an internal success/error envelope. The guest preload reconstructs the exact public rejection locally so Electron cannot wrap a CIP error or CIP-103 mixed array in a generic `Error`.

## CIP-8 Message Signing

CIP-8 is not advertised or negotiated as an extension. It is the encoding used by base `signData` and CIP-95 DRep `signData`.

```cddl
COSE_Sign1 = [
  protected: bstr,
  unprotected: map,
  payload: bstr,
  signature: bstr
]
```

Produced protected headers:

```text
{
  1: -8,
  "address": raw address or DRep ID bytes
}
```

Produced unprotected headers:

```text
{
  "hashed": false,
  "version": 1
}
```

Signature structure:

```text
[
  "Signature1",
  exact protected header bytes,
  empty external AAD,
  exact payload bytes
]
```

COSE key:

```text
{
   1: 1,
   3: -8,
  -1: 6,
  -2: raw 32-byte public key
}
```

Rules:

- Return untagged CBOR as hex.
- Embed exact original payload bytes.
- Reject odd-length, prefixed, or invalid hex.
- Never normalize text or switch to hashed signing.
- Omit `kid` consistently. If later added, Sign1 and COSE_Key values must match.
- Produce the CIP-8-required body headers `hashed:false` and `version:1`.
- Interoperability verification may recognize legacy CIP-30 wallet output that omitted `version`, but Daedalus never produces that nonconforming form.
- Base, pointer, and ordinary enterprise addresses select the payment key.
- Reward addresses select the stake key.
- A raw 28-byte CIP-95 DRep ID selects the role-3 DRep key.
- For ecosystem compatibility, a type-6 enterprise key address also selects the role-3 DRep key when its credential equals the selected wallet's DRep key hash; the produced protected `"address"` header is the raw 28-byte DRep key hash.
- Verify the returned public key hash, address field, payload, and signature before release.
- Display origin, credential type, raw hex, and a UTF-8 preview only when decoding is exact and safe.

## CIP-95 Contract

```ts
interface Cip95Api {
  getPubDRepKey(): Promise<string>;
  getRegisteredPubStakeKeys(): Promise<string[]>;
  getUnregisteredPubStakeKeys(): Promise<string[]>;
  signData(
    addr: Address | DRepID,
    payload: Bytes
  ): Promise<DataSignature>;
}
```

When CIP-95 is negotiated:

- `api.cip95` exposes the four methods above.
- Base `api.signTx` gains CIP-95 Conway/DRep semantics.
- The printed CIP-95 omission of `.cip95` from `getRegisteredPubStakeKeys` is treated as a specification typo.
- Daedalus follows Lace, Yoroi, and Cardano JS SDK and exposes the namespaced getter only.
- Public DRep and stake keys are raw 32-byte Ed25519 public keys encoded as hex.
- DRep ID input accepts the normative raw 28-byte Blake2b-224 public-key hash encoded as hex.
- A Bech32 or raw-byte-hex type-6 enterprise key address is also accepted as a DRep identifier when its credential equals the selected wallet's DRep key hash. Otherwise it follows ordinary payment-key `Address` semantics.
- DRep path is `m/1852'/1815'/account'/3/0`.
- Registered keys include pending registration certificates.
- Unregistered keys include pending deregistration and unknown registration state.
- Payment, stake, and DRep witnesses are supported; committee and pool key witnesses are not.

Consent scopes:

- `governance-key-disclosure`
- `governance-data-signing`
- `governance-transaction-signing`

Extension enablement requires an elevated disclosure warning because public stake/DRep keys can correlate governance activity. Every CIP-95 signing call remains per-call approved.

## CIP-103 Contract

```ts
type TransactionSignatureRequest = {
  cbor: cbor<transaction>;
  partialSign?: boolean;
};

api.cip103.signTxs(
  txs: TransactionSignatureRequest[]
): Promise<cbor<transaction_witness_set>[]>;

api.cip103.submitTxs(
  txs: cbor<transaction>[]
): Promise<hash32[]>;
```

- Preserve input order; never topologically reorder.
- Witness result index matches request index.
- Signing failure rejects the whole call with one `TxSignError`; `info` identifies the first failing zero-based index.
- No witness array is released on any failure.
- Submission attempts every transaction in order despite prior submission failures.
- All-success resolves an aligned `hash32[]`.
- Any failure rejects directly with an aligned `(hash32 | TxSendError)[]`.
- Do not wrap the mixed array, resolve it, or expose a separate settled-results API.
- A batch is limited to 50 items; larger requests reject with `APIError.InvalidRequest` before any side effect.

## CIP-104 Contract

```ts
api.cip104.getAccountPub(): Promise<cbor<Bip32PublicKey>>;
```

- Return the selected CIP-1852 account xpub at `m/1852'/1815'/account'`.
- Public key payload is exactly 64 bytes: 32-byte public key plus 32-byte chain code.
- Convert stored `acct_xvk` or hardware account xpub to raw bytes before encoding.
- Never expose private or hardened-parent material.
- Require elevated `account-public-key-disclosure` consent.
- Explain that disclosure permits indefinite derivation and correlation of past and future non-hardened addresses.
- Do not log, persist a returned copy, or telemeter the xpub.
- The grant may persist only when explicitly approved and remains separately revocable.

CIP-104 does not define `Bip32PublicKey` CDDL precisely enough to settle raw bytes versus a CBOR byte string. Daedalus must not advertise CIP-104 until an interoperability task confirms one exact encoding against at least one listed implementor and freezes a golden vector.

Failure to prove one interoperable encoding is a valid terminal gate outcome: CIP-104 remains disabled and omitted, while the rest of the connector may proceed to release.

## CIP-142 Contract

```ts
api.cip142.getNetworkMagic(): Promise<number>;
```

- Negotiate with `{cip: 142}`.
- Namespace is `api.cip142`, despite inconsistent `cip-142` prose.
- Return a plain JavaScript number.
- Mainnet returns `764824073`, preprod returns `1`, preview returns `2`, and custom networks return their configured magic.
- Keep base `getNetworkId()` unchanged.
- Label CIP-142 Proposed in plan, UI/developer documentation, and capability policy.

## Hostile Renderer Threat Model And Architecture ADR

### ADR-001: Separate The Hostile Guest From The Privileged Wallet Renderer

- Status: Accepted
- Scope: Embedded dApp browser and CIP-30 connector.

**Context.** A catalogued dApp is remote, hostile content. The current main
renderer is Node-enabled, not context-isolated, and uses a privileged preload;
it is therefore explicitly legacy privileged UI, not a possible dApp host.

**Decision.** Each dApp runs only in a separately managed sandboxed
`BrowserWindow`, with a fresh random nonpersistent session and a dedicated
least-authority preload. The Electron main process is the authoritative
capability broker: it owns guest lifecycle, canonical origin and top-frame
identity, document generation, route lease, wallet/network selection,
connection and request IDs, immutable request bytes, grants, approval
correlation, and result validation. Trusted Daedalus UI presents consent and
review only; it cannot replace broker-owned request bytes. Existing privileged
IPC remains unavailable to the guest and must authenticate the exact trusted
main `WebContents` and main frame before any production guest is enabled.

**Rejected alternatives.** Do not host remote content in the trusted renderer,
an iframe, or a `<webview>`; reuse the privileged preload or generic IPC; let
the renderer own authority or grant persistence; reuse a guest session; or
provide an external-browser connector. Each either joins hostile content to
privileged authority or removes a main-process enforcement point.

**Consequences.** Guest lifecycle and policy are fail-closed. A dApp/origin
switch recreates the guest. Grants persist only approved connection/read scopes
and bind exact origin, wallet, network genesis, and scopes; signing and staged
witnesses remain memory-only. The current baseline is not the accepted target:
all target components and production guest launch remain disabled until the
listed implementation and release gates have evidence.

### Protected Assets And Attacker Model

Protected assets include funds and signing authority; exact transaction,
message, submission, and witness bytes; addresses, UTxOs, balances, public
keys and xpubs; wallet/network identity and grants; passphrases; mutual-TLS
material; filesystem, shell, logging, update, store, hardware, and other
privileged IPC authority; catalog/policy integrity; and privacy-sensitive
origins and wallet associations.

The attacker controls a dApp's top-level document, scripts, workers,
subframes, navigation, malformed and concurrent requests, resource hosts,
storage attempts, redirects, and DNS/network behavior. The attacker may fully
compromise the guest renderer, including a catalogued dApp. Main, trusted local
UI, cardano-wallet, cardano-node, OS, and device firmware are distinct trusted
dependencies, not assumed to be infallible; their failures are constrained by
the gates and capability checks below.

### Boundary And Authority Invariants

| Boundary | Authority and validation | Fail-closed outcome |
|---|---|---|
| Network/DNS to guest session | Main/session policy enforces HTTPS/WSS on each actual connection destination. Initial navigation, redirects, subresources, WSS, DNS changes/rebinding, and IPv4, IPv6, and IPv4-mapped IPv6 forms are covered. DNS preflight alone is insufficient. | Deny the destination; production Diagnostics stays disabled if its initial destination cannot be connection-bound. |
| Hostile top frame, subframes/workers, preload, and guest WebContents | Only the exact guest top frame, canonical origin, document generation, and fresh session may invoke the dedicated gateway. | Reject; revoke capability before teardown or stale-result release. |
| Guest gateway to main and trusted IPC | Guest uses a dedicated schema-validated broker. Existing IPC accepts only the exact trusted main WebContents and main frame. | Reject without privileged side effect. |
| Main to trusted UI/executor | Main-issued request ID selects immutable broker-owned arguments; trusted UI returns decision plus identity only. | Reject mismatched, expired, or lifecycle-cancelled approval. |
| Trusted executor to cardano-wallet | Trusted executor uses mutually authenticated TLS only with the local wallet backend. cardano-wallet is authoritative for wallet UTxOs, ownership, and pending-submission state; main validates broker-bound result identity. | Reject backend, context, or result mismatch without guest release. |
| cardano-wallet to cardano-node/network | cardano-wallet and node provide the reviewed ledger context and normal submission outcome; main never treats renderer summaries as authoritative. | Treat unavailable or inconsistent context and rejected submission as typed failure. |
| Hardware service to physical device | The device signs only capability-supported broker-owned requests. Main verifies returned body hash, public key, witness, or COSE signature before release. | Fail closed for unsupported fields, device errors, or invalid returned material. |
| Persistent and ephemeral state | Main-owned grants/collateral are atomic persistent records; capabilities, approvals, signing state, and staged witnesses are memory-only; guest storage is nonpersistent. | Treat corruption or mismatch as revoked; clear guest state on teardown. |
| Logs, analytics, crash reporting, package sandbox | Sensitive wallet, transaction, signature, origin, and full Diagnostics URL data is excluded; packaged OS sandbox is a release gate. | Do not log sensitive values; disable guest launch if containment proof is absent. |

Authority always binds the exact guest WebContents and top frame, canonical
origin, document generation, fresh session, route-selected eligible Shelley
wallet, monotonic route epoch, network ID/magic/genesis, negotiated extensions,
scopes, and main-issued connection/request IDs. Invalid wallet routes never
fall back. Navigation, reload, route/wallet/network change, guest failure or
close, trusted renderer reload, and revocation invalidate authority before
result release. An already authorized submission is the narrow exception: it
continues against the fixed wallet/network, but no stale guest receives a
result.

WebRTC/data channels, STUN/TURN, WebTransport, QUIC, and every other
non-proxied or unaudited transport remain disabled. A compatibility exception
requires a concrete need, equivalent connection-level enforcement, and security
review.

### Consent, Exact Bytes, And Availability

Main stores immutable validated request bytes. Trusted UI receives
broker-authoritative review data keyed by the request ID and cannot substitute
bytes. Connection and elevated disclosure use trusted consent; every signing,
data-signing, and submission call needs fresh consent, and signing never waives
submission consent.

VKey witnesses cryptographically sign exact body bytes, but review binds more:
the exact body, existing witnesses, outer `isValid`, auxiliary data, script data
and redeemers, datums, native/Plutus/reference scripts, authenticated resolved
context snapshot or digest, complete decoded semantic effects, and all checked
commitments. Missing, unknown, unsupported, mismatched, or changed material
fails closed. Submission consent separately binds the complete final immutable
envelope after witness assembly, including its body, final witness set,
`isValid`, auxiliary data, script data, datums, and scripts; nothing is added or
substituted after approval.

Requests above 64 KiB CBOR, CIP-103 batches above 50 items, pages above 100
entries, and consent inactive for five minutes reject with the frozen typed
error before side effects. Within-limit crashes, slowness, queue pressure, or
ordinary rejection are robustness defects. Any authority confusion, privileged
access, sensitive-data leak, review/byte mismatch, unverified signer result,
private-network or transport bypass, or unsandboxed production guest is a
release-blocking confidentiality/integrity defect.

### Threat Traceability And Release Gates

Task-001 establishes this model, not the downstream proof. Phase-0 evidence
owners are `task-003` for the reviewed cardano-wallet contract, consistency,
migration/rollback, and pin gate; `task-004` for exact-CBOR/body/output and
supported-era evidence; `task-005` for packaged Linux `.deb`/`.rpm` sandbox
strategy and proof (portable `.bin` rejected; research 06); and `task-006` for
Ledger/Trezor library, model, firmware, message-signing, and returned-hash
matrices. Phase 1 packaging follow-through is `task-108` (`.deb`), `task-109`
(`.rpm`), `task-110` (`.bin` retirement and auto-update migration), and
`task-103` (flag removal and canary). Phases 1 through 9 implement and validate
privileged IPC, session/network policy, exact semantic review,
backend/pending-submission behavior, device capability, packaged hostile tests,
internal/external review, and controlled rollout.

Production launch remains disabled until sender/main-frame authentication,
trusted navigation lock, packaged sandbox proof, connection-bound HTTPS/WSS
egress, complete ledger review, exact-byte signer/result validation,
pending-submission fault testing, privacy inspection, physical hardware
certification, internal review, external audit, Electron/Chromium review, and
release-candidate change control are complete.

### Backend Contract Validation

Task-003's proposed cardano-wallet delivery contract is recorded in
[`research/03-cardano-wallet-backend-contract.md`](research/03-cardano-wallet-backend-contract.md).
It fixes strict capability negotiation, the exact-point `W/G/P` context capture
protocol, stateless context binding, reuse-first V1/V2 signing evidence,
backend-produced CIP-8/CIP-95 COSE, write-ahead wallet submission, error/privacy
boundaries, and the task-200-through-task-209 evidence assignment. The contract
is not a shipped API. For task-003, the user directed the Orchestrator on
2026-08-11 to assume cardano-wallet implementation signoff and proceed; external
owner/reviewer identities and a durable URL were not supplied and are not
fabricated. This task-003 planning assumption does not replace concrete
implementation, upstream review, migration/rollback, integration, or pin
evidence required from tasks 200-209.

Task-003 changes no sibling source and is therefore validation-only. Phase-2
tasks produce candidate cardano-wallet commits and migration/rollback evidence;
task-209 may update the Daedalus pin only after authorized sibling review and
Daedalus integration against the candidate revision.

### Exact-CBOR Validation

Task-004's approved validation evidence is recorded in
[`research/04-exact-cbor-era-coverage.md`](research/04-exact-cbor-era-coverage.md),
with the machine-readable pinned-ledger inventory, exact-span fixtures, frozen
wire-policy cases, and reproducible SDK comparison under
`source/common/cardano/fixtures/exact-cbor/`. The dependency decision retains
`@cardano-sdk/core@0.41.4` as a non-authoritative helper. Conway has conditional
fixture/inventory readiness only; Dijkstra remains
`unsupported/readiness-blocked`. Backend implementation, production parsing,
and product support remain owned by downstream tasks.

## Technical Design

### Trust Boundaries

```text
Hostile remote dApp
  -> sandboxed guest preload
  -> main-process CIP-30 broker and extension registry
  -> trusted Daedalus approval and wallet executor
  -> cardano-wallet or hardware-wallet service
  -> cardano-node
```

Main process owns:

- Guest lifecycle and session policy.
- Catalog and URL policy.
- Origin and frame authentication.
- Route lease and capability state.
- Immutable guest request bytes.
- Extension negotiation and grants.
- Pending approval records.
- Result validation before guest release.
- The durable grant and collateral-preference repositories.

Trusted renderer owns:

- Catalog, settings, and approval presentation.
- Password collection without persistence or logging.
- Existing wallet state integration.
- Backend execution where existing architecture requires renderer API access.
- Human-readable transaction and batch review rendered from broker-authoritative data.

cardano-wallet owns:

- Wallet-scoped pending-submission state and reconciliation.
- Wallet UTxO, history, and chain state from which collateral readiness is derived.

The approval response can contain decision and request identity only. It cannot replace transaction/message bytes. A renderer-side executor receives broker-owned arguments over sender-authenticated IPC keyed by the main-issued request ID, and the broker verifies the returned body hash, witness set, COSE data, or submission result before guest release.

### Guest Window

Use an independent, non-modal `BrowserWindow` with a locally supplied title and native frame.

```ts
{
  show: false,
  frame: true,
  fullscreenable: false,
  autoHideMenuBar: true,
  webPreferences: {
    session: guestSession,
    preload: dappPreloadPath,
    nodeIntegration: false,
    nodeIntegrationInWorker: false,
    nodeIntegrationInSubFrames: false,
    contextIsolation: true,
    sandbox: true,
    webSecurity: true,
    allowRunningInsecureContent: false,
    webviewTag: false,
    devTools: false,
    plugins: false,
    spellcheck: false,
    enableWebSQL: false,
    navigateOnDragDrop: false,
    disableDialogs: true,
    autoplayPolicy: 'document-user-activation-required'
  }
}
```

Session requirements:

- `session.fromPartition(randomNonPersistName, {cache: false})`
- `session.isPersistent() === false`
- `session.getStoragePath() === null`
- Default-deny permission check and request handlers.
- Deny display media, HID, USB, serial, Bluetooth, WebAuthn, clipboard permission, authentication, certificate exceptions, client certificates, downloads, pointer lock, and fullscreen.
- Disable WebRTC/data channels, STUN/TURN, WebTransport, QUIC, and every other network path that can bypass the audited HTTPS/WSS destination policy. No compatibility exception is enabled without a concrete dApp requirement, equivalent connection-level enforcement, and security re-review.
- Curated subresources use exact catalogued HTTPS/WSS origins.
- Diagnostics subresources may use public HTTPS/WSS origins but cannot access loopback, private, link-local, file, custom, or insecure schemes.
- Public/private network classification covers IP literals and every resolved IPv4/IPv6 address. Hostname allow decisions must be bound to the actual connection destination, preferring Chromium Private Network Access enforcement where it provides equivalent connection-level guarantees, with a guest-specific resolver/proxy added only for demonstrated gaps; a pre-request DNS check with a time-of-check/time-of-use gap is insufficient.
- The initial top-level connection, redirects, subresources, DNS answer changes, DNS rebinding, IPv4-mapped IPv6, and WSS receive the same destination policy. If Electron cannot provide connection-level proof for the initial Diagnostics URL, Diagnostics launch remains disabled; failure for an optional cross-origin destination disables that destination rather than weakening the boundary.
- Production top-level navigation remains on the exact canonical origin.
- Cross-origin redirects are denied and destroy the guest.
- Popups are denied and are not automatically opened externally.
- Main-frame navigation increments document generation and revokes live capabilities.
- Guest closes on crash, unresponsive state, preload error, or irrecoverable load failure.
- Teardown revokes first, destroys the guest, closes connections, clears data/auth/DNS state, and rejects pending calls.
- Production guest DevTools remain disabled.
- Development DevTools require a non-production build policy and cannot be page-requested.

### Linux Sandbox And Packaging

- **Accepted strategy (2026-08-12):** ship Linux exclusively as system **`.deb`** and **`.rpm`** packages. Evidence and ownership: [research/06-linux-system-package-decision.md](./research/06-linux-system-package-decision.md). Portable feasibility negative evidence: [research/05-linux-chromium-sandbox-packaging.md](./research/05-linux-chromium-sandbox-packaging.md).
- **Rejected for Linux shipping:** portable self-extracting `.bin` to `$HOME/.daedalus/<cluster>`, AppImage, Flatpak, Snap, and other non-deb/rpm channels.
- Install to a fixed path under `/opt/` so postinst can establish Chromium sandbox prerequisites:
  - root-owned `chrome-sandbox` mode `4755` when unprivileged user namespaces are unavailable;
  - unprivileged user namespaces when the host supports them;
  - AppArmor profile with `userns,` for the fixed Electron binary path on Ubuntu 24.04+ and other AppArmor hosts that restrict unprivileged userns (install only when `apparmor_parser` accepts the profile ABI).
- Remove default `--no-sandbox` and `--disable-setuid-sandbox` launch configuration from development and packaged `.deb`/`.rpm` launchers.
- Detect `--no-sandbox`, sandbox-disabling environment variables, and unsupported packaging at runtime.
- Keep Daedalus wallet functionality available where practical, but hide and reject dApp guest launch when OS sandbox proof is unavailable.
- Never auto-retry with `--no-sandbox` and never weaken containment for remote content.
- Packaged tests must verify active seccomp/no-new-privileges or equivalent OS containment on the exact guest renderer PID, not only `process.sandboxed`.
- Retire the portable `.bin` producer, home-extract installer, and `.bin`-oriented Linux auto-update path; migrate existing home installs to system packages without deleting wallet data under `XDG_DATA_HOME/Daedalus`.

### Existing IPC Hardening

Before guest creation:

- The trusted main window now loads one canonical local document, rejects
  untrusted main-frame navigation and redirects, denies every subframe and
  popup, and keeps policy-aborted loads out of renderer recovery.
- Renderer-requested external opening now accepts only parsed, credential-free
  HTTPS URLs, awaits the shell operation, and returns privacy-safe failures.
- Refactor privileged handlers to retain and authenticate the Electron event.
- Require exact trusted main `WebContents` and main frame for existing renderer channels.
- Move remaining import-time side-effect listeners into explicit initialization.
- Fix uncorrelated shared response channels and response-listener ordering races.
- Do not use legacy `IpcChannel` or `IpcConversation` as the guest protocol.

Guest broker uses a dedicated scoped gateway with main-issued request IDs and runtime-validated discriminated method schemas. No raw method lookup, fallback dispatch, or generic privileged IPC object is exposed.

### Route Lease

```ts
type DappRouteLease = {
  walletId: string;
  routeEpoch: number;
  networkGenesis: string;
};
```

- Main observes trusted in-page route changes and maintains a monotonic epoch.
- Guest launch is valid only for an exact `/wallets/:id/dapps` route.
- Diagnostics creates an in-memory one-use pending launch bound to a selected eligible wallet, navigates trusted UI to that wallet's dApp route, and launches only after the matching lease commits.
- A pending Diagnostics URL is consumed once, cancelled on any route/wallet mismatch, and never permits launch directly from the Diagnostics route.
- Every enable, approval, signing execution, and submission authorization revalidates the lease. Result release also revalidates it.
- After submission authorization is recorded, the fixed wallet/network submission continues even if the lease is later revoked; no result is released to a stale guest.
- Direct hash/history changes revoke the lease.
- Wallet refresh preserving the same wallet ID keeps the lease.
- Wallet deletion, replacement, unsupported type, or network change revokes it.
- Invalid dApp routes do not execute WalletsStore's first-wallet fallback.

### Extension Registry

```ts
type ExtensionDescriptor = {
  cip: number;
  status: 'active' | 'proposed';
  namespace: string;
  dependencies: number[];
  scopes: DappScope[];
  methods: Record<string, MethodDescriptor>;
  baseOverrides?: Record<string, MethodDescriptor>;
};
```

- Startup fails on duplicate public paths, dependency cycles, undeclared base overrides, or missing schemas/scopes.
- Known, supported, and enabled are distinct states.
- Capability predicates include wallet kind, backend API version, device/app/firmware, network, and product policy.
- Invocation rechecks capability to prevent time-of-check/time-of-use drift.
- CIP-95 declares a base `signTx` override.
- CIP-103 composes with the active base/CIP-95 signer.
- CIP-104 and CIP-142 declare Proposed policy gates.
- CIP-106 and CIP-141 have no runtime descriptor in this plan.

### Capability State

```ts
type DappCapability = {
  guestWebContentsId: number;
  documentGeneration: number;
  dappId?: string;
  origin: string;
  connectionId: string;
  walletId: string;
  routeEpoch: number;
  networkId: number;
  networkMagic: number;
  networkGenesis: string;
  enabledExtensions: number[];
  grantedScopes: DappScope[];
};
```

Live capability is memory-only and revoked on navigation, reload, guest close, route change, wallet deletion, network change, trusted renderer reload, explicit disconnect, node shutdown, or grant revocation. Apart from the consent inactivity timeout, there is no request timeout; lifecycle cancellation remains mandatory. Revocation suppresses stale result delivery but does not cancel a submission that the user already authorized.

### Persistent Grants

```ts
type DappGrant = {
  schemaVersion: number;
  origin: string;
  walletId: string;
  networkGenesis: string;
  networkMagic: number;
  readScopes: DappScope[];
  enabledExtensionScopes: number[];
  launch:
    | {
        kind: 'catalog';
        catalogEntryId: string;
      }
    | {
        kind: 'diagnostics';
      };
  grantedAt: string;
};
```

- Store through a dedicated main-owned atomic repository.
- Do not use renderer local storage or the generic renderer-writable electron-store conversation as the authority.
- Base read and elevated key-disclosure scopes are independently visible and revocable.
- Persistent grants never provide reusable signing or submission authority.
- Origin canonicalization uses HTTPS scheme, ASCII host, and effective port.
- Reject credentials, opaque origins, certificate errors, and deceptive URL forms.
- Bind grants to network genesis, not magic alone.
- Catalog grants bind the exact catalog entry and become invalid when that entry is removed or changed in a release; emergency invalidation uses the launcher kill switches or a grant schema-version bump.
- `isEnabled()` reports a valid persisted base connection grant for the current route lease.
- Closing a guest retains the grant.
- `Forget connection` deletes the grant; a separate `Close` only tears down the live guest.
- Corruption fails closed and surfaces a settings-repair path.

### Catalog

```ts
type DappCatalogEntry = {
  id: string;
  nameMessageId: string;
  iconAsset: string;
  entryUrlByNetworkGenesis: Record<string, string>;
  canonicalOrigin: string;
  allowedResourceOrigins: string[];
  supportedWalletKinds: string[];
  supportedExtensions: number[];
};
```

- Main resolves opaque `id`; renderer/guest cannot supply a replacement URL.
- Names, icons, and descriptions are local trusted assets/messages.
- Do not use page titles, favicons, manifests, or page HTML in trusted UI.
- Initial updates arrive only through Daedalus releases.
- Catalog copy states that preferred means compatibility-tested, not audited or endorsed.
- Catalog network mismatches fail before guest creation.
- Removing or changing a catalog entry in a release invalidates that entry's grants.

### Transaction Byte Engine

Add a shared engine with two layers:

1. A low-level CBOR cursor extracts the exact original transaction-body span, witness-set span, outer `isValid`, auxiliary data, and exact output spans without reserialization.
2. A semantic decoder maps every supported era-specific field into a review/device model.

The engine must:

- Hash exact body bytes with Blake2b-256.
- Reject trailing bytes, duplicate map keys, invalid set encodings, malformed envelopes, unsupported eras, and unknown body fields.
- Preserve body field order and tagged-set representation needed by hardware serialization.
- Decode normal, collateral, and reference inputs.
- Decode outputs, values, datum hashes, inline datums, and reference scripts.
- Decode fee, validity interval, mint/burn, withdrawals, certificates, deposits/refunds, required signers, native scripts, Plutus scripts, redeemers, execution units, script-data hash, auxiliary data, governance, collateral return, and total collateral.
- Verify the body `auxiliary_data_hash` against the exact supplied auxiliary-data bytes and reject missing, extra, or mismatched committed auxiliary data.
- Recompute and verify `script_data_hash` from the exact redeemers, datums, and pinned ledger language views whenever the body commits to script data.
- Verify every supplied or backend-resolved datum, native script, Plutus script, and reference script against the datum hash, script hash, credential, policy ID, or reference expected by the body and resolved inputs.
- Verify existing VKey/bootstrap witnesses against the exact body hash when they participate in completeness or submission review, while never reporting them as newly generated witnesses.
- Treat any unavailable data needed to validate a body commitment or render complete effects as an incomplete review and fail closed.
- Recognize CIP-95 Conway fields even when the selected hardware device cannot encode them.
- Cross-check semantic context against backend ledger decode/context data.
- Fail closed rather than show an incomplete signable summary.

Because VKey witnesses sign only the transaction body and not outer `isValid`, signing review must display maximum collateral loss even when incoming `isValid=true`. Submission receives a separate exact-envelope review.

### Backend Transaction Context

Add a versioned cardano-wallet operation that returns one coherently captured
context:

- Chain point, volatile delta, era, protocol version, network identity, and protocol parameters.
- Exact TxOut CBOR for every normal, collateral, and reference input.
- Wallet UTxO snapshot with exact `transaction_unspent_output` CBOR.
- Local pending overlay.
- Wallet ownership and derivation evidence.
- Required wallet key witnesses under pinned ledger rules.
- Relevant stake/DRep registration, deposit, certificate, and governance state.
- Earlier-batch outputs derived by hashing/parsing the exact preceding transactions.
- A context digest binding the response to the transaction body set and chain context.

The backend, not the renderer, derives earlier outputs and ownership. Renderer-supplied paths are never authoritative.

The capture protocol is not globally atomic across wallet DB and node LSQ. It
reads wallet point `W`, wallet generation `G`, and pending generation `P`,
queries node state exactly at `W`, then confirms unchanged `W/G/P`; it retries
the complete capture at most three times and otherwise fails closed without a
partial response. Exact provenance, digest/token encoding, restart behavior,
and downstream tests are frozen in the task-003 backend contract.

The UTxO endpoint or replacement context API must query full ledger outputs through local-state query because wallet `TxOut` persistence is lossy. All cardano-wallet implementation paths in this plan refer to the sibling `../cardano-wallet` working tree and must land there before Daedalus updates its reviewed pin. Upstream acceptance into cardano-wallet is the default path for every new API; any long-lived fork divergence requires explicit sign-off recorded in the backend contract validation task.

### Software Signing

The existing `transactions-sign` endpoint combined with main-process witness-set diffing is the baseline for witness-only responses; a new backend signing endpoint requires written justification from the backend contract validation task.

The backend request binds exact transaction bytes, reviewed context token and
digest, ordered current-request parents, request index, and `partialSign`. Reuse
of `transactions-sign` may return its existing full modified transaction only
after both V1 and V2 paths prove that the body, envelope, and every pre-existing
witness class are unchanged except for valid newly added VKeys. Daedalus then
performs task-306 exact-envelope validation and witness differencing. A distinct
backend witness-only result is permitted only when task-003's path-specific
reuse evidence justifies the smaller replacement endpoint. In either case the
value released to CIP-30 is a verified VKey-only witness set.

- Backend verifies the reviewed context digest, exact body bytes, and wallet ownership evidence before signing, but it does not refresh chain state or require the original chain point to remain current.
- If chain state advances, rolls back, or spends an input after review, signing may still complete and later node submission may fail normally.
- Single signing may be implemented through the batch primitive with one item.
- Batch orchestration may call the reviewed single backend seam sequentially,
  but Daedalus releases no witness result unless every item succeeds.
- Existing witnesses are preserved in any full-transaction backend result and
  excluded by Daedalus from the newly generated VKey-only result.
- Returned VKey witnesses are verified and deduplicated.
- `partialSign=false` evaluates complete required key/native-script satisfaction after applying all producible wallet witnesses.
- `partialSign=true` returns all newly producible owned VKeys, including a canonical empty witness set when the wallet controls no applicable key; missing non-wallet proofs do not cause `ProofGeneration` in partial mode.
- Collateral-only paths, explicit required signers, withdrawals, stake certificates, DRep certificates/votes, and policy keys are included where supported.
- Reference inputs do not request spending witnesses.
- Password remains transient and is never persisted or logged.

### CIP-95 Backend

- Introduce an explicit CIP-105 DRep derivation domain at role 3/index 0.
- Derive public stake and DRep children from the stored account xpub without password/device interaction.
- Enumerate discovered stake public keys and classify registration against ledger state plus pending certificates.
- Add DRep private-key derivation for software signing.
- Add DRep CIP-8 signing.
- Add DRep certificate and governance-vote witness analysis.
- Recognize all current Conway transaction fields 0 through 22.
- Reject deprecated Genesis/MIR certificate requests with code 3.
- Do not sign constitutional-committee or pool credentials under CIP-95.

### CIP-104 Backend

- Reuse the stored account xpub returned by cardano-wallet's extended account-key API.
- Hardware account xpubs already stored by Daedalus/cardano-wallet are eligible without reconnecting the device.
- Decode Bech32 account-xvk form to 64 raw bytes.
- Encode only after the interoperability task locks the exact CBOR form.
- Revalidate route account before release.

### Submission

- Use wallet-scoped transaction submission for dApp and hardware transactions.
- Correct the current octet-stream request type/content-length ambiguity by accepting an explicit CBOR-hex string or exact byte buffer contract.
- Commit the wallet-scoped `authorized` record and normal/collateral pending
  claims in one database transaction before any node call, then commit the
  `broadcasting` generation before broadcast. Reconcile every crash boundary
  through the task-003 write-ahead state machine.
- Verify the returned hash equals the locally calculated transaction ID.
- Re-submitting an exact already-pending transaction is idempotent and returns its existing hash.
- Treat submission as a point of no return once the user gives explicit confirmation.
- Guest navigation after authorization drops the response but does not stop or undo the node attempt.

### Hardware Transaction Signing

Create a vendor-neutral exact transaction adapter instead of extending coin-selection reconstruction.

For each transaction:

1. Retain exact body bytes and expected body hash.
2. Resolve owned normal/collateral inputs and credential paths from trusted backend context.
3. Verify output ownership by deriving the path and matching the encoded address.
4. Map every present field to the vendor request.
5. Reject before device interaction if any field or encoding cannot be represented exactly.
6. Select ordinary, Plutus, pool, or governance signing mode from semantics.
7. Require the vendor-returned hash to equal the expected exact body hash.
8. Verify each returned public key hash and Ed25519 signature.
9. Reject missing or extra device witnesses.
10. Return a fresh VKey-only witness set.

Ledger requirements:

- Upgrade/validate Ledger JS 8.x for Cardano app v8 compatibility.
- Preserve support for app v7 where certified.
- Support input/output datum/reference scripts, mint, script-data hash, collateral, required signers, network ID, reference inputs, supported certificates, DRep certificates, supported voting, treasury, and donation.
- Reject proposal procedures and unsupported multi-voter/multi-vote structures.
- Base CIP-8 uses full-address mode; DRep CIP-8 uses key-hash mode.
- Add transport cancellation by closing/invalidating transport and ignoring late generations.

Trezor requirements:

- Support fields represented by Trezor Connect 9.7.2.
- Reject unsupported DRep certificates, governance voting procedures, proposals, treasury, donation, committee/DRep certificates, and other absent protocol fields.
- Support Model T and compatible Safe/Core devices for CIP-8 on firmware meeting the configured gate.
- Reject Trezor One for `signData`.
- Supply address parameters for base `signData` and verify full-address/credential equality.
- Validate payload hex before vendor calls.

No feature-parity claim is made. Device capability rejection maps to `ProofGeneration`.

### CIP-103 Batch Engine

#### Preflight

- Validate the complete outer array and every transaction before side effects.
- Preserve each original full-CBOR digest and exact body hash.
- Capture one coherent backend context at a named chain point.
- Index every batch-produced output by `txId#index`.
- Resolve every normal, collateral, and reference input from node state, cardano-wallet pending-submission state, or an earlier output in the current batch.
- Reject self references, forward references, unresolved inputs, context byte mismatches, network mismatches, and unsupported era/fields.
- Input resolution is required even with `partialSign=true`.

#### Sequential Overlay And Conflict Detection

- Items resolve in caller order against node state, cardano-wallet pending-submission state, and the outputs and spent set of earlier items in the batch.
- Earlier output use creates a dependency; reference inputs create dependencies but no spending claims.
- An item that spends an input already claimed by an earlier item is flagged as conflicting with that earlier item's index.
- Flagged conflicting items remain signable with informed user consent; attempt-all submission produces the normative mixed result for them.
- Duplicate items are signed independently; no request aliasing or deduplication is performed.
- Never topologically reorder caller input.

#### Review

- Show item position and body hash.
- Show full per-item effects and wallet ownership.
- Show current-batch parent dependencies and backend pending-submission dependencies.
- Show conflict flags referencing the earlier item index.
- Show per-item effects rather than a misleading aggregate across conflicting items.
- Show maximum collateral risk for every signed body.
- One trusted sign review covers the whole ordered request.
- One separate trusted submit review covers the whole ordered submission request.

#### Signing

- Acquire the per-wallet in-process send lock.
- Stage software or hardware witness sets in memory.
- Hardware may show one physical confirmation per transaction.
- User/device cancellation invalidates the operation generation.
- Late hardware/backend results are ignored.
- Any failure discards every staged result.
- Signatures already produced by a hardware device cannot be cryptographically revoked, but Daedalus must not release them.

#### Submission

- Prevalidate all CBOR before first node side effect.
- Submit every item sequentially in caller order through wallet-scoped submission.
- Catch every item result before advancing.
- Continue after parent or prior-item failure.
- Guest closure, navigation, or route-lease revocation after authorization does not stop the remaining attempts; it only prevents result delivery to that guest.
- If the app exits mid-call, un-attempted items are not submitted; cardano-wallet pending-submission state reconciles and an exact retry returns existing hashes.
- Resolve only on all-success; otherwise reject with the mixed aligned array.

### Submission Recovery

- cardano-wallet's wallet-scoped pending-submission state is the sole durable submission record and reconciliation source; Daedalus keeps no separate submission journal.
- Signing state, derived outputs, and staged witnesses remain in memory and are discarded when the signing call ends or the app exits; no password or staged witness is ever persisted.
- Startup never submits a merely signed transaction and never resumes an interrupted batch call.
- After an app exit mid-submission, already-broadcast items reconcile through cardano-wallet pending state and transaction history; un-attempted items are absent from chain and mempool, and dApps may retry the call idempotently.
- Native sends and dApp submissions serialize per-wallet through a shared in-process lock held for the selection-sign-submit sequence.

### Collateral Management

#### Authority And Persistence

- Daedalus main owns the persisted collateral-preference record in a dedicated repository, following the same authority pattern as grants; renderer and guest never write it directly.
- The stored record is minimal: wallet, network genesis, target amount, preferred inputs, and schema version.
- Readiness (`checking`/`ready`/`preparing`/`in-use`/`will-be-spent`/`charged`/`stale`/`not-ready`) is derived at read time from cardano-wallet UTxO, history, and pending-submission state; no cardano-wallet schema change is required for collateral preference.
- Corruption of the preference record fails closed and surfaces a settings-repair path; wallet funds remain governed by ledger/cardano-wallet state.

#### Policy

Collateral is a persisted soft preference:

- It is not removed from total balance.
- It remains visible in CIP-30 `getUtxos` and `getBalance`.
- Normal coin selection is unchanged; ordinary sends may spend it like any other UTxO.
- Transaction review states when preferred collateral will be spent.
- Daedalus then adopts another candidate or offers preparation of a replacement.
- Clearing the preference requires no transaction.

#### Model

The projected read model is:

```ts
type CollateralPreference = {
  schemaVersion: number;
  walletId: string;
  networkGenesis: string;
  targetLovelace: string;
  preferredInputs: TxIn[];
  generation: number;
  state:
    | 'checking'
    | 'ready'
    | 'preparing'
    | 'in-use'
    | 'will-be-spent'
    | 'charged'
    | 'stale'
    | 'not-ready';
};
```

#### Candidate Rules

- Payment-key controlled.
- Pure ADA for candidates returned by CIP-30 `getCollateral`.
- Total value at least the requested amount.
- Count compatible with current `maxCollateralInputs`.
- Deterministically choose the smallest sufficient combination.
- Default target is 5 ADA, but a caller may request more and Daedalus adds no policy maximum.
- The deprecated `getCollateral` compatibility endpoint returns `null` when existing candidates cannot satisfy the requested valid `Coin`; it never prepares collateral automatically. Malformed CBOR or an invalid ledger `Coin` returns `APIError.InvalidRequest`.
- If preferred inputs no longer qualify, reconcile and choose another candidate.

#### Preparation

- Automatically adopt an existing suitable candidate without a chain transaction.
- If absent, show an explicit `Prepare collateral` action.
- Build a normal self-transfer creating a 5 ADA pure-ADA output.
- Compute current minimum output ADA and fee from protocol parameters.
- Require normal software password or hardware confirmation.
- Mark `preparing` through pending/confirmation.
- Record preference only after backend observation.
- Never silently sign or submit preparation.

#### Transaction Validation

- Verify collateral-input existence and payment-key ownership.
- Verify collateral witness paths.
- Verify count against current protocol parameters.
- Verify minimum collateral `ceil(fee * collateralPercentage / 100)`.
- Verify `total_collateral` equality when present.
- Verify collateral return minimum ADA.
- Verify all non-ADA assets from token-bearing collateral are returned exactly.
- Require collateral return to an active-wallet-owned address as Daedalus policy.
- Show full input value as maximum risk when no valid return exists.
- Show declared charge and expected return when CIP-40 fields are valid.
- Model `isValid=true` and `isValid=false` outcomes separately.
- If a preferred input appears as an ordinary dApp input, show the readiness impact but do not prohibit user-approved signing.

#### Pending And Recovery

- Wallet-scoped submit records ordinary and collateral inputs in pending state.
- Valid script completion keeps collateral available and returns preference to `ready`.
- An invalid accepted script transaction leads to derived `charged` or `stale` state and starts replacement evaluation.
- Rollback restores preference when the UTxO reappears.
- A preferred UTxO may back more than one valid pending transaction under ledger rules, but conflict/conditional-risk state must be visible in batch and pending context.

### Renderer UX

#### Routes And Navigation

- Add `ROUTES.WALLETS.DAPPS = '/wallets/:id/dapps'`.
- Add dApps to wallet navigation, excluded for legacy wallets.
- Keep catalog and collateral readiness under the active wallet route.
- Add `/settings/dapp-connections` for global grant inspection/revocation.
- Add diagnostics URL entry without placing the full URL in route/query persistence.

#### Global Approval Coordinator

- Mount at `App.tsx` level so route transitions cannot orphan requests.
- Main issues correlated request IDs.
- Queue concurrent requests rather than applying a connector concurrency rejection.
- Pending consent auto-rejects after five minutes without user interaction, using the canonical declined/refused error for the method; no other request timeout is introduced.
- Guest remains hidden while any sensitive request awaits decision or execution.
- Node stop, wallet change, navigation, close, or capability revocation rejects requests that have not crossed submission authorization. Already-authorized submissions continue without releasing a result to a stale guest.

Approval surfaces:

- Connection and read access.
- CIP-95 governance public-key disclosure.
- CIP-104 account-xpub disclosure.
- Single transaction signing.
- Batch transaction signing.
- Base/CIP-95 data signing.
- Single transaction submission.
- Batch transaction submission.
- Collateral preparation.
- Spending preferred collateral in a normal or dApp transaction.

#### Transaction Review

- Never use guest-provided summaries.
- Show origin, local catalog identity, wallet, network, and extensions.
- Show ordered inputs/outputs, ADA, assets, fees, mint/burn, metadata, certificates, withdrawals, deposits/refunds, scripts, datums, redeemers, required signers, governance, collateral, and auxiliary data.
- Show exact unsupported reason before consent rather than after device connection.
- Show raw CBOR/body hash as an advanced verification affordance.
- Refuse signability when semantic decoding is incomplete.
- Batch review shows dependencies and conflict flags.
- SignData review shows exact payload hex and safe decoded preview.

#### Collateral UX States

- `Checking`: node/wallet context is loading.
- `Ready`: suitable preferred collateral exists; copy says preferred, not locked.
- `Not ready`: no candidate; show prepare action.
- `Preparing`: setup transaction awaiting confirmation or chain observation.
- `In use`: referenced by pending script transaction.
- `Will be spent`: selected by ordinary coin selection in the reviewed transaction.
- `Charged`: invalid accepted script consumed collateral.
- `Stale`: preferred outpoint is missing or rolled back pending reconciliation.

#### Copy And Accessibility

- All messages ship in polished en-US and ja-JP.
- Explain that preferred catalog status is not endorsement.
- Explain that 5 ADA is a compatibility target, not protocol-mandated collateral.
- Explain maximum collateral risk when no CIP-40 return exists.
- Explain permanent privacy impact before CIP-104 disclosure.
- Explain governance correlation before CIP-95 key disclosure.
- Keyboard focus returns to the originating trusted control after approval.
- Hostile origin strings are rendered as escaped text only.

### Network And Sync Gates

- Provider metadata and network identity may be available from local configuration.
- Wallet read methods require a loaded responsive wallet and node connection.
- UTxO, collateral, transaction-context, signing, and submission require full `NetworkStatusStore.isSynced` readiness, including acceptable system time.
- Node disconnection during approval cancels the live request with a typed failure while retaining the persistent grant.
- Wallet restoration or nonresponding state prevents guest launch or method execution.
- Account/network change requires `enable()` again; no nonstandard change event is added.

### Observability And Privacy

The main-owned authoritative grant repository persists the canonical exact origin, wallet ID, network genesis, scopes, and catalog entry or Diagnostics launch kind needed to enforce and revoke a grant. A full Diagnostics URL is never persisted anywhere; only the canonical origin may persist in the grant repository.

Authoritative wallet history, pending-submission stores, and collateral state may persist any transaction and wallet data Daedalus/cardano-wallet already persist today; this plan introduces no additional Daedalus-side transaction persistence. These repositories use restrictive permissions and documented retention, and their contents never enter observability channels.

Allowed observability fields:

- Catalog dApp ID, never arbitrary origin.
- Method category, never payload.
- Payload byte length and item count only if existing telemetry policy permits.
- Result category and elapsed time.
- Device family/capability category without serial number.

Forbidden from observability and non-authoritative storage:

- Full arbitrary Diagnostics URLs in logs, analytics, error text, crash reports, routes, or non-authoritative caches.
- Arbitrary Diagnostics origins or origin-to-wallet associations in logs, analytics, error text, crash reports, or non-authoritative caches.
- Addresses, UTxOs, transaction CBOR, body hashes, assets, metadata, DRep/stake public keys, account xpubs, COSE data, signatures, witnesses, passphrases, or hardware identifiers in logs, analytics, error text, crash reports, or non-authoritative caches.
- Grant associations copied into logs, analytics, non-authoritative caches, or renderer/guest storage; writable collateral-preference copies outside the main-owned repository.

## Components Affected

### Main Process

- `source/main/index.ts`: early security hooks, broker lifecycle, route lease, shutdown.
- `source/main/windows/main.ts`: trusted-window navigation lock and sender authority.
- `source/main/webpack.config.js`: dedicated preload entry and development sandbox flags.
- `source/main/preloads/dapp.ts`: capability-limited guest bridge.
- `source/main/dapp/`: browser manager, session policy, URL policy, catalog, route lease.
- `source/main/cip30/`: registry, negotiation, dispatcher, session/grant/consent coordinators, collateral preference service, method handlers.
- `source/main/cardano/`: narrow dApp backend client/context service.
- `source/main/hardware/`: extracted arbitrary transaction/message hardware service.
- `source/main/ipc/`: authenticated trusted controls and explicit handler initialization.
- `source/main/cip103/`: batch context and sequential-overlay resolution.

### Shared Code

- `source/common/cip30/`: public wire types, errors, schemas, extension descriptors.
- `source/common/cardano/`: CBOR slicing, transaction model, values, witness sets, COSE verification.
- `source/common/ipc/dapp.ts`: internal broker/trusted-renderer contracts.
- `source/common/types/dapp.types.ts`: catalog, grant, capability, approval types.
- `source/common/types/cip103.types.ts`: batch contracts.
- `source/common/types/collateral.types.ts`: collateral preference/status contracts.
- `source/common/types/hardware-wallets.types.ts`: vendor-neutral exact signing/message contracts.
- `source/common/config/dappCatalog.ts`: bundled catalog.

### Renderer

- `source/renderer/app/routes-config.ts`, `Routes.tsx`: wallet-scoped dApp route and settings route.
- `source/renderer/app/components/wallet/navigation/WalletNavigation.tsx`: dApp navigation.
- `source/renderer/app/App.tsx`: global approval coordinator.
- `source/renderer/app/stores/DappStore.ts`: trusted catalog/browser/grant presentation state.
- `source/renderer/app/stores/Cip30ConsentStore.ts`: correlated approval presentation state.
- `source/renderer/app/stores/Cip103Store.ts`: batch orchestration/view state where renderer ownership remains required.
- `source/renderer/app/stores/WalletsStore.ts`: no-fallback route behavior, shared per-wallet send lock, wallet deletion cleanup.
- `source/renderer/app/stores/HardwareWalletsStore.ts`: delegation to new arbitrary-CBOR hardware service.
- `source/renderer/app/services/Cip30WalletService.ts`: trusted wallet execution seam.
- `source/renderer/app/components/dapp/`, `containers/dapp/`: catalog, approvals, batch, collateral, grant settings.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx`: arbitrary URL launch.
- `source/renderer/app/api/`: transaction context, full UTxO, witness sign, CIP-8, wallet-scoped submit wrappers.
- `source/renderer/app/i18n/`: en-US and ja-JP messages.

### Backend And Packaging

- Sibling `../cardano-wallet`: API, wallet core, Shelley transaction, derivation/discovery, network layer, local-state-query, pending-submission, and SQLite state modules.
- `flake.nix`, `flake.lock`: pin the reviewed backend revision.
- `nix/internal/x86_64-linux.nix`: sandbox-safe `.deb`/`.rpm` launchers and package outputs.
- Linux `.deb` and `.rpm` packaging (postinst SUID helper, AppArmor profile, desktop entries, update path).
- Retire `nix/internal/linux-self-extracting-archive.sh` and portable `.bin` shipping once task-110 migration completes.
- Windows packaging source-map/output cleanup for the added preload where applicable.

## Implementation Strategy

### Phase 0: Contracts And Threat Model

- Freeze public CIP-30/8/95/103/104/142 contracts and error shapes.
- Record golden extracts and fixtures for every standards contract used by implementation.
- Resolve CIP-104 wire interoperability before advertising it.
- Define supported era and device capability matrices.
- Document hostile-renderer threat model and accepted availability exclusion.
- Define backend API version/capability handshake.
- Define feature flags, proposed-extension policy, and release gates.

### Phase 1: Electron And IPC Security Foundation

- Prove and implement Linux `.deb`/`.rpm` packaging with Chromium OS sandbox (SUID and/or userns + AppArmor), remove default sandbox-disabling launch flags, and retire the portable `.bin`.
- Lock trusted main navigation and popup/external URL handling.
- Sender/frame-scope all privileged IPC.
- Build the dedicated guest preload and guest manager only after privileged IPC migration, behind a disabled feature flag, with non-HTTPS/WSS transport stacks disabled.
- Add hostile packaged-Electron tests before wallet API exposure.

### Phase 2: Backend Foundations

- Add full-ledger transaction context and full UTxO CBOR.
- Add wallet ownership/path and current protocol/governance state.
- Add verified VKey-only Daedalus single/batch results with current-request
  parent context, using validated full-transaction differencing or a justified
  backend witness-only replacement.
- Add exact CIP-8 software signing.
- Add CIP-105 DRep derivation and stake-key registration classification.
- Add wallet-scoped pending submission.
- Land and review those changes in `../cardano-wallet`, then update the Daedalus backend pin only after backend integration and migration/rollback tests pass.

### Phase 3: Shared Cardano And Extension Engine

- Build exact CBOR span extraction and semantic transaction model.
- Build witness extraction/verification and COSE serialization/verification.
- Build extension registry, schemas, negotiation, and error envelopes.
- Build sequential-overlay conflict detection and the collateral outcome model.
- Run differential ledger and fuzz tests before signability is enabled.

### Phase 4: Browser, Grants, Read API, And Collateral UX

- Add wallet route, catalog, diagnostics launch, and browser lifecycle.
- Add route lease and main-owned grant repository.
- Add the main-owned correlated approval coordinator before connection or key-disclosure grants are exposed.
- Implement and test independent global, preferred-catalog, Diagnostics, CIP-104, and CIP-142 launcher-configuration switches before the audited baseline is frozen.
- Implement base read API and CIP-142.
- Implement CIP-95 public key getters.
- Implement gated CIP-104 after its interoperability gate.
- Add dApp connection settings and collateral preference/preparation UX.
- Keep production launch disabled until signing and audit phases complete.

### Phase 5: Software Signing And Submission

- Reuse the phase-4 global trusted approval coordinator for every signing and submission surface.
- Add exact transaction/data/submission review.
- Add software base/CIP-95 `signTx` and `signData`.
- Add wallet-scoped submission and per-wallet native-send coordination.
- Verify all public typed errors and privacy invariants.

### Phase 6: Hardware Signing

- Upgrade and certify Ledger JS where required.
- Extract hardware service and add vendor-neutral arbitrary-CBOR adapters.
- Add exact transaction hash/witness verification.
- Add base and DRep CIP-8 hardware signing.
- Add cancellation and late-result suppression.
- Complete physical model/firmware capability matrix.

### Phase 7: CIP-103

- Add ordered preflight and immutable context capture.
- Add sequential-overlay resolution and conflict-flagged review.
- Add all-or-nothing software/hardware witness staging.
- Add idempotent retry through cardano-wallet pending-submission state.
- Add normative attempt-all submission and mixed-array rejection.

### Phase 8: Hardening And Audit

- Complete packaged hostile-renderer suite.
- Complete differential/fuzz/conformance/interoperability tests.
- Complete physical hardware matrix.
- Perform internal security review.
- Perform independent external audit and remediation.
- Confirm current Electron/Chromium security posture.
- Record the audited source commit, dependency locks, backend pins, package hashes, catalog contents, and launcher-configuration variants as the release-candidate baseline.
- Any material code, dependency, packaging, protocol-policy, catalog-origin/resource, or security-boundary change after that baseline reruns affected tests and security review before rollout can continue.

### Phase 9: Rollout

- Ship disabled internal builds first.
- Enable curated testnet entries for staff and partners.
- Validate preprod and preview software/hardware flows.
- Compare every pilot/release-QA change against the audited baseline and rerun affected automated, packaged, hardware, internal-review, and external-audit gates before mainnet enablement.
- Task 903-a produces the final reviewed source commit, catalog contents, and exact packaged launcher-configuration variants for curated-mainnet and later Diagnostics rollout. Tasks 904 and 905 deploy only those reviewed artifacts through the normal release process.
- Any change required during tasks 904 or 905 returns to task 903-a change control and affected security review before activation continues.
- Enable curated mainnet catalog only after audit closure.
- Enable arbitrary diagnostics launch last.
- Preserve a launcher-config kill switch that prevents launch without deleting grants or interfering with pending-submission reconciliation. Applying it through a normal update/restart also tears down any guest from the prior process.

## Testing Strategy

### Shared Unit And Property Tests

- Strict schema validation and extension composition.
- Origin canonicalization, route lease, grant matching, and revocation.
- CBOR span extraction, body hashing, duplicate/trailing data, era fields, and set tags.
- Auxiliary-data, script-data, datum, native/Plutus/reference-script, and existing-witness commitment verification against exact body bytes.
- Value/address/UTxO serialization and pagination.
- Transaction semantic summaries for every supported field.
- CIP-8 COSE golden vectors and credential ownership.
- CIP-95 derivation, key encoding, stake registration classification, and deprecated certificates.
- CIP-104 exact encoding fixture when interoperability succeeds, or explicit namespace-omission fixtures when the gate concludes disabled.
- CIP-103 ordered dependencies, conflicts with earlier items, duplicates, invalid transactions, collateral, and references.
- Witness-set delta, merge, deduplication, and signature verification.
- Collateral preference selection, ordinary-spend reconciliation, replacement, CIP-40 validation, and rollback.
- Pending-submission reconciliation and idempotent replay.

### Backend Tests

- Full UTxO CBOR across datum/reference-script output forms.
- Context consistency at the review snapshot, including rollback fixtures; later chain changes do not trigger re-review.
- Foreign/script input resolution.
- Backend pending-submission overlay and current-batch parent-output derivation.
- Software witness-only signing and all-or-nothing batch response.
- `partialSign` completeness, native-script satisfaction, and the canonical empty partial witness-set result when no wallet key applies.
- Collateral-only signer path.
- CIP-8 payment/stake/DRep signing.
- Stake-key registration/pending classification.
- DRep certificate/vote signing.
- Wallet-scoped submit crash windows and pending persistence.
- Signing can complete from the approved snapshot after an input is spent; later node submission returns the normal failure.

### Main/Electron Tests

- Exact guest web preferences.
- Nonpersistent session and cleanup.
- Permission/device/download/popup/navigation/certificate denial.
- WebRTC/data channels, STUN/TURN, WebTransport, QUIC, and non-proxied network paths remain unavailable in release-equivalent guests.
- Wrong sender, subframe, null frame, stale generation, wrong origin, wrong route epoch, and wrong wallet rejection.
- Navigation/approval/execution/close races.
- Existing privileged IPC cannot be invoked by guest.
- Generic external URLs cannot reach shell without trusted HTTPS approval.
- Guest cannot access Node, raw IPC, TLS configuration, filesystem, electron-store, or hardware channels.
- Installed Linux `.deb` and `.rpm` packages prove OS sandboxing on the exact guest renderer.

### Renderer/Jest/Storybook

- Catalog loaded/empty/network-incompatible states.
- Preferred versus diagnostics-origin connection review.
- Base, CIP-95, and CIP-104 disclosure consent.
- Transaction review for payments, multi-assets, mint/burn, scripts, governance, metadata, collateral, and unknown-field rejection.
- Batch linear chains, conflict flags, per-item effects, and hardware progress.
- Grant settings and revocation.
- Collateral checking/ready/not-ready/preparing/in-use/will-be-spent/charged/stale states.
- Offline, syncing, account change, wallet deletion, and guest crash states.
- en-US/ja-JP and all supported themes.

### Cucumber

- Open wallet-scoped catalog and launch preferred dApp.
- Launch arbitrary diagnostics dApp.
- Enable, persist, reuse, and forget read grant.
- Active wallet route change disconnects the guest.
- Base read API on supported networks.
- Software signTx/signData/submitTx.
- CIP-95 getters, DRep signData, governance transaction signing.
- Collateral preparation and ordinary-spend warning.
- Ledger/Trezor mocked signing and cancellation.
- CIP-103 linear dependency chain, conflict flags, user decline, partial submission, and mixed rejection.
- Restart reconciliation and idempotent retry.

### Hardware Certification

- Ledger supported models with certified Cardano app v7/v8 combinations.
- Trezor Model T and supported Safe/Core firmware combinations.
- Trezor One base transaction support and explicit `signData` rejection.
- Every supported transaction field and every fail-closed unsupported field.
- Base and DRep CIP-8 payload forms and boundaries.
- Device rejection, disconnect, app-not-open, wrong app, transport failure, and cancellation.
- Body-hash equality and witness public-key/signature verification.

### Interoperability

- CIP-30 conformance test dApp.
- CIP-95 behavior against Lace/Yoroi/Cardano JS SDK expectations.
- CIP-103 invocation/rejection behavior against Eternl/Typhon/Lucid-compatible clients where available.
- CIP-104 encoding against a listed implementor before enablement; inability to prove it produces a tested disabled outcome rather than blocking unrelated capabilities.
- CIP-142 network magic on mainnet, preprod, preview, and a custom network fixture.

### Quality Commands

- `yarn compile`
- `yarn lint`
- `yarn prettier:check`
- `yarn test:jest`
- `yarn test:unit`
- `yarn test:e2e`
- `yarn storybook:build`
- `yarn i18n:manage`
- `yarn test:hardware-wallets`

## Security Review Gates

Release remains blocked until:

1. Existing privileged IPC is sender/frame authenticated.
2. Trusted main navigation is locked.
3. Guest is proven OS-sandboxed in packaged Linux `.deb` and `.rpm` output.
4. Initial and subsequent HTTPS/WSS destinations are connection-bound, and all unaudited bypass transports remain disabled.
5. Full transaction context and exact semantic review are available.
6. Unknown/unsupported transaction fields and mismatched body-to-witness/auxiliary commitments fail closed.
7. Software and hardware signers prove exact body-hash equality.
8. CIP-8 and witness outputs are locally verified.
9. Backend pending-submission crash windows and idempotent replay are fault-tested.
10. Logs, analytics, and grants pass privacy inspection.
11. Physical hardware compatibility matrix is complete.
12. Internal security review has no unresolved critical/high findings.
13. External audit has no unresolved critical/high findings.
14. Current Electron/Chromium release has no unaddressed critical security update.
15. The exact release-candidate source/dependency/backend/package/catalog baseline is recorded.
16. No material post-audit or pilot change remains outside affected retesting and security re-review.

## Rollout / Migration / Rollback

### Feature Flags

- Global dApp browser kill switch.
- Preferred catalog enable switch.
- Diagnostics arbitrary URL switch.
- CIP-104 proposed-extension switch.
- CIP-142 proposed-extension switch.
- Per-catalog-entry network policy.

All switches are main-owned launcher-configuration inputs implemented and tested before the audited release-candidate baseline. They are packaged and changed only through the normal reviewed release process; there is no remote runtime-policy service. Disabled launch modes reject new guests, and disabled proposed extensions are omitted from negotiation. The rollout manifest records the exact package/configuration variants used at each stage.

### Migrations

- Main-owned dApp grant and collateral-preference schema.
- cardano-wallet schema for any pending-submission additions.
- Hardware account/path capability metadata where required.

All migrations must be versioned, atomic, and fail closed. Wallet funds remain governed by ledger/cardano-wallet state; connector metadata corruption must disable the feature rather than mutate wallet state silently.

### Rollback

- Disable new guest launch through launcher config.
- Destroy any live guest and revoke in-memory capabilities.
- Preserve grants for a later fixed release unless the catalog entry was removed or the grant schema changed.
- Rely on cardano-wallet pending-submission state for submission reconciliation.
- Continue reconciling collateral preference against wallet state even while browser launch is disabled.
- Revert a backend pin only when its database/API migration is backward-compatible or a separate rollback migration exists.
- Do not fall back to unsandboxed guest launch, legacy IPC, incomplete transaction review, proxy submission, or reconstructed hardware bodies.

## Risks And Mitigations

| Risk | Mitigation |
|---|---|
| Remote content reaches existing privileged IPC | Authenticate every existing handler and keep guest on a separate scoped gateway. |
| Public dApp hostname resolves or rebinds to a private destination | Enforce IP-literal and connection-bound DNS destination policy for initial and subsequent HTTPS/WSS connections; disable Diagnostics launch if its initial destination cannot be proven. |
| Guest bypasses HTTPS/WSS policy through another Chromium transport | Disable WebRTC/data channels, STUN/TURN, WebTransport, QUIC, and non-proxied transports; any future compatibility exception requires evidence and equivalent enforcement. |
| Linux guest is only renderer-sandboxed, not OS-sandboxed | Ship `.deb`/`.rpm` with SUID/AppArmor/userns, remove global flags, prove packaged sandboxing, fail feature closed otherwise. |
| Portable `.bin` cannot privilege chrome-sandbox | Rejected: system packages only (research 06). |
| Approval summary differs from signed bytes | Main retains immutable bytes; shared exact parser; signer hash and result verification. |
| DApp changes outer `isValid` after signing | Show maximum collateral risk at signing; separately review exact submission envelope. |
| cardano-wallet UTxO loses datum/reference script | Query full ledger outputs at a coherent chain point. |
| Batch child spends unsubmitted parent | Backend derives and validates earlier outputs from exact parent bodies. |
| Conflicting batch items produce misleading totals | Show per-item effects and conflict flags; never aggregate conflicting items. |
| Hardware silently omits unsupported field | Complete semantic recognition plus fail-closed vendor mapping and hash equality. |
| CIP-95 public keys correlate governance identity | Elevated extension consent and separately revocable scopes. |
| CIP-104 xpub permanently exposes account history | Explicit high-risk consent, no logging/telemetry, and interoperability gate. |
| CIP-104 encoding is ambiguous | Do not advertise until a golden interoperability vector is locked. |
| Preferred collateral is spent by an ordinary send | Review warning, automatic re-evaluation, explicit preparation if needed. |
| Collateral return diverts wallet assets | Require wallet-owned return address and exact token/value validation. |
| Proxy submission loses pending locks | Use wallet-scoped submission and persist intent/pending context before broadcast. |
| Crash exposes partial batch witnesses | Stage only in memory and persist no signing witnesses. |
| App exit mid-batch leaves later items unattempted | cardano-wallet pending state reconciles; exact replays return existing hashes; dApps may retry. |
| Catalog entry changes origin/resources | Bundled catalog update invalidates that entry's grants. |
| Proposed extension changes upstream | Policy gate, conformance fixtures, and explicit status labeling. |
| Existing trusted renderer remains privileged | Lock its navigation and IPC; track full sandbox migration as separate architecture work. |

## Accepted Risks

- The existing trusted main renderer remains Node-enabled and not context-isolated.
- A Cardano VKey signature cannot bind outer `isValid`; a hostile dApp can reuse a released witness outside Daedalus.
- Chain state may change after review; Daedalus does not prompt again, and the node may reject the transaction at submission.
- Ledger and Trezor do not provide equal current-era transaction coverage; unsupported requests fail.
- CIP-104 and CIP-142 are Proposed and may change.
- An app exit during a batch submission may leave later items unattempted; recovery relies on cardano-wallet pending-submission state and idempotent retry.
- Preferred collateral is not guaranteed to remain available because the chosen policy permits ordinary last-resort spending.
- Platform, browser, backend, and device intrinsic limits may still reject requests that are within Daedalus's documented product limits.

## Evidence Gates And Open Questions

- Prove the Linux `.deb`/`.rpm` SUID and/or user-namespace plus AppArmor sandbox strategy across the supported distribution matrix; portable `.bin` is rejected.
- Prove the connection-level enforcement mechanism, built-in or custom, used to prevent private-network access and DNS rebinding for public HTTPS/WSS guest traffic.
- Determine whether `@cardano-sdk/core@0.41.4` fully decodes all target-era fields; upgrade only if fixture evidence requires it.
- Confirm exact raw-body and output-span handling for every accepted transaction encoding.
- Record upstream maintainer engagement for each new cardano-wallet API; upstream acceptance is the default path.
- Confirm volatile-ledger and local-pending snapshot consistency at one chain point.
- Resolve CIP-104 `Bip32PublicKey` encoding through upstream clarification and interoperability fixtures.
- Validate Ledger JS 8.x transaction and message behavior before changing the pin.
- Complete physical Ledger/Trezor firmware/model coverage.
- Confirm catalog resource allowlists for every preferred dApp.
- Confirm whether production diagnostics should allow public HTTPS IP literals; default policy should reject them until reviewed.
- Confirm the exact supported-era window, including Dijkstra-era readiness, before release claims.

## References

- Companion tasks: [dapp-browser-cip30-tasks.json](./dapp-browser-cip30-tasks.json)
- CIP-30: https://cips.cardano.org/cip/CIP-0030
- CIP-8: https://cips.cardano.org/cip/CIP-0008
- CIP-95: https://cips.cardano.org/cip/CIP-0095
- CIP-103: https://cips.cardano.org/cip/CIP-0103
- CIP-104: https://cips.cardano.org/cip/CIP-0104
- CIP-142: https://cips.cardano.org/cip/CIP-0142
- CIP-40: https://cips.cardano.org/cip/CIP-0040
- Electron security checklist: https://www.electronjs.org/docs/latest/tutorial/security

---

**Status:** Draft
**Date:** 2026-08-08
**Author:** OpenCode
