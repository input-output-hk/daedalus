# Hostile Renderer Threat Model Traceability

Status: supporting evidence for task-001. Normative decisions remain in the
[PRD threat model and ADR](../dapp-browser-cip30-prd.md#hostile-renderer-threat-model-and-architecture-adr).

## Live Baseline Evidence

| Surface | Verified finding | Consequence |
|---|---|---|
| `source/main/windows/main.ts` | The main renderer enables Node integration, disables context isolation, and loads `preload.js`. | It is privileged legacy UI and cannot host remote content. |
| `source/main/preload.ts` | The preload exposes raw `ipcRenderer`, HTTP(S), environment/configuration values, paths, OS data, and logging globals. | It must never be reused by a dApp guest. |
| `source/common/ipc/lib/IpcChannel.ts` and `IpcConversation.ts` | Handlers receive decoded messages, not sufficient Electron sender/frame identity; `IpcChannel` uses a shared response channel and attaches its listener after sending. | Legacy IPC is unsuitable for a hostile guest and requires task-101/102 hardening. |
| Task-101 Electron 41 runtime evidence | Packaged `file:` main frames report `WebFrameMain.origin === "file://"`; development HTTP frames report their serialized HTTP origin. | Trusted authority binds exact WebContents/frame/canonical URL and uses `file://` only for the exact packaged file document. Wrapper-backed channels are hardened; task-102 still owns raw-listener migration. |
| `source/main/index.ts` | The global popup handler is registered after `createMainWindow()` creates the window and initiates `loadURL`, following startup work; it forwards requested URLs to `shell.openExternal`. The source does not prove page-load completion before registration. | Task-100 must install safe trusted navigation/popup policy before any WebContents. |
| `source/main/ipc/open-external-url.ts` | Renderer input is passed to `shell.openExternal` without parsed HTTPS policy. | Task-100 must validate and await external URL handling. |
| `source/main/webpack.config.js` and `nix/internal/x86_64-linux.nix` | Development and packaged Linux launch paths include `--disable-setuid-sandbox --no-sandbox`. Historical portable `.bin` cannot privilege `chrome-sandbox`. | Product decision: ship `.deb`/`.rpm` only (research 06). Historical task-005 preserves the cancelled portable spike; task-005-a freezes the contract, tasks 108/109 build flag-free packages, task-005-b certifies them, and task-103 removes remaining bypasses and enables fail-closed runtime checks. |
| `source/main/dapp/` and guest preload paths | No dApp guest, nonpersistent session, route lease, or main-process broker exists. | These are accepted targets, not present safeguards. |

## Threat To Evidence Owner

| Threat/control class | Phase-0 evidence owner | Follow-through |
|---|---|---|
| Full ledger context, backend consistency, pending submission | task-003 | Phase 2 backend work and Phase 3 context/reconciliation. |
| Exact body/output preservation and supported semantic decoding | task-004 | Phase 3 cursor, semantic model, and witness verification. |
| Packaged Chromium OS sandbox (Linux `.deb`/`.rpm`) | task-005-a (contract/matrix), research 06 (decision); task-005 is cancelled portable evidence | Phase 1 tasks 108/109 (flag-free packages), task-005-b (installed-artifact certification), task-110 (migration), task-103 (remaining flags/runtime canary), then packaged hostile-renderer proof. |
| Hardware fields, CIP-8, returned hashes, models, and firmware | task-006 | Phase 6 adapters and Phase 8 physical certification. |
| Trusted navigation and privileged IPC authority | task-001 model | Phase 1 tasks 100-102. |
| Guest lifecycle, permissions, transport, and connection-bound egress | task-001 model | Phase 1 tasks 104-107, especially 106-a. |
| Consent, immutable bytes, complete semantic review, and submission continuation | task-001 model | Phase 3 parser/model, Phase 4 approval coordination, Phase 5 software signing/submission, and Phases 6-7 hardware and batch flows. |
| Privacy, audit, release baseline, and rollout change control | task-001 model | Phases 8 and 9. |

## Durable Findings

- The existing renderer and IPC surfaces are not a safe incremental guest host;
  isolation and a new main-owned gateway are required rather than wrappers
  around legacy guest access.
- Connection-time destination enforcement is a security boundary. A hostname or
  DNS preflight approval is insufficient when the resulting connection can
  rebind; unsupported initial Diagnostics enforcement keeps that launch mode
  disabled.
- Exact transaction body preservation alone is insufficient. The review and
  submission approvals bind the complete immutable material described in the
  ADR, while later tasks provide the parser and backend evidence.
- No runtime changes or test fixtures were added by task-001. Packaged sandbox,
  hardware, external audit, and configured-network evidence remain explicitly
  unperformed downstream gates.
