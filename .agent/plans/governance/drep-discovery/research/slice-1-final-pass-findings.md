# Slice-1 Final Pass — Research & Findings

> **Status:** active reference | **Date:** 2026-06-15 | **Source:** slice-1 final-pass sprint (FP-1…FP-11)
> **Parent:** [slice-1-final-pass-PRD.md](../task-plans/slice-1-final-pass-PRD.md)

Technical findings, gotchas, and verified facts encountered while implementing the slice-1 final pass. Captured for future governance slices so the same ground is not re-walked.

---

## 1. cardano-cli network flag is a PER-SUBCOMMAND option (FP-1 — verified on a real binary)

**Finding:** The `(--mainnet | --testnet-magic NATURAL)` network selector is **not** a top-level `cardano-cli` flag. It is an option of the leaf query subcommand (`query tip`, `query drep-state`, …). It must therefore appear **after** the subcommand, never before the era token.

**Verified** against the bundled binary `~/.daedalus/preview/libexec/bundle-daedalus-bridge/bin/cardano-cli` (parse-only, no node):

| argv | result |
|---|---|
| `cardano-cli --testnet-magic 1 latest query tip --output-json` (flag **prepended**) | ❌ `Invalid option '--testnet-magic'` |
| `cardano-cli latest query tip --output-json --testnet-magic 1` (flag **appended**) | ✅ passes network parser → `Missing: --socket-path` (supplied at runtime via env) |
| `cardano-cli latest query tip --output-json --mainnet` | ✅ passes network parser → `Missing: --socket-path` |

**Consequence in code:** `GovernanceQueryService._runCliQuery` **appends** the network-flag tokens after the incoming args (`[...args, ...flagTokens]`), so the final argv is e.g. `['latest','query','drep-state','--all-dreps','--include-stake','--output-json','--testnet-magic','1']`.

**Trap for future maintainers:** unit tests mock `child_process.spawn` and assert the argv *shape*; they pass regardless of flag ordering and **cannot** catch this class of grammar error. The original FP-1 implementation prepended the flag (per the sprint YAML step which was itself wrong) and passed 26/26 mocked tests while being broken on a real node. Only the review pass — which ran the real binary — caught it. **Recommendation:** any future change to the CLI argv composition must be re-checked against the real binary with a parse-only invocation (`--socket-path` left unset is sufficient: you'll see the network parser pass and then complain about the missing socket).

**`--socket-path` vs env:** the service supplies the socket via the `CARDANO_NODE_SOCKET_PATH` environment variable (never as a user-controllable argv flag). `cardano-cli` reads that env var as the fallback for `--socket-path`, so the runtime invocation does not pass `--socket-path` explicitly.

---

## 2. Bundled cardano-cli version is 10.15.0.0 locally — flake pins 11.0.1 (affects FP-11)

**Finding:** The locally-installed Daedalus dev bundle ships **`cardano-cli 10.15.0.0`** (`linux-x86_64 - ghc-9.6`), while the flake / release configuration (per PRD E1) pins **`cardano-node`/`cardano-cli` 11.0.1**.

**Why it matters:** PRD E1 / FP-11 reason about the **node 11.0.1 LSM-tree (LedgerDB V2)** UTxO-HD backend. That assessment is keyed to 11.0.1. The binary used for local dev verification here was 10.15.0.0. The network-flag placement and the `latest`/`conway` era-fallback were validated against **10.15.0.0** (and the CLI grammar for the network selector is consistent across 10.x/11.x).

**Open item for FP-11 (manual):** confirm the **actual** `cardano-cli`/`cardano-node` version in the environment used for the FP-8 smoke test (`yarn nix:preprod` shell ≈ flake-pinned 11.0.1; the locally-installed app ≈ 10.15.0.0). Record the version and the active UTxO-HD backend alongside the `drep-state` capture. The "in-memory snapshot" framing in the plan is exact only for the InMemory backend.

---

## 3. IpcChannel error transport: plain objects survive structured clone (FP-2 — verified)

**Finding:** `source/common/ipc/lib/IpcChannel.ts` `onRequest` forwards the **raw thrown value** to the renderer:

```
catch (error) { event.sender.send(this._responseChannel, false, error); }   // line ~183
```

and the renderer-side `request()` rejects its promise with the structured-cloned value (`reject(response)`, line ~140). It does **not** re-wrap the error.

**Consequence:** a thrown **plain object** (e.g. `{ __governanceError: true, type, message, details }`) survives Electron structured clone with **all properties intact**, whereas a thrown `Error` instance is flattened to `{ name, message }`. The old transport relied on JSON-stringifying the structured error into `Error.message` and re-parsing it in `_normalizeError` — functional but fragile.

**Resolution (FP-2):** `governanceChannel.ts` now throws the marked plain object; `GovernanceStore._normalizeError` checks `__governanceError === true` first, then falls back to the JSON-in-message path (kept for backward-compat), the direct-`queryErrorType` path, and finally the `Error`-instance path. The acceptance-critical half of FP-2 was the **UI**: `DRepDirectory.tsx` previously rendered only `error.message` and never `error.details` (the actionable CLI stderr) — now both render in the Failed state and the error banner.

**Test-harness caveat:** `GovernanceStore.spec.ts` mocks `governanceDRepListChannel.request()`, so it never exercises the real structured clone. The marker path is unit-tested by feeding `_normalizeError` the plain object directly; production correctness rests on the verified transport behaviour above, not on the mocked test.

---

## 4. Test runner: use the jest binary directly — `npx`/`yarn` jest are broken under node 24

**Finding:** `npx jest`, `yarn jest`, and `yarn test:jest` all fail in this environment with:

```
npm error Invalid property "devEngines.node"
```

This is an npm-version incompatibility with `package.json`'s `devEngines.node` field under node 24 (`node --version` = v24.15.0; `devEngines` declares `node >=v22.0.0`). It is **not** a test failure.

**Workaround (use everywhere):**
```
node_modules/.bin/jest --testPathPattern="<pattern>" --no-coverage
node_modules/.bin/tsc --noEmit
```

**Why it matters for agents/automation:** a subagent that runs `yarn test:jest`, sees this error, and concludes "the tests are broken" will be wrong. Every implementation/review prompt in the sprint had this quirk injected explicitly. The clean `tsc --noEmit` baseline (exit 0) means any *new* TS error is attributable to the change under review.

---

## 5. conway-era retry heuristic keys off the substring "latest" (FP-1 — fragility note)

**Finding:** `GovernanceQueryService._shouldRetryWithConway` decides whether to retry a failed `latest`-era query with the `conway` era flag by string-matching the failure text for `latest` plus an era-ish keyword. Any new `GovernanceQueryError(QueryFailed)` message that happens to contain the word **"latest"** would trigger a spurious conway retry.

**Mitigation in place:** the FP-1 "network not set" error message was intentionally worded to avoid the word "latest". **Future error strings on the QueryFailed path should avoid "latest"** unless a conway retry is genuinely desired. A more robust long-term fix would be to gate the retry on a structured marker rather than substring matching (candidate cleanup for a later slice).

---

## 6. NetworkMagics does not cover `mainnet_flight` / `development`

**Finding:** `source/common/types/cardano-node.types.ts` `NetworkMagics` has no entry for `mainnet_flight` or `development`. `GovernanceQueryService.setNetwork` handles both with explicit string checks (`mainnet_flight → --mainnet`, `development → --testnet-magic 42`). `NetworkMagics.mainnet[0]` is `1`, which is a placeholder, **not** a testnet magic — mainnet is correctly routed to `--mainnet` via the explicit check and never reads that index.

**Consequence:** adding a new cluster requires either a `NetworkMagics` entry or a new special-case in `setNetwork`. Unknown clusters resolve to `networkFlag = null`, and `_runCliQuery` then rejects with a clear "network is not set" `QueryFailed` before spawning.

---

## 7. FP-11 — cardano-node 11 / LSM-tree backend assessment (UNVERIFIED on live node)

Consolidated from PRD E1; **carried forward as a manual verification item** (see FP-11). Assessment (from release notes / UTxO-HD docs), to be confirmed on the live preprod node:

- UTxO-HD moves only the **UTxO set** to disk; `drep-state` and the DRep **stake distribution** are ledger-state components served from in-memory ledger tables, so `drep-state --all-dreps --include-stake` is **expected** to be unaffected by the OnDisk/LSM backend.
- Confirm which UTxO-HD backend the bundled node runs — **InMemory** (~24 GB RAM) vs **OnDisk/LSM** (~8 GB RAM). The plan's "in-memory snapshot" framing is exact only for InMemory.
- If OnDisk/LSM is active, confirm the Linux runtime deps (`liburing`, `snappy-c`, `protobuf-compiler`) are present in the Nix closure.
- Known LSM bug: node cannot read blocks with **>4096 items** (blockio-uring, fix pending) — a sync-stall risk that would pin the directory in the soft-sync-banner state.
- **Version caveat:** see §2 — verify the actual bundled version first; this assessment assumes 11.0.1.

**Sources:** [cardano-node 11.0.1 release](https://github.com/IntersectMBO/cardano-node/releases/tag/11.0.1) · [UTxO-HD overview](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview/)

---

## 8. FP-3 sidebar push — the YAML root_cause was overstated (verified)

**Finding:** The sprint YAML claimed the app-level sidebar handler "pushes `ROUTES.GOVERNANCE` on every click without checking the current route", implying a second code fix was needed. **That is incorrect.** `AppStore.currentRoute` (AppStore.ts:69-73) is a `@computed` returning the live `router.location.pathname`; `goToRoute` is bound to `_updateRouteLocation`, which pushes only when `this.currentRoute !== newRoutePath` (AppStore.ts:216). Because `currentRoute` *is* the live pathname, the same-path sidebar push is already unreachable. Only the **sub-tab** path in `Governance.tsx` needed the guard. The directory-tab duplicate-push warning is therefore fully resolved by the single `Governance.tsx` guard. No `SidebarStore`/`AppStore` change was made or required.
