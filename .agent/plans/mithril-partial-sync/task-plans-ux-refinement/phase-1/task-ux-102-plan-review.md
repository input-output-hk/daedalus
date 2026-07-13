# task-ux-102 — Plan Review Log (append-only)

Planner: 2026-06-22T10:45:01Z
Drafted the canonical plan for task-ux-102 (compute the certified-immutable-gap behind-ness signal,
PRD D2). Resolved the DESIGN DECISION: Option A — compute behind-ness in MithrilPartialSyncService
(it owns resolveLatestSnapshotMetadata + the aggregator helpers + a disk-backed _chainStorageManager)
and merge in MithrilController.getPartialSyncAvailability(), which already owns both the coordinator
(isEnabled) and _partialSyncService. This resolves the task-ux-101 open question: the availability read
becomes async end-to-end; the IPC handler already awaits, so the channel/type/renderer wrapper from 101
stay frozen. Rejected Option B (inject a provider into the coordinator) as pure plumbing. Verified two
load-bearing seams against live code: (1) chainStorageManager.getManagedChainPath() is lock-free (not
wrapped in _withMutationLock); (2) getConfig() -> getChainStorageConfig(this) is disk-backed/stateless,
so the service's separately-constructed ChainStorageManager still resolves the authoritative managed
chain path (incl. relocated chains). Gap computed by DIRECT subtraction (latest - local) with the same
definition as derivePartialSyncRange but non-throwing; no-certified-range / local>=latest / any
aggregator-or-FS failure all degrade to { isSignificantlyBehind: false }. Aggregator query cached with a
5-min TTL (Date.now clock); local FS read is uncached so the local position is always fresh. Threshold
is backend-owned: DEFAULT constant + optional launcherConfig.mithrilPartialSyncThresholdImmutables
override (config.ts + nix).
KEY OPEN ITEM for the Critiquer: PIN the default threshold. Doc uses a placeholder of 30 immutable files
and explicitly rejects the originally-proposed 2160 (that is the security param k, not the immutable-file
count per epoch, and would likely never fire in QA). The Critiquer must verify real mainnet
immutable-file cadence and pin a conservative-yet-QA-firing value, used in both the constant and nix.
Note: authored/persisted by the orchestrator from the read-only Plan agent's Option A design plus
independently-verified live-code anchors.
Outcome: plan drafted, awaiting critique

---

Critiquer: 2026-06-22T10:48:20Z
Threshold default: PINNED to 20 immutable files — one Cardano mainnet immutable chunk covers 10*k =
10*2160 = 21,600 slots ≈ 6h (1 slot = 1s), so an epoch (432,000 slots = 5 days) = 432,000/21,600 = 20
immutable files; 20 is exactly "≈ ≥ 1 epoch-equivalent" per PRD D2, conservative against over-prompting,
and reliably fires for a node several days behind (cadence ≈ 4 files/day). ASSUMPTION (repo is silent —
no chunk-size/epoch constant in source or nix; only the runtime-fetched epochLength/slotLength in
NetworkStatusStore and the beacon immutable_file_number): chunk = 10k slots, mainnet k=2160. Use 20 in
BOTH Step 1 (DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES) and Step 9 (nix). It is QA-calibratable downward
(e.g. to ~4 = ~1 day) and is not load-bearing for correctness. The planner was right to reject 2160:
that is k, and as an immutable-FILE count it is ~2160*6h ≈ 540 days behind, so the prompt would never
fire.

Load-bearing seam claims — both VERIFIED TRUE against live code:
- getManagedChainPath() lock-free: chainStorageManager.ts:194-197 calls getConfig() + _getManagedChainPath(),
  neither wrapped in _withMutationLock (contrast setDirectory/ensureManagedChainLayout/installSnapshot at
  :90,:168,:277). The probe never serializes behind an in-flight sync. TRUE.
- getConfig() disk-backed/stateless: chainStorageManagerConfig.ts:32-72 derives customPath purely from the
  on-disk symlink target (_captureChainPathState -> fs.lstat/realpath at chainStorageManagerShared.ts:172-213)
  + disk-space; the only instance state read is _isRecoveryFallback (false on a fresh instance; affects only
  an optional flag, not the resolved path) and _stateDirectoryPath/_chainPath (derived deterministically
  from the shared default `stateDirectoryPath` in the ctor at :77-85). A fresh new ChainStorageManager()
  resolves the SAME authoritative managed chain path incl. relocated/symlinked chains. TRUE. And the nix
  passthrough is generic: readLauncherConfig (utils/config.ts:57+) returns the whole parsed YAML as
  LauncherConfig (no field whitelist), so mithrilPartialSyncThresholdImmutables flows through exactly like
  mithrilPartialSyncEnabled. TRUE.

Verified, no defect:
- PRD D2 gap formula: direct gap = latest - local matches derivePartialSyncRange length (start=local+1,
  end=latest => end-start+1 = latest-local) WITHOUT its PARTIAL_SYNC_NO_CERTIFIED_RANGE throw. gap<=0 and
  every catch => { isSignificantlyBehind:false } (truthfulness). Correct.
- _createStageError signature (:819-825) is (stage, message, code?); resolveLocalImmutableNumber's factory
  (preflight.ts:11-15,63-65) is the same (stage, message, code?). The plan's lambda
  (stage,message,code)=>this._createStageError(stage,message,code) is exactly right.
- Async ripple: the ONLY production callers of getPartialSyncAvailability() are the channel handler
  (mithrilPartialSyncChannel.ts:116-117, already `async () => controller....`) and the controller->coordinator
  hop. No other production caller. The 3 task-ux-101 availability specs (mithrilPartialSyncChannel.spec.ts:174-228)
  already use `await ...resolves.toEqual(...)`; the mock returns a plain value that `async () =>` wraps in a
  resolved Promise either way — they survive the async change unchanged (the plan's "make the mock async only
  if needed" note is correct; it is NOT needed).
- ResolvedLatestSnapshot.latestCertifiedImmutableNumber:number (mithrilSnapshotMetadata.ts:3-6) and
  resolveLatestSnapshotMetadata (:571) match the cache wrapper. Cache TTL (5min) caches only the aggregator
  query; local FS read uncached => gap tracks local progress (no tight poll). Correct + low-frequency.

[MINOR] Test implementability — getManagedChainPath() stubbing is unspecified. In MithrilPartialSyncService.spec.ts
the fs-extra mock (:11-22) does NOT include lstat/realpath/readlink, and the service builds a REAL
ChainStorageManager(). If a new behind-ness test calls the real getPartialSyncBehindness() without stubbing,
getManagedChainPath() -> getConfig() -> _captureChainPathState() hits unmocked fs.lstat: safeLstat
(chainStorageManagerShared.ts:400-412) re-throws non-ENOENT errors (a TypeError here), getConfig's catch
(:35-49) then falls back to _getDefaultStorageConfig() which calls the UNMOCKED real checkDiskSpace on
/tmp/daedalus-state => non-deterministic/possibly-flaky path that resolves to /tmp/daedalus-state/chain,
NOT the fixture's /tmp/chain. FIX (add to Step 6/test guidance): each new test must
`jest.spyOn(service._chainStorageManager, 'getManagedChainPath').mockResolvedValue('/tmp/chain')` and stub
`resolveLatestSnapshotMetadata` (resolve/reject) + the fs.stat/fs.readdir local-immutable read (already
mocked), rather than exercising the real path resolver. Deterministic and matches the existing spec's
spy-on-the-service pattern (:122-125). This is a guidance gap, not a design defect.

[MINOR] Step-5 test #2 shape consistency. The plan's Step 5 sets behindByImmutables for ANY gap>0
(including gap<threshold), but testCase #2's parenthetical hedges "or is omitted". Pin it: per Step 5,
gap>0 ALWAYS sets behindByImmutables, so test #2 must assert { isSignificantlyBehind:false,
behindByImmutables: <smallGap> } (not omitted). Keep the single shape to avoid an ambiguous assertion.

Decision: approved

Planner: 2026-06-22T10:50:14Z
Fix pass after critique (Decision: approved). (1) Pinned DEFAULT_PARTIAL_SYNC_THRESHOLD_IMMUTABLES = 20 (Step 1) and the nix field (Step 9), per the cadence reasoning (10*k = 21,600 slots ~= 6h/file => ~20 files/epoch). (2) Added required test-setup note: spy/mock `service._chainStorageManager.getManagedChainPath` and stub the local immutable read (the fs-extra mock lacks lstat/realpath). (3) Fixed Test #2 shape: gap>0 always sets behindByImmutables, so assert it is set (small gap), not omitted. (4) Updated the Risks PIN item to RESOLVED. No structural changes; both load-bearing seam claims (lock-free getManagedChainPath; disk-backed stateless getConfig) were verified TRUE by the Critiquer.
Outcome: plan approved, proceeding to implementation
