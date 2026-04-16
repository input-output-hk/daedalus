# Mithril Chain Storage Hardening Summary

## Purpose

This summary captures the durable outcomes of the chain-storage and Mithril fixes in one place. It is meant to stand on its own as future-work context without requiring any companion planning or review documents.

## Core Outcome

The fixes solved one class of problems with one consistent rule: chain storage must have a single source of truth, and directory changes must act as hard invalidation boundaries for Mithril startup state.

The resulting system now behaves like this:

- `stateDir/chain` is the authoritative chain-storage entry point.
- Default storage uses a local `stateDir/chain` directory.
- Custom storage uses a symlink or junction from `stateDir/chain` to `<parent>/chain`.
- Validation accepts a directly selected `chain` directory or alias that resolves to one, then canonicalizes it back to the owning parent.
- No chain-storage config file is consulted; runtime state is derived only from the live `stateDir/chain` entry point.
- Broken custom storage falls back to default storage instead of aborting startup.
- Directory changes cancel stale Mithril decision flows and cached startup assumptions.
- Returning to the picker can discard a newly created empty custom `chain` directory safely before reopening selection.
- If the new directory already contains chain data, the node starts immediately.
- Mithril status updates to the renderer are best-effort and do not block node startup.
- While a storage-location apply is in flight, the decision view remains visible but non-interactive and failed applies preserve the attempted path as a draft.

## Problems That Were Solved

### 1. Unsafe custom storage ownership

Previously, the selected custom directory could become the live chain root itself. That made wipe, cleanup, and snapshot-install flows capable of operating directly inside a user-selected directory instead of an app-managed child.

The fix established a managed-subdirectory model:

- users select a parent directory
- Daedalus manages only the `chain` child inside that parent
- Mithril work stays co-located with the managed chain directory
- destructive operations are scoped to the managed chain subtree

### 2. Split-brain storage state

Previously, live symlink or junction state and persisted config state could disagree. That made startup, Mithril, and IPC capable of interpreting the active storage location differently.

The fix removed that ambiguity:

- live link state is authoritative
- legacy config cleanup was removed once rollout tolerance was no longer needed
- recovery no longer depends on config-file interpretation

### 3. Broken custom storage causing startup failure

Previously, a broken symlink, missing drive, or stale junction could escalate into startup failure.

The fix changed recovery semantics:

- broken custom storage now degrades to default storage
- startup continues through the normal emptiness check afterward
- recovery is visible to the renderer, but it does not create a special Mithril mode

### 4. Ghost-node state after directory change

Previously, if the user changed chain storage to a directory that already contained chain data, the main process could start the node while the renderer still showed the Mithril decision screen.

That created two risks:

- stale Accept could try to bootstrap on top of a non-empty or already-running chain
- stale Decline could still wipe data after the runtime had already moved on

The fix made directory changes a full invalidation boundary:

- stale decision waiters are cancelled
- pending decision state is cleared
- Mithril status returns to `idle`
- cached startup state is discarded
- stale polling continuations are suppressed after async boundaries

### 5. Node not starting immediately after directory change

After the invalidation fix, the node could still remain stopped until the next disk-space polling interval even when the newly selected directory already had chain data.

The fix added immediate rerun behavior:

- directory change now triggers a serialized startup re-evaluation
- the rerun uses fresh-directory semantics instead of stale disk-space context
- overlapping triggers are coalesced instead of running in parallel
- the non-empty-chain path starts the node immediately

### 6. Node remaining stopped on initial app load

After the chain-storage and Mithril startup refactor, the node could still remain stopped on initial app launch even when the managed chain directory already contained data.

Two startup-flow changes mattered:

- the non-empty-chain startup path emitted Mithril status to the renderer before starting the node
- the refactored error path no longer preserved the prior fallback behavior of still attempting `cardanoNode.start()` for non-layout errors

That created a startup-only failure mode:

- if the renderer IPC send rejected or hung during initial load, the main process could stall before requesting node start
- if the new chain-storage emptiness or Mithril-decision path threw a non-layout error, the node could remain stopped instead of following the older fallback behavior

The fix restored the intended behavior:

- main-process startup state is updated first and renderer notification is fire-and-forget
- Mithril status IPC failures are treated as non-fatal for startup
- non-layout startup-flow errors still fall back to `cardanoNode.start()`
- only explicit managed-layout failures remain hard-stop errors

### 7. Storage picker edge cases around canonicalization and rollback

After the earlier hardening work, a few storage-selection edge cases could still leave runtime behavior and UI state out of sync.

Three cases mattered:

- selecting a directory already named `chain`, or a symlink or alias that resolved to `.../chain`, still represented managed storage ownership awkwardly even though the owning path should remain its parent
- returning from the Mithril decision view back to Change location could leave behind a newly created but still empty custom `chain` directory
- an in-flight or invalid storage apply could momentarily bounce the UI back into picker state without preserving the attempted location cleanly

The fix closed those gaps:

- direct `chain` selections and aliases that resolve to managed chain data are accepted and canonicalized back to the parent storage location
- reuse messaging only appears when the managed `chain` directory actually contains data
- returning to the picker first runs a coordinated cleanup step that can switch the live entry point back to default and remove an empty custom managed chain directory
- the renderer keeps a pending path separate from the committed path so failed applies reopen the picker with the attempted location still visible

## Final Behavior Model

### Storage model

- The only authoritative storage state is the live `stateDir/chain` entry point.
- Default storage is a normal directory.
- Custom storage is a symlink or junction to `<parent>/chain`.
- Validation may accept a directly selected `.../chain` directory or alias, but the committed custom storage model remains the parent directory that owns that managed child.
- Mithril staging, install, cleanup, and chain-emptiness checks all operate against the managed chain location.

### Startup model

- If chain data is present, startup skips Mithril and starts the node.
- If chain data is absent, startup proceeds through the existing Mithril decision flow.
- If a custom target is unavailable, startup falls back to default storage and then follows the same rules above.
- Renderer-facing Mithril status updates must not sit on the critical path for deciding whether cardano-node starts.
- Startup error handling should preserve the previous non-layout fallback semantics unless there is a deliberate reason to hard-fail.

### Directory-change model

- A successful directory change invalidates Mithril startup state, decision state, and cached layout assumptions.
- Old decision flows cannot complete against the new directory.
- Stale async continuations are suppressed by generation checks.
- The system re-evaluates startup immediately after the change instead of waiting for the next poll.
- Returning to the storage picker is allowed to run a preparatory cleanup step first, but only if that step actually changes the live managed storage location.

### UX model

- Recovery fallback is surfaced as a one-time session notice.
- Existing blockchain data is surfaced as a data-reuse notice.
- Existing-data messaging is shown only when the managed `chain` directory is non-empty, including for direct `chain` selections.
- Existing-data notice replaces generic help text in that state.
- The picker and decision summary display the effective managed `chain` path rather than only the selected parent.
- Storage-location apply uses its own transient UI state: the decision view stays mounted but disabled, and failed applies keep the attempted path visible as a draft.
- Recovery notice clears when the user chooses a new location or continues.

## Important Implementation Insights

### Single source of truth matters more than broad refactoring

The hardest failures came from disagreement about what the active chain directory was, not from the overall size of the startup code. Narrowing storage interpretation to the live chain entry point fixed more risk than a broad rewrite would have.

### Directory change is not a UI event, it is a state-boundary event

The important insight was that changing the chain location invalidates every in-flight Mithril decision and startup assumption tied to the old directory. Treating it as a first-class invalidation boundary closed the ghost-node and stale-wipe risks.

### Fresh-directory context must win over stale poll context

Once the user changes directories, the follow-up startup decision must use the new directory's state, even if an older poll-triggered run is still in flight. This was important enough to require explicit serialization and pending-args merge behavior.

### Recovery should degrade, not fork startup behavior

Broken custom storage did not need a special startup mode. Falling back to default storage and then reusing the ordinary startup decision tree kept behavior simpler and safer.

### Renderer messaging needed to follow runtime truth

The storage picker and Mithril bootstrap UI had to reflect actual runtime behavior: fallback to default storage and reuse of existing chain data. Without that, runtime correctness and user-visible state would drift apart again.

### Invalidation resets should not leak transient idle states to the renderer

The first-pass directory-change prompt flash exposed a narrower version of the same boundary rule: cancelling stale Mithril decision waiters is an internal invalidation step, not a user-visible state transition.

The durable lesson is:

- directory-change invalidation may need to clear pending decisions immediately
- that reset should not broadcast a transient `idle` if an immediate startup re-evaluation is about to emit the next authoritative state
- otherwise the renderer can unmount the storage picker mid-apply and force the user through the same prompt twice

### Storage-apply transitions need their own renderer state

Even after removing the transient idle broadcast, the renderer could still briefly repaint the storage picker while a directory-change IPC request was in flight, because the view only advanced after the apply completed and confirmation flipped.

The durable lesson is:

- treat storage-location apply as its own short-lived renderer transition state
- stop rendering the picker as soon as an apply begins
- keep the Mithril decision view visible but non-interactive until the storage update settles
- only expose snapshot selection and accept or decline actions once the directory change is actually committed

### Canonicalize direct chain selections without weakening ownership

The storage contract still depends on parent-directory ownership, but the UI and validation layer need to tolerate the paths users actually select, including `chain` directories and aliases that resolve to them.

The durable lesson is:

- treat direct `chain` selections as a canonicalization case, not as a separate ownership model
- resolve aliases and symlinks before deciding whether the selected path is really the managed chain directory
- only show data-reuse messaging if the managed `chain` directory is non-empty

### Renderer path display should reflect the managed chain directory

The storage model still treats custom selection as choosing a parent folder, but the UI is clearer when it shows the actual managed chain path that Daedalus will use.

The durable lesson is:

- keep the storage contract in terms of selecting a parent folder
- keep the explanatory copy about creating or reusing a chain subdirectory inside that parent
- display the effective managed chain path itself in the picker input and decision summary so the runtime path is always explicit

### Empty direct chain selections should stay neutral in the UI

When the user directly selects a folder already named `chain`, the UI should only surface a reuse message if that directory actually contains data.

The durable lesson is:

- a direct `chain` selection can still canonicalize to its parent for storage ownership
- an empty directly selected `chain` directory should not be labeled as existing blockchain data
- in that case the picker should show no creation or reuse helper text at all

### Returning to the picker should discard empty custom selections safely

If the user reaches the Mithril snapshot view after selecting a new custom location, Daedalus may already have created the managed `chain` child under that parent.

The durable lesson is:

- clicking Change location should discard that empty custom selection if the managed chain directory is still empty
- the cleanup must switch the live entry point back to default before removing the old empty custom `chain` directory
- the cleanup should sit behind a dedicated prepare step so the renderer can ask for rollback before reopening the picker
- the renderer can still preserve the old parent as a draft selection so the picker remains convenient without leaving an orphaned empty chain folder behind

### Failed applies should preserve draft user intent

The apply flow exposed a separate renderer-state rule: the location the user just attempted to use is not always the same as the currently committed storage location.

The durable lesson is:

- keep a pending storage path distinct from committed storage state while an apply is in flight
- if apply fails, reopen the picker with the attempted managed path still visible instead of snapping back to the last committed value
- keep confirmation and interactivity tied to committed state, not to the mere existence of a pending request

### Renderer IPC cannot be in the startup critical path

The initial-load startup regression exposed a separate rule from the broader storage work: the main process cannot wait on renderer readiness in order to start cardano-node.

The durable lesson is:

- update authoritative main-process Mithril state first
- attempt renderer notification on a best-effort basis
- never make node startup depend on whether the renderer can currently receive or process that notification

This is especially important during initial app load, where the renderer may still be mounting while startup logic is already evaluating chain state.

### Preserve old fallback semantics when refactoring startup

The regression also showed that startup refactors need an explicit check that previous non-fatal fallback semantics are still preserved.

In this case, the working behavior was to still attempt node startup after non-layout errors in the Mithril-decision path, while the refactored flow initially stopped doing so. Restoring that behavior fixed the user-visible regression without weakening the managed-layout hard-stop guard.

## What Future Work Should Preserve

- Keep `stateDir/chain` as the only authoritative entry point.
- Keep custom storage modeled as `<parent>/chain`, not the raw selected parent.
- Keep directory change as an atomic invalidation boundary.
- Keep stale decision cancellation as benign control flow, not as a user-visible error.
- Keep immediate reruns serialized so poll overlap cannot create double-start or stale-context bugs.
- Keep recovery inside the storage or layout layer rather than scattering custom recovery paths through startup callers.
- Keep direct `chain` selections and aliases canonicalized back to parent ownership instead of introducing a second storage model.
- Keep empty-selection rollback behind a coordinated prepare step before reopening the picker.
- Keep renderer IPC out of the startup critical path for node start.
- Keep non-layout startup fallbacks aligned with the previously working behavior unless a hard-failure policy is intentional.

## Remaining Debt

The fixes intentionally left a short list of follow-ups:

1. Add a startup-local error boundary around non-recoverable managed-layout failures instead of relying only on process-level handlers.
2. Remove deprecated rollout-tolerance metadata such as `setAt` once compatibility is no longer needed.
3. Decide whether the selfnode reset path should participate in the same coordinator invalidation contract.
4. Validate Windows junction behavior more directly outside mocked or unit-test coverage.
5. Restructure the large startup closure only if a future change truly requires it; correctness was fixed without broad redesign.

## Practical Guidance For The Next Change

If future work touches chain storage or Mithril startup, start from these assumptions:

- the main risk is stale state crossing a directory boundary
- the second main risk is accidentally reintroducing a second source of truth
- async overlap must be treated as normal, not exceptional
- any new destructive operation must be explicit about whether it unlinks, removes, or empties managed chain state
- any new renderer state around storage selection must reflect the real main-process decision flow
- cancellation paths must prove they only clean snapshot artifacts while a bootstrap run is actually active
- any validation change for storage selection must preserve canonical parent ownership even when the user chooses the managed `chain` path directly
- any rollback to the storage picker must decide explicitly whether an empty custom managed directory should be discarded first
- any renderer notification added to startup must be safe to drop, delay, or fail without changing node-start behavior
- any failed storage apply should preserve enough draft state for the user to understand what path just failed
- any startup refactor should preserve established fallback semantics, not just happy-path correctness

## Bottom Line

These fixes turned chain storage from a loosely coordinated set of path assumptions into a coherent model with one authoritative entry point, explicit recovery, and safe invalidation around directory changes.

---

## Review-Driven Hardening (2025-07)

The following changes were applied to address 15 review findings from the chain-storage and Mithril bootstrap hardening work. All 180 affected tests pass.

### Validation hardening

- **Nested managed-child rejection**: `validateChainStorageDirectory` now rejects any path nested inside the active managed chain root, including paths ending in `/chain` (direct-chain-selection descendants). Both `isDirectChainSelection` and non-direct cases check against the resolved default chain path.
- **Symlink alias detection**: Symlinks or aliases that resolve to the default `stateDir/chain` path are treated as reset-to-default rather than new custom selections. The check happens after the `isDirectory()` guard to avoid false positives on files.
- **Test coverage**: 5 new validation tests (nested path, deeply nested, chain-suffixed descendant, symlink alias, direct chain selection).

### Coordinator and IPC safety

- **Node-state guard**: `prepareForLocationChange`, `setDirectory`, and `wipeChainAndSnapshots` accept an optional `nodeState` parameter. If the node is not strictly `STOPPED`, the mutation throws before touching the filesystem. The `STOPPING` state is also rejected because `cardano-node` may still have the database open while shutting down. The guard uses a parameter-passing pattern rather than coupling the coordinator to IPC internals.
- **Channel forwarding**: `chainStorageChannel` passes `getMithrilBootstrapNodeState()` to both coordinator calls.
- **Idempotent channel setup**: Both `handleMithrilBootstrapRequests` and `handleChainStorageRequests` use a module-level flag to prevent double registration during window recreation.

### Mithril bootstrap resilience

- **Decoupled status listeners**: `broadcastMithrilBootstrapStatus` now fires internal listeners before attempting renderer send, and wraps the send in try/catch. A renderer crash or rejected IPC cannot prevent other listeners from receiving status updates.
- **Generation checks in handoff**: `startNodeAfterMithrilCompletion` captures `directoryChangeGeneration` at entry and checks it after each async boundary (status emit, node start, delay, state poll) to abort stale handoffs.
- **Full transient-state reset**: The `preparing` status now sends `snapshot: null`. The `cancelled` status now resets all transient fields (snapshot, error, file counts, ancillary counts, elapsed time, progress items).

### Selfnode safety

- **Validate before mutating**: `createSelfnodeConfig` now checks that the selfnode config file exists before calling `unlinkChainEntryPoint` or `resetToDefault`. Previously, the chain entry point could be destroyed even when the config file was missing.

### Rollback reliability

- **Broken symlink rollback**: `rollbackSetDirectory` falls back from `resolvedPath` to `linkTargetPath` for the symlink case. Uses `fs.remove` + `createSymlink` directly instead of `replaceCustomChainEntryPoint` (which internally calls `ensureDir`, failing on broken targets).

### Renderer resilience

- **Return-to-storage serialization**: `returnToStorageLocation` in `MithrilBootstrapStore` has a re-entry guard (`_returnToStorageInFlight`) and try/catch/finally. Concurrent clicks are suppressed, and failures safely reset storage state.
- **IPC rejection handling**: `handleContinue` in `ChainStorageLocationPicker` catches apply/reset IPC rejections and surfaces a generic validation error instead of letting the exception propagate.
- **Rejected path preservation**: When validation fails in `handleChooseDirectory`, the rejected path is stored in local candidate state so the user sees what they attempted. The display helper now bypasses managed-chain path formatting for invalid draft candidates, showing the exact path the user selected instead of appending `/chain`.

### Remaining known edges (documented, not addressed)

1. ~~Mithril IPC idempotent guard pins `sendStatusUpdate` to the first window; window recreation in the same process lifetime could leave status targeting stale webContents.~~ **Fixed**: `handleMithrilBootstrapRequests` now always rebinds `sendStatusUpdate` to the latest `BrowserWindow` while keeping IPC request listeners idempotent. Daedalus is single-window in normal operation but recreates the main window after renderer failure via `RendererErrorHandler`, so `sendStatusUpdate` must always target the current live `webContents`.
2. Status listeners in `broadcastMithrilBootstrapStatus` are not individually isolated — one bad listener can abort subsequent listeners.
3. Fire-and-forget status emitters in `startNodeAfterMithrilCompletion` can still race with directory changes between the generation check and the actual emit. The critical `start()` call is protected.

That is the durable takeaway for future work: do not reopen the source-of-truth question, do not weaken the directory-change invalidation boundary, and do not let startup decisions continue on stale chain-location state.