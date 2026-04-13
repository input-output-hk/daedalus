# Mithril Chain Storage Hardening Summary

## Purpose

This summary captures the durable outcomes of the chain-storage and Mithril fixes in one place. It is meant to stand on its own as future-work context without requiring any companion planning or review documents.

## Core Outcome

The fixes solved one class of problems with one consistent rule: chain storage must have a single source of truth, and directory changes must act as hard invalidation boundaries for Mithril startup state.

The resulting system now behaves like this:

- `stateDir/chain` is the authoritative chain-storage entry point.
- Default storage uses a local `stateDir/chain` directory.
- Custom storage uses a symlink or junction from `stateDir/chain` to `<parent>/chain`.
- The old config-file-based chain-storage state is no longer authoritative.
- Broken custom storage falls back to default storage instead of aborting startup.
- Directory changes cancel stale Mithril decision flows and cached startup assumptions.
- If the new directory already contains chain data, the node starts immediately.

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
- legacy config data is tolerated only for cleanup or rollout compatibility
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

## Final Behavior Model

### Storage model

- The only authoritative storage state is the live `stateDir/chain` entry point.
- Default storage is a normal directory.
- Custom storage is a symlink or junction to `<parent>/chain`.
- Mithril staging, install, cleanup, and chain-emptiness checks all operate against the managed chain location.

### Startup model

- If chain data is present, startup skips Mithril and starts the node.
- If chain data is absent, startup proceeds through the existing Mithril decision flow.
- If a custom target is unavailable, startup falls back to default storage and then follows the same rules above.

### Directory-change model

- A successful directory change invalidates Mithril startup state, decision state, and cached layout assumptions.
- Old decision flows cannot complete against the new directory.
- Stale async continuations are suppressed by generation checks.
- The system re-evaluates startup immediately after the change instead of waiting for the next poll.

### UX model

- Recovery fallback is surfaced as a one-time session notice.
- Existing blockchain data is surfaced as a data-reuse notice.
- Existing-data notice replaces generic help text in that state.
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

## What Future Work Should Preserve

- Keep `stateDir/chain` as the only authoritative entry point.
- Keep custom storage modeled as `<parent>/chain`, not the raw selected parent.
- Keep directory change as an atomic invalidation boundary.
- Keep stale decision cancellation as benign control flow, not as a user-visible error.
- Keep immediate reruns serialized so poll overlap cannot create double-start or stale-context bugs.
- Keep recovery inside the storage or layout layer rather than scattering custom recovery paths through startup callers.

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

## Bottom Line

These fixes turned chain storage from a loosely coordinated set of path assumptions into a coherent model with one authoritative entry point, explicit recovery, and safe invalidation around directory changes.

That is the durable takeaway for future work: do not reopen the source-of-truth question, do not weaken the directory-change invalidation boundary, and do not let startup decisions continue on stale chain-location state.