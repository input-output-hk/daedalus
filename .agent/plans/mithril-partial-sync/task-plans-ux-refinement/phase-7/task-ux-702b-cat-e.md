# task-ux-702b-cat-e — Availability poll: bound the request (anti-pin) + known-stable back-off; keep the idle poll

> Per-category implementation doc, decomposed from the canonical plan `task-ux-702b.md` (task-ux-702b =
> 702a code-review remediation). **Self-contained — implementable from this doc alone.**
> Parent task: `task-ux-702b`. Decisions: `task-ux-702b-decisions.md`. If this doc ever disagrees with
> live code, prefer live code and reconcile here.

## Sequencing position

CAT-E is the **fifth** category in the collision-safe order and the **last of the four
`MithrilPartialSyncStore.ts` editors** (the store-file editors serialize **B → A → D → E**). It lands
**after CAT-D** (which rewrites `dismissCompletedOverlay` in the same store) and **before CAT-H** (the
backend per-probe-cost fix). CAT-E rewrites `_refreshAvailability` (adds the request timeout) and adds
the known-stable back-off; it touches `MithrilPartialSyncStore.ts` + `MithrilPartialSyncStore.spec.ts`
only — renderer-only, no overlap with CAT-C/CAT-F/CAT-G/CAT-H files. CAT-H is placed right after CAT-E so
the two poll-cost fixes (CAT-E = poll **frequency** + anti-pin; CAT-H = per-probe **cost**) stay adjacent
and reviewable together.

> **Full collision-safe order:** `CAT-B → CAT-A → CAT-C → CAT-D → CAT-E → CAT-H → CAT-F → CAT-G`, each followed by `yarn compile` + the touched specs + a per-category code-review pass. Store-file editors (`MithrilPartialSyncStore.ts`) serialize **B → A → D → E**. Overlay-file editors (`MithrilPartialSyncOverlay.tsx`) serialize **C → D**; the overlay spec is also touched by **G** (run order C → D → G, each its own test case). **CAT-B before CAT-A** — the container near-tip gate consumes CAT-B's `computeBehindByEpochs`. **CAT-H is backend-only** (collision-free), placed after CAT-E. **CAT-F is standalone.** **#16 producer (CAT-H) / consumers (CAT-B, CAT-A) are order-independent by safe degradation** — an absent/undefined `certifiedEpoch` degrades the hybrid to networkTip-only (= today, no regression).

## Findings closed & decisions implemented

| Finding # | Short desc | Decision | Severity |
|---|---|---|---|
| #6 | Availability re-entrancy pin: `_isRefreshingAvailability` clears only in `finally` and `channel.request()` has no timeout → a wedged main process pins the guard `true` forever | D-702b-6 | High |
| #9 | The 30s availability poll lost its `isWorking` gate (correct) but now fires every 30s for the whole session even after the node is fully synced | D-702b-6 | Cleanup |

## Locked invariants this change must NOT break

> - **#4 Backend owns the OFFER** (`isSignificantlyBehind`, immutable gap). CAT-E/CAT-H must not change the offer verdict for any given (latest, local) pair; CAT-H only deduplicates the I/O to obtain `local` and adds the optional `certifiedEpoch`.
> - **No ISSUE-1 regression.** A stale-low cached local read makes `gap = latest − local` LARGER ⇒ reads as MORE behind (conservative), never a premature not-behind. CAT-E's back-off must be gated behind ≥2 consecutive stable reads so it cannot engage on a premature first not-behind read.

Additional CAT-E-specific locks:
- **Do NOT restore the `isWorking` guard on the availability poll.** Its removal was intentional — the
  proactive prompt (`status==='idle'` gate) and the Diagnostics dialog are **idle-time** consumers of
  `isSignificantlyBehind` that an `isWorking`-only poll could never serve.
- **SLOW, never STOP** the poll. A stopped poll would strand those idle consumers.
- **Renderer-only.** CAT-E addresses **how often** the poll fires (cadence back-off) + the re-entrancy
  pin. It does **NOT** reduce the **per-probe backend cost** — that is CAT-H / #15 / D-702b-9. CAT-E's
  back-off predicate (`!isPartialSyncEnabled || !isSignificantlyBehind`) is **gated off while the node is
  behind**, so during the catch-up window the poll stays at 30s and pays the full backend cost every tick;
  do not re-scope CAT-E to the backend.

## Exact files (full repo-relative paths)

- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — adds the request timeout in
  `_refreshAvailability`, the back-off consts/counter/helpers, and the `_armAvailabilityInterval` rearm.
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` — new anti-pin + back-off cases.

## Implementation steps (ordered, mechanical)

### 1. Bound `request()` (#6) — anti-pin timeout

Add `const AVAILABILITY_REQUEST_TIMEOUT_MS = 10_000;` near `AVAILABILITY_REFRESH_INTERVAL` (`:36`,
currently `AVAILABILITY_REFRESH_INTERVAL = 30_000`). In `_refreshAvailability` (`:200-216`), wrap the
`mithrilPartialSyncAvailabilityChannel.request()` (`:207`) in a timeout so a wedged main process can never
keep `_isRefreshingAvailability` pinned `true`. Use a local helper that races the request against a
timeout-reject **and clears the timer** to avoid a dangling timeout:

```ts
let timeoutId: ReturnType<typeof setTimeout> | undefined;
const withTimeout = new Promise<never>((_, reject) => {
  timeoutId = setTimeout(
    () => reject(new Error('availability request timed out')),
    AVAILABILITY_REQUEST_TIMEOUT_MS
  );
});
try {
  const availability = await Promise.race([
    mithrilPartialSyncAvailabilityChannel.request(),
    withTimeout,
  ]);
  this._applyAvailability(availability);
} catch (error) { /* existing logger.warn */ }
finally {
  if (timeoutId) clearTimeout(timeoutId);
  this._isRefreshingAvailability = false;
}
```

(A generation-token reset is the sanctioned alternative; the `Promise.race` timeout is recommended —
simpler and fake-timer testable. Keep it local to the store. The guard `_isRefreshingAvailability` today
clears only in `finally` `:213-215` and `request()` `:207` has no timeout — that is the pin being fixed.)

### 2. Known-stable back-off (#9) — gated so it CANNOT engage on the premature first read (HIGH-1)

Add `const AVAILABILITY_REFRESH_BACKOFF_INTERVAL = 300_000;` (5 min) and
`const STABLE_READS_BEFORE_BACKOFF = 2;`, plus a private counter `_consecutiveStableReads = 0`. The
stability predicate is `_isAvailabilityStable() => !this.isPartialSyncEnabled ||
!this.isSignificantlyBehind` (disabled ⇒ never available; enabled-and-not-behind ⇒ caught up), **but a
single stable read MUST NOT slow the poll.**

**Why (ISSUE-1, documented verbatim at `MithrilPartialSyncStore.ts` setup, ~`:83-91`; the ISSUE-1 comment
lives at `:83-88`):** a probe that lands before the backend behind-ness settles reports
`isSignificantlyBehind:false` while the node IS behind, so `enabled && !behind ⇒ stable`; backing off on
that transient first read would slow the poll to the 5-min cadence **before** the probe settles and
re-surface the exact ISSUE-1 regression (Diagnostics/prompt show "not behind / no offer" for minutes).
Gate the back-off behind a **settled signal — require ≥ `STABLE_READS_BEFORE_BACKOFF` (2) CONSECUTIVE
stable reads before slowing.**

After `_applyAvailability`, evaluate `_isAvailabilityStable()`:
- **stable read:** increment `_consecutiveStableReads`; only when it reaches
  `STABLE_READS_BEFORE_BACKOFF` **and** the poll is currently fast, **slow** the poll (re-arm at
  `AVAILABILITY_REFRESH_BACKOFF_INTERVAL`). The premature first stable read leaves the counter at **1** —
  the poll stays at 30s, the fast poll keeps running and self-corrects (ISSUE-1 preserved).
- **unstable read:** reset `_consecutiveStableReads = 0` and **ALWAYS re-arm the fast 30s interval** (a
  fall-behind after back-off returns to 30s immediately; re-arming an already-fast interval is a harmless
  no-op re-arm).

Factor the interval (re)arm into a small `_armAvailabilityInterval(intervalMs)` helper used by `setup()`
and the back-off; it clears any existing `_availabilityRefreshInterval` (today set up at `:89-91`) before
arming the new cadence. **SLOW, never STOP.** **Preserve the first-load self-correct (ISSUE-1):** the
initial `_refreshAvailability()` in `setup()` and the fast 30s interval until ≥2 consecutive stable reads
are unchanged. Update `teardown()` only if the helper changes the field it clears
(`_availabilityRefreshInterval`).

### 3. Keep the idle poll

The poll still fires regardless of `isWorking` — the proactive prompt (`status==='idle'` gate) and the
Diagnostics dialog remain served. Do NOT restore the `if (this.isWorking)` guard.

## New symbols

- `const AVAILABILITY_REQUEST_TIMEOUT_MS = 10_000;`
- `const AVAILABILITY_REFRESH_BACKOFF_INTERVAL = 300_000;` (5 min)
- `const STABLE_READS_BEFORE_BACKOFF = 2;`
- private counter `_consecutiveStableReads` (default `0`; reset to `0` on every unstable read).
- private helper `_isAvailabilityStable(): boolean` = `!this.isPartialSyncEnabled ||
  !this.isSignificantlyBehind`.
- private helper `_armAvailabilityInterval(intervalMs): void` — clears any existing
  `_availabilityRefreshInterval` then arms a new one at `intervalMs`; used by `setup()` and the back-off.

## Acceptance

- A never-settling availability `request()` no longer pins `_isRefreshingAvailability`; after the timeout
  the guard clears and the next tick can refresh.
- The back-off **cannot** engage on a premature first not-behind read: a single stable read leaves the
  poll at 30s (counter at 1); the poll only slows after **≥2 consecutive** stable reads, so the ISSUE-1
  first-load self-correct is preserved.
- Once availability is settled-stable (disabled, or enabled-and-not-behind for ≥2 consecutive reads) the
  poll backs off to the slower interval; on the **first** unstable read (node falls behind again) it
  resets the counter and re-arms to 30s immediately. The poll **slows, never stops**, so idle consumers
  (the proactive prompt and the Diagnostics dialog) keep updating.

## Tests (add/update)

Spec file: `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

- Inject a never-settling request; advance fake timers past `AVAILABILITY_REQUEST_TIMEOUT_MS`; assert the
  guard cleared and a subsequent tick issues a fresh request.
- Assert the existing idle-refresh test (refresh fires while idle) still passes (the `isWorking` guard
  stays removed).
- **(HIGH-1 i) a premature not-behind read does NOT slow the poll:** a single stable read (enabled &&
  `isSignificantlyBehind:false`) leaves the interval at 30s — advance one back-off worth of fake time and
  assert the poll still fired at the 30s cadence (counter at 1, not yet ≥2). Only a SECOND consecutive
  stable read re-arms the interval at `AVAILABILITY_REFRESH_BACKOFF_INTERVAL`.
- **(HIGH-1 ii) a later fall-behind re-arms to 30s:** after the poll has backed off (≥2 consecutive stable
  reads), feed an unstable read (enabled && `isSignificantlyBehind:true`) and assert the counter resets and
  the interval returns to the fast 30s cadence.
- Keep the teardown-clears-interval test green.

## Verify commands

Per-step (from the master §Sequencing, step 5):

```
yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts
yarn compile
yarn lint
```

> **Renderer verify-env (Node v24):** regen the relevant `.scss.d.ts` via `typed-scss-modules` and ensure the gitignored `identity-obj-proxy` jest sidecar is present **before** treating any tsc/jest failure as a regression (memory: `mithril-ux-renderer-verify-env`). (CAT-H is backend-only ⇒ no `.scss.d.ts` regen.)

(CAT-E changes no `.scss`, so no `.scss.d.ts` regen is needed here; the sidecar caveat still applies to any
jest run.)

## Operator / verify-only gates

None.

## Cross-category coupling notes

- **Shared store file `MithrilPartialSyncStore.ts`** is edited by CAT-B (declares `certifiedEpoch`
  observable), CAT-A (re-pop guard + `_applyAvailability` epoch population), CAT-D (rewrites
  `dismissCompletedOverlay`), and CAT-E (this — `_refreshAvailability` timeout + back-off). Serialize
  **B → A → D → E** so CAT-E lands last and edits the `_refreshAvailability`/`setup`/`teardown` region,
  which the prior three do not touch.
- **Scope split with CAT-H:** CAT-E = poll **frequency** + anti-pin (renderer); CAT-H = per-probe
  **cost** (backend). CAT-E's back-off is gated off while behind, so only CAT-H lowers the per-tick CPU
  during the catch-up window — that is why CAT-H is required, not optional. Do not move backend cost into
  CAT-E.
