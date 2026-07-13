# Mithril Sync — Smoke Test Cheat Sheet

> **Version 3 — 2026-07-13** 
> Tester quick sheet for the Mithril partial sync feature on
> `feat/mithril-partial-sync-ux-refinement`. On screen the feature is called
> **"Mithril Sync"** (vs **"Blockchain Sync"**); "partial sync" is internal naming only.
> Budget ≈ 20–30 min per platform on Preprod.

## Before you start

- The feature flag (`mithrilPartialSyncEnabled`) is **ON for all networks on this branch**.
  If no Mithril UI appears anywhere, check you're on the right build before filing a bug.
- The **proactive prompt** appears only when the node is **significantly behind**: ≥ 20
  immutable files (≈ 1 epoch ≈ ~5 days on mainnet) behind the latest certified Mithril
  snapshot. The Diagnostics **"Mithril Sync"** section is always visible while the feature
  is enabled — near tip and even when the behind-ness check fails — only its copy changes.
  The UI shows the distance in **epochs** — or, when no epoch estimate is available, a
  plain "behind the blockchain tip" line — never file counts.
- The behind-ness check refreshes about every **30 seconds** — allow up to a minute for
  offers to appear after conditions change (and up to ~5 minutes to *re*-appear after the
  check has gone quiet, e.g. after reconnecting the network).
- **Recommended network: Preprod** for the fast loop (small chain, quick download).
  Spot-check Mainnet last — its snapshot is tens of GB and needs disk space and time.

## Where things live (per platform)

| | Linux | macOS | Windows |
|---|---|---|---|
| State dir (mainnet) | `~/.local/share/Daedalus/mainnet` | `~/Library/Application Support/Daedalus Mainnet` | `%APPDATA%\Daedalus Mainnet` |
| State dir (preprod) | `~/.local/share/Daedalus/preprod` | `~/Library/Application Support/Daedalus Pre-Prod` | `%APPDATA%\Daedalus Pre-Prod` |
| Chain folder (the one you delete in Setup) | `<state dir>/chain` | same | same |
| Staging folder (must never persist after a run) | sibling of the chain folder, named `mithril-partial-sync` | same | same |
| Recovery marker | `<state dir>/Logs/mithril-partial-sync.lock` | same | same |

Linux honors `$XDG_DATA_HOME` if set (default `~/.local/share`). Nix dev-shell runs use
the state dir from the generated launcher config instead of these installer defaults.

Deleting `chain` removes **blockchain data only** — wallets are stored separately in the
state dir and are unaffected.

## 0 · Setup — get a far-behind node quickly

The trick: wipe the chain, then **decline** the fresh-install Mithril offer so the node
restarts from genesis. You are now maximally behind, and the Mithril Sync offer fires
almost immediately.

1. [ ] Fully quit Daedalus. Confirm no `cardano-node` process is still running.
2. [ ] Delete the `chain` folder (see table above).
3. [ ] Start Daedalus → the fresh-start **"Fast sync with Mithril"** screen appears
       (a chain-storage location step may show first — keep the default), offering
       **"Use Mithril fast sync"** / **"Blockchain Sync from Genesis"**.
4. [ ] Choose **"Blockchain Sync from Genesis"** → node boots and starts the slow
       blockchain sync from scratch.
5. [ ] Stay on the loading screen until the app connects and reports sync progress —
       the proactive prompt should appear within about a minute.

Re-test trick: prompt dismissal is **session-only**. Restarting Daedalus re-arms the
prompt as long as you are still behind — no need to delete the chain again unless a
Mithril Sync completed and caught you up.

## 1 · Proactive prompt (loading screen)

- [ ] Prompt appears with title **"Mithril Sync"** and body
      *"Your node is about {N} epochs behind."* + *"Mithril can catch you up faster than
      Blockchain Sync."* — N ≥ 1 and plausible (from genesis on mainnet, N is in the
      hundreds ≈ the current epoch number). When no epoch estimate is available the body
      reads *"Your node is behind the blockchain tip."* instead — valid, not a bug (in
      this from-genesis setup you should normally get a number).
- [ ] Note line present: *"Note: If skipped, you can still start the Mithril Sync from
      the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)"* — the shortcut is
      platform-aware: **"(Cmd + D)"** on macOS, **"(Ctrl + D)"** elsewhere.
- [ ] Two buttons: **"Blockchain Sync (slow)"** (secondary) and **"Mithril Sync (fast)"**
      (primary, visually emphasized).
- [ ] Click **"Mithril Sync (fast)"** → nothing starts yet; a confirm view
      **"Mithril Sync Process"** appears explaining node shutdown → verified-data restore
      → node restart, with **"Cancel"** / **"Start now"**.
- [ ] **"Cancel"** on the confirm view → returns to the fast/standard choice (prompt
      stays available, not dismissed).
- [ ] **"Blockchain Sync (slow)"** → prompt disappears and does **not** return for the rest
      of the session (wait > 1 min to confirm).
- [ ] Restart Daedalus → prompt reappears (dismissal is not persisted).
- [ ] **"Start now"** → prompt yields to the full-screen overlay starting with
      *"Stopping Cardano node..."* (continue in section 4).
- [ ] After any attempt this session (cancelled or completed), the prompt does not
      re-pop; the Diagnostics path is the way back in.

## 2 · Replay interrupt button (ledger replay screen)

When the node replays its ledger from on-disk blockchain data — the sync-progress table
shows a **"Replaying ledger from on-disk blockchain"** row — an inline
**"Use Mithril Sync instead"** button appears below the table.

Setup: the section-0 wipe boots from genesis and won't produce a replay. Instead, with
plenty of chain already on disk (e.g. right after a completed Mithril Sync), fully quit
Daedalus, delete only the `ledger` folder **inside** the chain folder (keep `immutable`),
and relaunch — the node recomputes the ledger state from genesis.

- [ ] Button appears once the replay screen has been up a moment (intentional ~2 s
      debounce — not instant), even while replay progress still reads 0%.
- [ ] Clicking it starts Mithril Sync **immediately** — no confirmation step (unlike the
      prompt and Diagnostics paths); the overlay takes over at *"Stopping Cardano
      node..."*.
- [ ] The run succeeds even though no immutable files are missing: expect a very small
      download (ledger-state-only restore), then the normal success flow of section 4.
- [ ] The proactive prompt does **not** appear on this screen (it requires a connected
      node; the replay screen is pre-connection) — the inline button is the only Mithril
      entry point here. The button can also show during the adjacent *"Verifying on-disk
      blockchain state"* phase; same behavior.

## 3 · Diagnostics flow (Help → Daedalus Diagnostics)

Expected state of the **"Mithril Sync"** row:

The section is **always visible** while the feature is enabled; only its copy and button
state change:

| Condition | Expected |
|---|---|
| Significantly behind, no Mithril work running | Row visible with the recommendation copy, button **enabled** |
| Any Mithril work in flight (partial sync or fresh-install fast sync) | Row visible, button **disabled** + hint *"Unavailable while Mithril work is already active."* |
| Node near tip / fully synced | Row **visible** with informational (non-recommending) copy, button still **enabled**; when local data is at or past the latest certified snapshot, the copy says so explicitly |
| Aggregator unreachable / behind-ness check failing (e.g. machine offline) | Row **visible** with an "availability unknown" note, button still **enabled** |

The per-state copy in the first, third and fourth rows lives in the button's **hover
tooltip** — only the "already active" hint renders inline next to the disabled button.

- [ ] While behind (from Setup): open Diagnostics → "Mithril Sync" section and button
      visible and enabled. Hover the button → tooltip explaining Mithril Sync restores
      verified chain data to speed up syncing.
- [ ] Click **"Mithril Sync"** → confirmation **"Before Mithril Sync begins"**:
      *"Your node is about {N} epochs behind..."* + the same process explanation, with
      **"Back to Daedalus Diagnostics"** / **"Start Mithril Sync"**. (The button stays
      enabled near tip / at-or-past snapshot, so the dialog is reachable in those states
      too — its context line then reads accordingly instead of quoting epochs.)
- [ ] **"Back to Daedalus Diagnostics"** → returns; nothing starts.
- [ ] **"Start Mithril Sync"** → the Diagnostics dialog closes itself and the overlay
      takes over.
- [ ] During a run: reopen Diagnostics → button visible but **disabled** with the
      "already active" hint.
- [ ] After a completed Mithril Sync (node near tip): the row **stays visible** with
      informational copy that no longer claims you are behind; the button stays enabled —
      never a dead or misleading recommendation near the tip.
- [ ] Optional: go offline, wait ≥ 1 min, reopen Diagnostics → row stays visible with the
      "availability unknown" note and the button still enabled. Reconnect → the
      recommendation / informational copy returns (may take up to ~5 min).

## 4 · Progress overlay & success

- [ ] *"Stopping Cardano node..."* frame with explanation text and a **ticking elapsed
      timer** — the dialog must never look frozen (Windows: this phase can take several
      minutes; that is known and accepted, but the timer must keep ticking).
- [ ] Waterfall steps **Preparing → Downloading → Finalizing**; the active step's circle
      **rotates** (animated spinner, not a static dot).
- [ ] Downloading shows the combined progress bar with **no named sub-steps**; a
      *"Verifying snapshot..."* sub-step appears under Downloading once verification
      starts. Under Finalizing, *"Converting snapshot format..."* then
      *"Installing snapshot..."* appear in turn — *"Moving snapshot to storage"* belongs
      to the fresh-install flow only and must never appear here.
- [ ] Download progress reads *"Snapshot files: X / Y files | Ledger state: X / Y"* —
      snapshot files as counts, ledger state as data sizes (e.g. *"154 MB / 683 MB"*).
      No *"≈ N GB total"* line appears in this flow (that context is fresh-install-only);
      the combined bar advances by file counts, not bytes.
- [ ] Elapsed time ticks every second through every working phase. (It is intentionally
      hidden on the *"Cleaning up..."* frame after a cancel and frozen on terminal
      screens — not a bug.)
- [ ] Long non-download phases show *"This can take several minutes — Daedalus is still
      working."*
- [ ] During *"Installing snapshot..."* the active step shows the caution *"To preserve
      data integrity, please don't close Daedalus until this step is complete."*
- [ ] *"Starting Cardano node..."* frame appears after the restore — promptly once
      Finalizing begins, with no multi-minute stall in between (a fixed mainnet bug once
      held this for up to ~8 minutes).
- [ ] Success: *"Mithril Sync completed successfully."* → auto-transition *"Returning to
      Daedalus..."* after ~4 s → normal UI, node syncing near the tip.
- [ ] Failure path (hard to force — verify only if it happens): if the automatic
      "Returning to Daedalus..." hand-off fails, it retries once by itself; if the retry
      also fails, the overlay switches to the Mithril **error screen with a Retry action**
      that re-runs the hand-off — never a silently stuck success frame.
- [ ] Post-success cleanup: staging folder `mithril-partial-sync` **gone**; marker
      `Logs/mithril-partial-sync.lock` **gone**; Diagnostics row shows the near-tip
      informational copy (no longer recommends); prompt does not reappear; you can
      quit/relaunch with no recovery dialog.

## 5 · Cancelling attempts

Cancel button availability by phase:

| Phase on screen | Cancel button |
|---|---|
| "Stopping Cardano node..." | Visible but **disabled**; tooltip *"Cancellation available once the node has stopped"* |
| Preparing / Downloading / Verifying / Converting | **Enabled** |
| "Cleaning up..." / "Installing snapshot..." / Finalizing / "Starting Cardano node..." / completed | **Hidden** (past the point of no return) |

- [ ] Start a Mithril Sync; during node stop, Cancel is disabled with the tooltip and
      clicking it does nothing.
- [ ] Once **Downloading**, click **Cancel** (no extra confirmation dialog is expected)
      → *"Cleaning up..."* → terminal screen **"Mithril Sync was cancelled"** with calm,
      non-error copy (*"...Your existing chain data is unchanged..."*) — clearly distinct
      from a failure screen.
- [ ] Recovery buttons offered: **"Retry Mithril Sync (fast)"** and
      **"Restart Blockchain Sync (slow)"**.
- [ ] **"Restart Blockchain Sync (slow)"** → node restarts and blockchain syncing resumes
      on the unchanged chain; the Diagnostics "Mithril Sync" button becomes available again
      (within ~1 min) — no app restart needed.
- [ ] After cancel: staging folder removed; `chain` folder intact.
- [ ] Run again and cancel, then choose **"Retry Mithril Sync (fast)"** → a fresh run
      proceeds; let it complete and re-verify the section-4 success checks.
- [ ] Watch the late phases (Installing snapshot / Finalizing / Starting node):
      the Cancel button must not be visible at all.

## 6 · Edge checks (optional, quick)

Failure-screen recovery buttons: errors **before** the cutover (Preparing / Downloading /
Verifying / Converting) offer **three** actions — **"Retry Mithril Sync (fast)"**,
**"Restart Blockchain Sync (slow)"**, **"Wipe chain data and do full Mithril Sync"**.
Errors **at or after** the cutover (Installing / Finalizing / node start, e.g.
**"Node failed to start"**) offer **only** the wipe button. Mid-run failures carry
specific titles — *"The Mithril snapshot could not be verified"*, *"Preparing the
Mithril snapshot failed"*, *"The verified Mithril snapshot moved on"* (retriable) —
not generic copy. (Only the **cancelled** screen has exactly two buttons; see section 5.)

- [ ] **Quit mid-download:** quit Daedalus while Downloading → relaunch → app starts
      normally, no blocking dialog; node resumes blockchain sync and Mithril Sync can be
      started again.
- [ ] **Quit during cutover (advanced, destructive):** quitting exactly during
      "Installing snapshot..." (the live-chain cutover, marked with the don't-close
      caution; quitting later, during Finalizing, resumes normally on relaunch) must
      produce, on next launch, the native blocking dialog
      **"Interrupted Mithril Sync detected"** with
      **"Wipe chain and full Mithril Sync"** / **"Quit"**. Wipe leads back to the
      fresh-start flow (Setup step 3). The window is seconds wide — best effort only.
- [ ] **Low disk:** with less free space on the chain volume than Mithril Sync needs
      (snapshot size minus current chain-data size, plus a 20%-of-snapshot margin;
      never less than ~4 GB), starting Mithril Sync fails cleanly during Preparing
      with **"Not enough disk space for Mithril Sync"** quoting required vs available
      GB in the error details, plus the three-button recovery — never a crash. A node
      already near the tip needs far less free space than the full snapshot. If the
      snapshot's size can't be determined, Preparing fails with **"Checking the latest
      Mithril snapshot failed"** instead — also clean, with the same recovery actions.
- [ ] **Offline mid-download:** cut the network while Downloading →
      **"Downloading the Mithril snapshot failed"** + *"Check your internet
      connection..."* with the three-button recovery; reconnect and Retry succeeds.

## Platform-specific notes

**Linux**
- Baseline platform; no special Mithril behavior. Paths honor `$XDG_DATA_HOME`.

**macOS**
- State dirs carry the spaced network suffix (`Daedalus Mainnet`, `Daedalus Pre-Prod`);
  a plain `~/Library/Application Support/Daedalus` folder is a legacy path — ignore it.
- The prompt's shortcut note is platform-aware: it must read **"(Cmd + D)"** on macOS
  (**"(Ctrl + D)"** elsewhere) — flag if you see Ctrl on a Mac.

**Windows**
- Node stop is noticeably slower — the *"Stopping Cardano node..."* frame may sit for
  several minutes (known/accepted). Verify the elapsed timer keeps ticking and the frame
  never looks dead.
- Cancel uses a hard process kill (`taskkill /f`) under the hood — after cancelling during
  Downloading, verify the cancelled screen still appears and the staging folder is removed.
- Binaries are `.exe` (`mithril-client.exe`) — if a run never leaves Preparing, check
  antivirus/SmartScreen hasn't quarantined it.
- Paths live under `%APPDATA%` (Roaming).

## After testing

- To leave the machine synced fast: finish with one successful Mithril Sync, or wipe the
  chain and pick **"Use Mithril fast sync"** on the fresh-start screen.
- Confirm nothing was left behind: no `mithril-partial-sync` staging folder next to the
  chain, no `mithril-partial-sync.lock` in `Logs/`.

## Filing findings

Include: platform + network, the phase shown on screen, the exact on-screen copy, a
timestamp, and a copy of `<state dir>/Logs/` (plus `mithril-partial-sync.lock` if it
exists at failure time).

## Version history

- **v3 — 2026-07-13:** task-ux-706 copy renames ("Blockchain Sync" vocabulary, new
  replay-interrupt section) plus a full code-verified accuracy pass: corrected the
  prompt-during-replay and "≈ N GB total" claims, timer behavior during cancel,
  tooltip-only Diagnostics copy, and pre-/post-cutover failure recovery buttons.
- **v2 — 2026-07-03:** task-ux-703 revision (progress-overlay frames, cancel matrix,
  edge checks aligned with the remediated flow).
- **v1 — 2026-07-02:** initial sheet.
