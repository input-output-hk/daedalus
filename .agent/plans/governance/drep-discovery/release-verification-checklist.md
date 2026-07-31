# DRep Discovery — Release Verification Checklist (task-125)

> **Owner:** the human verifier. **Interaction mode:** `manual_execution`.
> **Status of the tracker row:** `task-125` stays `pending` until a completed run of
> this checklist is recorded. **No agent may close any item here.**
> **Feature:** DRep Discovery (`.agent/plans/governance/drep-discovery/`) —
> [plan](./governance-drep-discovery-plan.md) ·
> [tasks](./governance-drep-discovery-plan-tasks.json) ·
> [slice-8 PRD](./task-plans/slice-8-PRD.md) ·
> [slice-8 findings](./research/slice-8-findings.md)
> **Written at:** slice-8 close, commit `45efc1911` (`feat/drep-discovery`).

---

## 1. What this document is

`task-125` is the feature's single surviving aggregate acceptance:

> _Release verification confirms users can complete browse -> evaluate -> select ->
> delegate without external portals on a synced node._

This checklist is the executable form of that criterion. It is the **only** autonomous
deliverable for task-125 — authoring it does not discharge it. The acceptance is
discharged by a human running the legs below on a real environment and filling in
[§9 Result record](#9-result-record).

**It is not** a regression suite (Jest and Storybook cover that), not a copy review
(the `!!!` preliminary markers are expected and are not defects — see §8), and not a
performance benchmark (the timing budgets in §6 are observed, not measured with a
stopwatch).

**Promotion rule.** Only a completed §9 table justifies moving `task-125` past
`pending`, and only the user may move it. `verified` for any DRep-discovery row
requires exactly this kind of independent evidence
(`prompt.md:231-233`); no slice-8 row currently holds it.

---

## 2. Environment preconditions

Every precondition is a hard gate. If one cannot be met, stop and record the run as
**blocked**, not as failed — a blocked leg and a broken leg are different findings.

| # | Precondition | How to confirm | Pass |
| --- | --- | --- | --- |
| P-1 | **Packaged build**, not the dev shell | The app was installed from an installer artifact. `nix/internal/launcher-config.nix:295` forces `LedgerDB.Backend: "V2LSM"` on packaged builds; a dev shell runs a different backend, so dev-shell timings and behaviour are not evidence for this run. | ☐ |
| P-2 | **Synced node** | Daedalus reports the node in sync (sync indicator at 100%, `isNodeInSync === true`). Every leg except R-5 requires this. | ☐ |
| P-3 | Network is **mainnet or preprod** | Not selfnode. Selfnode has its own leg (§7) and its own launch. | ☐ |
| P-4 | **Software wallet** available, funded enough to pay the delegation certificate fee and deposit | Wallet list shows a restored/created software wallet with a non-zero balance. | ☐ |
| P-5 | **Real hardware wallet** available — Ledger or Trezor, physical device, unlocked, Cardano app open | The device is paired and visible in Daedalus. An emulator or a mocked device does **not** satisfy this leg. | ☐ |
| P-6 | At least one DRep on the network has a **resolvable metadata anchor**, and at least one has **none** | Needed for E-2 / E-3. On preprod both are normally present; if not, record E-3 as not-observed rather than passing it. | ☐ |
| P-7 | The verifier is a **human** | Self-evident. Recorded because it is the criterion's whole point. | ☐ |

Record the environment in §9 before starting: build hash/version, network, node tip
(epoch/slot), wallet names, device model and firmware.

---

## 3. Standing assertions — checked throughout, not once

These are not steps. Watch for them during every leg and record them at the end.

| # | Assertion | Fail condition | Pass |
| --- | --- | --- | --- |
| SA-1 | **No external portal.** The full journey completes without GovTool, Koios, Blockfrost, an explorer, or any hosted governance API. | Any leg requires leaving Daedalus to obtain a DRep ID, a DRep's identity, or its metadata. Opening a DRep's own anchor URL in a browser **out of curiosity** is not a failure; **needing** to, to complete a leg, is. | ☐ |
| SA-2 | **No network prompt for discovery data.** The directory populates from the local node only. | The app is unusable offline-from-the-internet-but-connected-to-the-node for browse/evaluate/select. (Anchor metadata fetching is the one deliberate outbound call and is labelled as such in the detail view.) | ☐ |
| SA-3 | **Sanitization floor.** After the run, spot-check `Logs/pub/`. No log line, analytics payload or crash report contains a DRep ID, a CIP-129/CIP-105 bech32 string, or the literals `abstain` / `no_confidence`. | Any such string appears. **Documented exception:** `Logs/pub/DRep-state-snapshot.json` deliberately carries public on-chain directory data and bypasses redaction — it must contain no record of *your own* vote. | ☐ |
| SA-4 | **No auto-delegation.** Daedalus never pre-selects or suggests a specific DRep as the choice. | A DRep arrives pre-filled in the delegation form without the verifier having selected it. | ☐ |

---

## 4. The journey — browse → evaluate → select → delegate

Run legs B through H **in order**, once end-to-end on the **software wallet**, then
repeat legs E through H on the **hardware wallet**. Legs B–D need not be repeated for
the second wallet.

### B — Browse

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| B-1 | From the wallet's **Voting / Governance** screen (`/voting/governance`), open the DRep directory via the **Browse DReps** link in the delegation form. | The app navigates to `/governance/dreps`. The verifier reached the directory **from the delegation flow**, not by typing a route. | ☐ |
| B-2 | Observe the first paint. | A **skeleton list** of placeholder cards appears immediately; it is replaced by real cards without an intervening blank screen. See R-1 for the timing expectation. | ☐ |
| B-3 | Read the directory header. | Title, a **Refresh** button, and a **"Last updated <relative time>"** line are present. The relative time is plausible ("a few seconds ago" on first load). | ☐ |
| B-4 | Read the **cohort banner**. | The banner explains that the list is a randomized recommended cohort rather than the full registry, and offers a **Reshuffle** link. | ☐ |
| B-5 | Click **Reshuffle**. | The visible set of DReps changes. The list stays interactive; no full-page spinner. | ☐ |
| B-6 | Scroll / page through the list. | Pagination advances (25 cards per page). Cards render a status badge, an identity, and voting power (or `—` with an explanatory tooltip if phase-2 stake data has not landed). | ☐ |
| B-7 | Confirm the directory contains **only registered DReps**. | Neither **Abstain** nor **No Confidence** appears as a directory entry, card, or search result. They are delegation-form choices only. | ☐ |

### E — Evaluate

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| E-1 | Open a DRep's detail view (**View details** on a card → `/governance/dreps/<drepId>`). | The detail view opens. A **Back to directory** affordance returns to the list. | ☐ |
| E-2 | Read the **on-chain section**. | It is labelled as on-chain data with its source label. Status, expiry (in epochs), voting power and vote positions render, or show an explicit unavailable/loading state — never a blank or a raw error string. | ☐ |
| E-3 | Open a DRep **with** a metadata anchor. | The anchor section shows the anchor URL and hash, and the fetched content (name, objectives, motivation, qualifications, references) renders under its **own** label, visibly distinct from the on-chain section. Unverified or failed anchor content is labelled as such, never presented as on-chain fact. | ☐ |
| E-4 | Open a DRep **without** an anchor. | The chain-native view still works: the on-chain section renders fully and the anchor section states plainly that no anchor is present. No error banner, no empty page. | ☐ |
| E-5 | Compare the identity shown in the detail view with the identity on the card. | Byte-identical. Both CIP-129 and CIP-105 forms, where both are shown, refer to the same DRep and are individually copyable. | ☐ |

### S — Search and show-all

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| S-1 | Type a prefix of a known DRep's ID or name into the directory search. | Matching entries appear as you type; non-matching ones disappear. | ☐ |
| S-2 | Search for a string that matches nothing. | The **no-results** empty state appears, offering **Clear filters** and **Show all**. The copy stays scoped to registered DReps and filters — it must not imply Abstain / No Confidence are found here. | ☐ |
| S-3 | Click **Show all**. | The list expands beyond the recommended cohort, and a **sort-bias warning** explains that the full registry is ordered differently from the recommended cohort. | ☐ |
| S-4 | Click **Clear filters**. | The directory returns to the recommended cohort with search cleared. | ☐ |

### F — Favorites

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| F-1 | Toggle the favorite control on two DReps. | The control reflects the on state immediately. | ☐ |
| F-2 | Open the favorites view (`/governance/favorites`). | Exactly the two favorited DReps are listed, with their status badges. | ☐ |
| F-3 | Un-favorite everything, then open the favorites view. | The **no-favorites** empty state renders with a route back to the directory. Re-favorite the two before continuing. | ☐ |
| F-4 | **Quit Daedalus completely and relaunch.** Reopen the favorites view. | Both favorites survived the restart (they persist in local storage, not in memory). | ☐ |
| F-5 | If any favorited DRep is retired or excluded from the cohort, check how it renders. | It still renders, with the appropriate status badge and the stale caption. **Favorites are never silently purged.** If no such DRep exists, record as not-observed. | ☐ |

### D — Select and delegate (software wallet)

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| D-1 | From a card (or the detail view), choose **Select** / the delegation action. | The app returns to the wallet's delegation form with that DRep pre-filled **as a result of your selection**. The wallet you started from is still the active wallet. | ☐ |
| D-2 | Compare the pre-filled DRep identity with the one you selected. | **Byte-equal.** Not truncated-and-guessed — copy both and compare, or compare the full string character for character. | ☐ |
| D-3 | Submit the delegation and open the confirmation dialog. | The dialog shows the same DRep identity, again **byte-equal** to the selection. Fee and deposit are shown. | ☐ |
| D-4 | Enter the spending password and confirm. | The transaction is accepted by the node. No error. | ☐ |
| D-5 | Once the transaction is on chain, return to the Voting / Governance screen. | The current-vote summary reflects the new delegation and names the same DRep identity. | ☐ |

### H — Select and delegate (hardware wallet, real device)

Repeat D-1 … D-3 with the **hardware** wallet active, then:

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| H-1 | Submit the delegation; follow the on-device prompts. | Daedalus asks for on-device confirmation and the device wakes with a vote-delegation request. | ☐ |
| H-2 | **Read the DRep ID on the device screen** and compare it with the DRep you selected in Daedalus. | **Byte-equal on the device itself.** This is the load-bearing check of the whole run: the signed payload's DRep ID must equal what the user chose. Do not infer it from the Daedalus screen — read the device. | ☐ |
| H-3 | Approve on the device. | The transaction is signed and accepted. | ☐ |
| H-4 | Reject on the device (optional second pass, on a fresh attempt). | Daedalus surfaces the rejection cleanly and no transaction is submitted. | ☐ |
| H-5 | After confirmation lands, check the current-vote summary for the hardware wallet. | It names the same DRep identity. | ☐ |

---

## 5. Abstain / No Confidence sanity check

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| N-1 | In the delegation form, choose **Abstain**, then **No Confidence**. | Both are selectable **in the form**. Neither ever appeared as a directory entry (see B-7). | ☐ |
| N-2 | Confirm one of them on the software wallet (optional; costs a fee). | The confirmation dialog names the sentinel, not a DRep ID. The logs contain neither literal (SA-3). | ☐ |

---

## 6. Refresh-latency and load states

These are the states slice-8's task-123 built. Several must be **induced** — the
instructions below say how. Where an induced state cannot be produced safely on the
verifier's environment, record it as not-observed rather than passing it.

| # | State | How to induce | Pass criterion | Pass |
| --- | --- | --- | --- | --- |
| R-1 | **Skeleton first load** | Cold-enter `/governance/dreps` with no cached state (fresh app launch). | Placeholder card skeletons appear essentially immediately (budget: ≤700 ms before something is on screen) and hold the page height; real cards replace them without layout collapse. A bare spinner is **not** the expected rendering. | ☐ |
| R-2 | **Stale-while-refresh** | With the list loaded, click **Refresh**. | A small spinner badge appears **next to the "Last updated <time>" line**, the Refresh button disables while in flight, and **the list stays on screen and interactive** — it does not blank out or revert to the skeleton. | ☐ |
| R-3 | **Refresh failed with retained data** | Load the directory, then stop `cardano-node` (or otherwise make its socket unavailable), then click **Refresh**. | An inline banner reads *"Couldn't refresh DRep data. Retry. Showing last successful snapshot from <time>."* The **previous list remains visible and usable**, and the banner offers a retry. No raw main-process error text and no file path is shown to the user. Restart the node afterwards. | ☐ |
| R-4 | **Ranking unavailable** | Best-effort: the phase-2 stake query failing while phase 1 succeeded. Often not inducible by hand. | If it occurs: the list still renders, the voting-power column shows `—` with a tooltip, and a banner reads *"Voting power data unavailable this refresh. Ranking-based filters disabled."* Otherwise record as not-observed. | ☐ |
| R-5 | **Node syncing (soft warning)** | Run this leg **before** P-2 is satisfied — i.e. during the initial sync, or after resyncing from a snapshot. | A persistent banner states the node is still syncing with a percentage, **the directory still renders whatever data is available**, and the banner clears once the node reaches the tip. The directory is **not** hard-gated on sync. | ☐ |
| R-6 | **noSync empty fallback** | While the node is syncing (R-5) *and* the query yields zero DReps. | The `noSync` empty state explains that DRep data becomes available at the tip. Not-observed is an acceptable outcome. | ☐ |
| R-7 | **Timeout** | Not reliably inducible by hand; the main process rejects phase 1 after 10 s. | If a phase-1 timeout occurs, the app shows R-3's banner (with retained data) or the generic error state with a Retry (without) — never an indefinite spinner. | ☐ |

**No renderer timer.** Slice-8's design keeps the main process as the single timeout
authority. A symptom of that contract breaking would be a banner appearing while a
refresh is plainly still running, or a spinner that never resolves after an error.
Note either if seen.

---

## 7. Selfnode empty state — separate launch

The selfnode state **cannot** be exercised on the synced-node environment of §2. It
needs its own short run.

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| X-1 | Launch Daedalus against the **selfnode** cluster. | The app starts normally. | ☐ |
| X-2 | Open `/governance/dreps`. | The **entire list area** is replaced by the selfnode empty state: an unavailability indicator carrying **both** an icon and a textual label (colour is never the sole signal), plus copy stating that DRep directory data is unavailable on the selfnode cluster. | ☐ |
| X-3 | Confirm no partial directory is shown. | **Zero** DRep cards, zero pagination, no retained-snapshot banner beside the empty state. | ☐ |
| X-4 | Confirm no raw error leaks. | The main process's internal message (e.g. anything naming the CLI, a socket path or a mode flag) is **not** on screen. | ☐ |
| X-5 | Click **Refresh** a few times, then navigate away and back. | The same empty state returns each time. No error dialog, no spinner storm, no growing memory. (By design the guard rejects before any CLI process is spawned, so repeated refreshes are cheap.) | ☐ |
| X-6 | In `ja-JP`, repeat X-2. | The Japanese copy renders and the label is not clipped. | ☐ |

---

## 8. Localization and copy

| # | Step | Pass criterion | Pass |
| --- | --- | --- | --- |
| L-1 | Switch the app language to **Japanese** and repeat B-2 … B-6, S-2, R-2 and R-3. | Every governance string renders in Japanese. The refresh-failed banner in particular **wraps to two or more lines and reflows vertically — it must never be ellipsized or clipped** (JA copy runs 30–60% longer than EN). | ☐ |
| L-2 | Note the `!!!` prefixes. | **Expected, not a defect.** Every preliminary string carries the `!!!` marker until the release-end manual copy review, which is a separate, still-open activity. Do **not** fail an item for it. | ☐ |

---

## 9. Result record

Fill this in during the run. An unfilled table is not a completed verification.

**Environment**

| Field | Value |
| --- | --- |
| Daedalus version / build hash | |
| Installer artifact (platform) | |
| Network | |
| Node version | |
| Node tip at start (epoch / slot) | |
| Software wallet | |
| Hardware wallet (vendor / model / firmware / Cardano app version) | |
| Verifier | |
| Date | |

**Legs**

| Leg | Items | Result (pass / fail / blocked / not-observed) | Notes, defect links |
| --- | --- | --- | --- |
| Preconditions | P-1 … P-7 | | |
| Standing assertions | SA-1 … SA-4 | | |
| Browse | B-1 … B-7 | | |
| Evaluate | E-1 … E-5 | | |
| Search / show-all | S-1 … S-4 | | |
| Favorites | F-1 … F-5 | | |
| Delegate — software | D-1 … D-5 | | |
| Delegate — hardware (real device) | H-1 … H-5 | | |
| Abstain / No Confidence | N-1 … N-2 | | |
| Refresh-latency states | R-1 … R-7 | | |
| Selfnode (separate launch) | X-1 … X-6 | | |
| Localization | L-1 … L-2 | | |

**Verdict**

- [ ] **task-125 acceptance met** — the journey browse → evaluate → select → delegate
      completed on a synced node, on **both** wallet types, with on-device
      confirmation observed on a real device, and **without any external portal**.
- [ ] Not met — see the failing legs above.

A verdict of "met" is what promotes `task-125` in
`governance-drep-discovery-plan-tasks.json`. Record the run's date and the build hash
in the row's `statusReason`, and add this file to its `evidence`.

---

## 10. Deliberately out of scope

Considered and dropped; **do not re-add** to this checklist:

- `cardano-cli` / `cardano-node` version and `LedgerDB.Backend` provenance capture as
  an acceptance criterion. Recorded once in
  [`research/ux-refinement-sync-and-load-research.md`](./research/ux-refinement-sync-and-load-research.md);
  task-125 gained no provenance criterion.
- Stopwatch measurement of the refresh budgets. The latency-measurement remainder is
  task-166's, not this checklist's; R-1 … R-3 observe the visual contract only.
- The release-end `!!!` copy review (see L-2) and the Storybook visual pass — both are
  separate open activities.
- Fixing the pre-existing `yarn stylelint` ordering debt in the governance SCSS — a
  pre-merge code obligation recorded in
  [`research/slice-8-findings.md`](./research/slice-8-findings.md), not a runtime
  behaviour this run can observe.
