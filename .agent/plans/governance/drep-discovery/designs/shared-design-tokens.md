# DRep Discovery — Shared Design Tokens

These tokens are referenced across the DRep Discovery design. They are intentionally framework-agnostic — the design doc maps them onto react-polymorph components and SCSS module class names.

## 1. Status Badges

> Token names in this document are the final semantic aliases for implementation planning. They map onto the existing Daedalus theme variables in `source/renderer/app/themes/`; reuse these semantic slots instead of inventing governance-specific color names.

| State | Source | Label (en) | Label (ja) | Light token | Dark token | Icon |
|---|---|---|---|---|---|---|
| Active | `drep-state` | Active | アクティブ | `--badge-success-bg` / `--badge-success-fg` | same tokens | dot |
| Inactive | `drep-state` (no votes within `drepActivity`) | Inactive | 非アクティブ | `--badge-neutral-bg` / `--badge-neutral-fg` | same | dot |
| Expiring soon (≤6 epochs) | derived | Expiring in {n} epochs | あと{n}エポックで失効 | `--badge-warning-bg` / `--badge-warning-fg` | same | warning triangle |
| Retired | `drep-state` | Retired | 退任 | `--badge-disabled-bg` / `--badge-disabled-fg` | same | none |
| Top-35 (search/show-all only) | derived from `drep-stake-distribution` | Excluded from default cohort | デフォルト候補外 | `--badge-info-bg` / `--badge-info-fg` | same | info `i` |
| Selfnode / CLI unsupported | `SelfnodeCliUnsupported` | DRep data unavailable on selfnode | DRepデータ利用不可 | `--badge-disabled-bg` | same | warning |

Contrast rule: every badge must meet **WCAG 2.1 AA 4.5:1** in both themes. Color must never be the sole indicator — pair every status with an icon **and** the textual label. Existing `_votingConfig.scss` already exposes the success/warning/neutral palette; reuse, don't redefine.

**Where the status badge is rendered.** The status badge appears on every DRep card in the directory, on the DRep detail view, and on the `CurrentVoteSummary` panel's `drep` state. Surfacing it on `CurrentVoteSummary` is binding: users must be able to tell at a glance whether the DRep they currently delegate to is still active or about to lapse.

## 1a. Category Badges (per-card informational)

Every DRep card in the directory and the DRep detail view renders **exactly one** category badge, drawn from the four-value enum below. The badge is informational only — it never reorders the cohort and never overrides the randomized default sort. A tooltip on the badge explains the rule that placed the DRep in that category. The category set is the agreed early stand-in for a future "Recommended" badge; the default cohort itself IS the recommended sort in Phase 1 (no separate Recommended tab or per-card Recommended badge ships in this release).

Implementation staging is locked: `slice-5` renders Primary / Threshold / Non-metadata only. High value activates in `anchor-1` once verified metadata-completeness exists.

| Category | Rule (informational) | Label (en) | Tooltip copy (en) |
|---|---|---|---|
| High value | Inside the default randomized cohort AND completed metadata AND voting power above the cohort median | High value | "Inside the default Recommended view, with verified metadata and voting power above the cohort median." |
| Primary | Inside the default randomized cohort AND completed metadata | Primary | "Inside the default Recommended view with verified metadata." |
| Threshold | Inside the default randomized cohort but expiry within the 6\u201312 epoch window (still above the 6-epoch floor) | Threshold | "Inside the default Recommended view but approaching expiry \u2014 review before delegating." |
| Non-metadata | Eligible for the cohort but anchor metadata is missing or unverified | Non-metadata | "Eligible for delegation but has no verified off-chain metadata yet." |

Labels and tooltip copy land in i18n under `governance.drepDirectory.category.*` (see \u00a79). The four labels are deliberately short to avoid wrapping inside cards in JA / DE; copy can iterate but the four-value enum is fixed for Phase 1.

**Priority rule (binding).** When a DRep satisfies more than one category simultaneously, the highest-priority badge wins. Priority order (highest → lowest): **High Value → Threshold → Primary → Non-metadata**. A DRep with metadata that is also approaching expiry (6–12 epochs) always shows **Threshold**, not Primary.

## 2. Source Labels (anchor-ready)

Every rendered field gets an explicit provenance label. This is the single most important anti-misleading-content control.

| Label | When applied | Visual | Tooltip copy (en) |
|---|---|---|---|
| **On-chain** | DRep ID, registration epoch, expiry, voting power, active/inactive, vote positions | small pill, `--source-onchain-fg`, monospace `chain` icon | "Read directly from the Cardano ledger by your local node." |
| **On-chain anchor reference** | The raw anchor URL and hash pair that lives on-chain (the *reference* itself, distinct from the *content* it points to) | small pill, `--source-onchain-fg`, link-chain icon | "The anchor URL and hash recorded on-chain. Content is not fetched or verified at this label." |
| **Verified off-chain content** | Anchor-derived fields after hash verification (`anchor-1` onward) | small pill, `--source-verified-fg`, check-shield icon | "Fetched from {host}, hash-matched the on-chain anchor hash." |
| **Unverified anchor** | Anchor *content* has been fetched but not yet hash-verified (transitional state during the `anchor-1` fetch pipeline). Never applied to the raw URL/hash pair on-chain — that uses **On-chain anchor reference**. | small pill, `--source-unverified-fg`, dashed-circle icon | "Anchor content fetched but not yet hash-verified. Treat as untrusted." |
| **Anchor unavailable** | Fetch or hash check failed | small pill, `--source-warning-fg`, warning triangle | "The anchor URL could not be retrieved or did not match the on-chain hash. Off-chain profile is not shown." |

Token names follow the existing `themes/` convention (e.g., `--theme-source-onchain-color`); naming-only — actual hex deferred to theme tokens used by `staking/delegation-center/`.

## 3. Voting Power Formatting

Voting power is `BigNumber | null` (lovelace). Never `Number`.

Two rendering forms:

- **Card / table**: human-rounded ADA via existing `formattedNumber()` style, two significant figures, with explicit `₳` glyph. Examples: `₳ 1.2M`, `₳ 23M`, `₳ 688K`, `₳ 0`.
- **Detail / confirmation**: full ADA with thousands separators **and** raw lovelace on a secondary line for evaluators. Example:
  ```
  ₳ 23,137,980.123456
  (23,137,980,123,456 lovelace)
  ```

`null` voting power = ranking unavailable → render `—` with a tooltip "Stake distribution unavailable this refresh." Do **not** silently fall back to 0.

Screen-reader announcement: use the visible localized number string directly in `aria-label` (e.g., `aria-label="Voting power 23,137,980 ada"`). Card surfaces add an `aria-describedby` element with the text `"rounded display, exact value in detail view"` so SR users know the rounded card value is not the exact figure. Do **not** synthesize a number-to-words form; reuse whatever the locale-aware formatter already produces.

## 4. DRep ID Display

Always show both CIP-129 (new bech32) and CIP-105 (legacy `drep1…`) when both are derivable. In cards, show CIP-129 primary + truncated middle (e.g., `drep1yg7s…aj8ras`) with a copy button. In detail, show both forms fully, monospaced, each with its own copy button.

Copy button feedback: existing Daedalus toast / inline confirmation. Toast copy: `"DRep ID copied"`. Announce via aria-live polite.

## 5. Randomization Indicator

The default-cohort view is randomized. Users must understand this without surprise.

- Inline banner at the top of the directory: `"Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power. {ShowAllLink} or {SearchLink} to find any DRep."` A secondary line credits the source: `"Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis."` The BMVG citation is a single line and may collapse into a tooltip on narrow widths; it must never be removed.
- The banner remains visible (not dismissible per-session) so refresh-induced reorder is never mysterious.
- When a filter or search is active, banner switches copy: `"Showing {n} DReps matching your filters. Default randomized order does not apply."`
- The seed is created once per app session. Explicit refresh preserves the current seed; a subtle "Reshuffle" link is the only control that reseeds without re-querying (the seed lives in `GovernanceStore`).

## 6. Refresh State

| Phase | Trigger | Visual | Time budget |
|---|---|---|---|
| Initial load | route enter, no cached state | full skeleton list | ≤700 ms before skeleton |
| Stale-while-refresh | explicit refresh or route re-enter with cached data | small spinner badge next to "Last updated {time}" timestamp, list interactive | up to 10 s |
| Timeout / failed | spawn or parse failure from `GovernanceQueryService` | inline error banner: `"Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}."` | banner appears at 10 s |
| Ranking unavailable (partial) | `drep-stake-distribution` failed but `drep-state` succeeded | list renders, voting-power column shows `—` with tooltip, banner: `"Voting power data unavailable this refresh. Ranking-based filters disabled."` | immediate |

The "Last updated {time}" timestamp is part of the directory header. Format: relative ("3 minutes ago") with absolute ISO timestamp in tooltip.

## 7. Confirmation Dialog Identity

The confirmation dialog must render the DRep identity in three byte-exact, mutually consistent representations so a user (and a test harness) can verify the on-screen identity matches what the signing layer hands to the wallet and the hardware device.

Until verified anchor pipeline lands:

```
You are delegating your voting power to:
CIP-129 DRep ID:  drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras
CIP-105 DRep ID:  drep185r8rr6j9evjs…uutaz3        (when derivable)
Signed payload:   { vote: { type: "drep", id: "<hex credential>" } }
                  (Source: On-chain)
```

After `anchor-2` (verified anchor only):

```
You are delegating your voting power to:
{verified givenName}
CIP-129 DRep ID:  drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras
CIP-105 DRep ID:  drep185r8rr6j9evjs…uutaz3        (when derivable)
Signed payload:   { vote: { type: "drep", id: "<hex credential>" } }
                  (Source: On-chain · Name: Verified off-chain content)
```

Never show an unverified anchor name in the confirmation dialog — confirmation is a security surface.

**Identity equality rule (binding):** The DRep identifier displayed on the hardware device must be **byte-equal** to the identifier rendered in the confirmation dialog *and* to the `vote.id` field of the signed payload. The CIP-129 and CIP-105 strings shown to the user must both decode to the same underlying DRep credential bytes as the payload `id`.

**Acceptance criterion (HW tests):** A hardware-wallet e2e test must assert that the identifier surfaced by the device prompt is byte-equal to `vote.chosenOption` (or the equivalent field in the constructed tx body). This is a release-blocking assertion.

## 8. Hardware Wallet Confirmation

Reuse the existing on-device prompt UI from `VotingStore` hardware path. Add a permanent caption inside the dialog:

> Confirm on your {Ledger | Trezor}. The DRep ID shown on the device will match the ID above. Do not approve if they differ.

Sub-state mapping is driven by the canonical `HwDeviceStatus` enum in `source/common/types/hardware-wallets.types.ts`. The five governance-specific copy variants below cover the in-flow states a user can encounter during a DRep delegation; **long-tail device errors fall through to Daedalus's existing HW error-mapping infrastructure** (used by ADA send / pool delegation today) — do not duplicate that mapping here.

| Sub-state | `HwDeviceStatus` | Message ID | Copy |
|---|---|---|---|
| Device disconnected | `disconnected` | `governance.hw.disconnected` | "Hardware wallet disconnected. Reconnect and unlock your device to continue." |
| Device locked | `device_locked` | `governance.hw.locked` | "Device is locked. Enter your PIN on the device to continue." |
| Cardano app not open | `launching_cardano_app` / `cardano_app_not_open` | `governance.hw.appNotOpen` | "Open the Cardano app on your {Ledger}." |
| Rejected on device | `verification_rejected` | `governance.hw.rejected` | "Transaction rejected on device. No delegation was submitted." |
| Timeout | `connecting_failed` / `timeout` | `governance.hw.timeout` | "No response from device. Disconnect and reconnect, then try again." |

## 9. Microcopy Inventory (message IDs)

All IDs below are shared so `yarn i18n:manage` produces a stable set. Status-badge ja-JP labels are locked above; broader ja-JP copy may land as slice-level placeholders and is fully reviewed as a pre-release batch gate.

| ID | en source |
|---|---|
| `governance.nav.label` | Governance |
| `governance.drepDirectory.heading` | DRep directory |
| `governance.drepDirectory.tabs.directory` | Directory |
| `governance.drepDirectory.tabs.favorites` | Favorites |
| `governance.drepDirectory.backToDirectory` | Back to directory |
| `governance.drepFavorites.empty.title` | No favorites yet |
| `governance.drepFavorites.empty.body` | DReps you favorite from the directory appear here. Favorites are stored on this device only. |
| `governance.drepFavorites.staleCaption` | This DRep is no longer in the default cohort. |
| `governance.drepDirectory.searchPlaceholder` | Search by DRep ID |
| `governance.drepDirectory.cohortBanner` | Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power. |
| `governance.drepDirectory.cohortBanner.showAll` | Show all DReps |
| `governance.drepDirectory.cohortBanner.reshuffle` | Reshuffle order |
| `governance.drepDirectory.lastUpdated` | Last updated {time} |
| `governance.drepDirectory.refresh` | Refresh |
| `governance.drepDirectory.filter.active` | Status |
| `governance.drepDirectory.filter.metadata` | Metadata |
| `governance.drepDirectory.filter.favorited` | Favorited |
| `governance.drepDirectory.empty.noResults` | No DReps match your filters. {ClearFilters} or {ShowAll}. |
| `governance.drepDirectory.empty.selfnode` | DRep directory data is unavailable on the selfnode cluster. |
| `governance.drepDirectory.empty.noSync` | Your node is still syncing. DRep data becomes available once the node reaches the tip. |
| `governance.drepDirectory.error.refresh` | Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}. |
| `governance.drepDirectory.error.rankingUnavailable` | Voting power data unavailable this refresh. Ranking-based filters disabled. |
| `governance.drepDirectory.card.status.active` | Active |
| `governance.drepDirectory.card.status.inactive` | Inactive |
| `governance.drepDirectory.card.status.expiring` | Expiring in {n} epochs |
| `governance.drepDirectory.card.status.retired` | Retired |
| `governance.drepDirectory.category.highValue` | High value |
| `governance.drepDirectory.category.primary` | Primary |
| `governance.drepDirectory.category.threshold` | Threshold |
| `governance.drepDirectory.category.nonMetadata` | Non-metadata |
| `governance.drepDirectory.category.highValue.tooltip` | Inside the default Recommended view, with verified metadata and voting power above the cohort median. |
| `governance.drepDirectory.category.primary.tooltip` | Inside the default Recommended view with verified metadata. |
| `governance.drepDirectory.category.threshold.tooltip` | Inside the default Recommended view but approaching expiry — review before delegating. |
| `governance.drepDirectory.category.nonMetadata.tooltip` | Eligible for delegation but has no verified off-chain metadata yet. |
| `governance.drepDirectory.cohortBanner.source` | Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis. |
| `governance.drepDirectory.card.votingPower` | Voting power |
| `governance.drepDirectory.card.viewDetails` | View details |
| `governance.drepDirectory.card.select` | Select for delegation |
| `governance.drepDirectory.card.favorite.add` | Add to favorites |
| `governance.drepDirectory.card.favorite.remove` | Remove from favorites |
| `governance.drepDetail.sourceLabel.onchain` | On-chain |
| `governance.drepDetail.sourceLabel.verified` | Verified off-chain content |
| `governance.drepDetail.sourceLabel.unverified` | Unverified anchor |
| `governance.drepDetail.sourceLabel.anchorUnavailable` | Anchor unavailable |
| `governance.drepDetail.copyIdToast` | DRep ID copied |
| `governance.delegationConfirm.hw.caption` | Confirm on your {device}. The DRep ID shown on the device will match the ID above. |
| `governance.hw.disconnected` | Hardware wallet disconnected. Reconnect and unlock your device to continue. |
| `governance.hw.locked` | Device is locked. Enter your PIN on the device to continue. |
| `governance.hw.appNotOpen` | Open the Cardano app on your {device}. |
| `governance.hw.rejected` | Transaction rejected on device. No delegation was submitted. |
| `governance.hw.timeout` | No response from device. Disconnect and reconnect, then try again. |

### Additional keys

The show-all sort-bias warning is the only additional ID beyond the shared set above.

| ID | en source |
|---|---|
| `governance.drepDirectory.showAll.sortBiasWarning` | Sorted by voting power. Default randomized order is designed to reduce popularity bias — consider returning to default for unbiased browsing. |

**JA-length risk:** The longer en-US strings — cohort banner (`governance.drepDirectory.cohortBanner`), refresh error banner (`governance.drepDirectory.error.refresh`), ranking-unavailable banner (`governance.drepDirectory.error.rankingUnavailable`), and the Show-all sort-bias warning — are expected to expand 30–60% in Japanese and German. Each surface must allow a minimum of 2 wrapped lines without truncating; in cards/banners the layout must reflow vertically rather than ellipsizing.

ja-JP source strings may land as slice-level placeholders; full Japanese review is a pre-release batch gate rather than a per-slice blocker.

## 10. Accessibility Floor

- **Card keyboard contract.** A DRep card is *not* itself a focusable element. The card wrapper uses `role="group"` purely to give a screen-reader a single label (`"{drepId}, {status}, voting power {amount}"`). Each card contains three real focusable child controls in this Tab order:
  1. `<button aria-pressed={isFavorite}>` — favorite toggle
  2. `<a href="…">` or `<button>` — "View details"
  3. `<button>` — "Select for delegation"

  `Enter` and `Space` activate each control with their **native** semantics (Space toggles the favorite button, Enter activates the link/buttons). No overrides, no `Shift+Enter` shortcut, no Space-on-card behavior.
- Focus ring: existing react-polymorph focus-skin tokens, no custom overrides.
- Live region: refresh state announcements (`"DRep list refreshed, 187 results"`) via `aria-live="polite"`.
- All numeric values that aren't trivially read (lovelace, voting power, expiry epochs) include an `aria-label` with human-readable form.
- Color contrast verified for both Daedalus light and dark themes in every status/source-label combination above.

## 11. DRep ID Search Semantics

Search input accepts partial and full bech32 DRep IDs across both CIP forms. Behavior:

- **Minimum prefix length: 8 characters** (after the `drep1` / CIP-129 HRP prefix). Below 8 chars the input shows the help text `"Enter at least 8 characters to search by ID"`; no IPC call is made.
- **Prefix match (≥ 8 chars, not full bech32):** returns all matches with their full DRep IDs visible in the result list. Never auto-selects, never opens detail — the user must explicitly pick a row.
- **Exact match (full bech32, valid checksum):** bypasses the result list and opens the detail view for that DRep directly.
- **Both ID forms are searched.** CIP-105 (`drep1…` legacy) and CIP-129 (new bech32) are both indexed. If the same underlying DRep credential matches via both forms, the result list **deduplicates by underlying DRep credential** (the row shows both ID forms stacked).
- **Validation before IPC.** Any input that the user submits as a full bech32 ID runs the same bech32 prefix / checksum / length checks defined in task-103 *before* `GovernanceQueryService` is called. Invalid full-form entries surface the existing "Invalid DRep ID" error and never reach the main process.
- **Verified-name search (deferred beyond v1).** v1 search matches DRep IDs only. Verified `givenName` is populated in `GovernanceStore.drepIndex` lazily per detail visit (anchor-1), so it does not cover unvisited DReps; name search is deferred until a bulk cohort anchor-prefetch phase makes names available directory-wide. ID search semantics above are the complete v1 contract.
