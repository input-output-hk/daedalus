# Slice-7 PRD: DRep Favorites — Per-Device Persistence, Toggle, Favorites View

> **Planning Status:** approved | **Date:** 2026-07-27 | **Parent Plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
> **Phase:** `slice-7` — "Slice 7 - Favorites" (riskLevel: low)
> **Tasks:** task-122 (single-task slice; dependencies task-106 and task-121 both satisfied)
> **Implementation guide:** [slice-7-implementation-guide.md](./slice-7-implementation-guide.md)

---

## Executive Summary

Slice-7 makes DRep favorites real. Task-122 wires `GovernanceStore` favorites as a
`Set<string>` of canonical CIP-129 DRep ids persisted through the existing renderer
`LocalStorageApi` → electron-store IPC path (a new `DREP-FAVORITES` key on the
`TOKEN-FAVORITES` precedent), adds the per-card favorite toggle to the directory
(native `<button aria-pressed>`, first focusable control per tokens §10), renders the
Favorited filter checkbox that slice-6 deliberately withheld (slice-6 D-3 / F-7), and
ships the designed Favorites surface as a real route — `/governance/favorites` — as a
second tab in the existing Governance section `Navigation`, rendered by the same
`DRepDirectoryPage`/`DRepDirectory` pair in a `favorites` view mode: favorites banner
line with count, favorited entries drawn from the **full membership** through the
slice-6 `favoritedOnly` predicate (never re-implemented), the `noFavorites` empty state
with a Back-to-directory CTA, and the AC-5 stale-favorite mechanism (staleCaption +
status badge + no auto-purge) built now with its real `Retired`/`doNotList` inputs
explicitly deferred to their owning phases. Zero new IPC channels, zero logging —
favorites ids reach exactly one sink: the sanctioned per-device electron-store record.

**Why now:** the locked slice order (prompt.md:147) reaches slice-7 after slice-6
closed (commit `73f983a3a`, task-121 `complete`); task-122's dependencies task-106
(`complete`) and task-121 (`complete`) are both green; and slice-6 explicitly left the
favorited contract "without UI" for this task (slice-6 D-3, findings F-7: "task-122
only feeds the set and renders the control").

---

## Problem Statement

The favorited filter predicate, the `favoriteDRepIds` context, and the
`DRepDirectory.favoriteDRepIds` prop all exist since slice-6 (`helpers.ts:164,186,215`;
`DRepDirectory.tsx:84,104`) — but nothing feeds them: no persistence, no toggle, no
Favorited control, no Favorites surface. `DRepDirectoryPage.tsx` passes neither a
favorites set nor a toggle handler. Plan invariants promise favorites that "persist
across app sessions through Electron local store and are not per-wallet or synced"
(plan.md:88, 114, 228; README.md:41, 58); the design specifies the Favorites route with
its banner, empty state, and stale-favorite treatment (drep-discovery-design.md:107-111).
Task-122 closes all of it.

---

## Per-Task Contract (interaction mode, scope, dependencies)

| Task | Interaction mode | Scope | Non-goals | Deps |
|---|---|---|---|---|
| **task-122** — Persist DRep favorites via Electron local store | `autonomous` | New `DREP-FAVORITES` electron-store key (`electron-store.types.ts` + `electron-store.config.ts`) + `LocalStorageApi` get/set/unset; `GovernanceStore` gains `@observable favoriteDRepIds: Set<string>` (reassign-only), `loadFavorites()`, `toggleFavorite()`, and a `setup()` load; `DRepCard` gains the aria-pressed favorite toggle + stale caption; `DRepDirectoryList` threads favorites; `DRepDirectoryFilters` gains the Favorited checkbox (`filter.favorited`); `DRepDirectory` gains the `favorites` view mode (favorites pipeline via the slice-6 predicate over `showAllList`); `DRepDirectoryBanner` gains the favorites line; `DRepEmptyState` gains `noFavorites`; new route `ROUTES.GOVERNANCE.FAVORITES` + `Routes.tsx` registration + Favorites tab in `containers/voting/Governance.tsx`; container wiring in `DRepDirectoryPage`; `isStaleFavorite` helper + injectable staleness seam; 9 i18n keys per locale (8 §9-inventoried + 1 minted); Jest across store/component/banner/container incl. an app-restart simulation; 4 new Storybook stories + connected-flow favorites tab | No detail-view favorite toggle (directory-only per AC-2); no re-implementation of the favorited predicate (AC-2); no `Retired` status value, no `DRepStatusBadge`/`DRepCategoryBadge`/`governance.types.ts` change (invariant #14 — R-3); no `doNotList` handling (anchor-2, task-153); no auto-purge of favorites ever (AC-5); no favorites analytics event of any kind (R-4); no new IPC channel or `source/main/` change beyond nothing at all (R-1 — the generic electron-store handler already serves the new key); no illustration asset in the empty state (R-2); no renaming of the pre-existing `governance.tabs.directory` key (R-2 drift, recorded); no favorites search/filter/sort controls inside the favorites view (R-2); no removal of any `!!!` marker | task-106 ✔ `complete`, task-121 ✔ `complete` |

task-122 is not in the locked non-autonomous set (task-125, task-166 remainder,
task-158, release-end `!!!` review — prompt.md:176-182). Planning surfaced **no
blocking decisions**: the five orchestrator reconciliations R-1…R-5 and planner
resolutions P-1…P-10 below close every open question.

---

## Required Reconciliations R-1…R-5 (resolved; grounded in live code)

- **R-1 — `targetPath: source/main/` conflict → renderer LocalStorageApi reuse
  (prefer-live-repo).** The tasks JSON declares `targetPath: "source/main/"` for
  task-122, but the live, shipped per-device persistence pattern is renderer-side:
  `LocalStorageApi` (`source/renderer/app/api/utils/localStorage.ts:278-295`,
  `TOKEN_FAVORITES` get/toggle/unset) over the existing `electronStoreConversation`
  IPC, with key registration in `source/common/config/electron-store.config.ts:9-38`
  and `source/common/types/electron-store.types.ts:2-31`, consumed by stores via
  `this.api.localStorage` (`AssetsStore.ts:19` favorites request, `:196`
  toggle). The main-process handler (`source/main/ipc/electronStoreConversation.ts:31-52`)
  is fully generic over `StorageKey` — **no `source/main/` file changes at all**.
  Decision: implement via renderer reuse; treat the JSON `targetPath` as loose
  metadata; conflict recorded here and for the slice findings note per the
  prefer-live-repo rule (prompt.md:39-41). *Residual:* the tracker's `targetPath`
  stays stale (documented; Scribe may correct it at close).
- **R-2 — Favorites surface: real `/governance/favorites` route + Governance-nav tab +
  a `favorites` view mode of the existing directory pair.** The design contract is a
  route (`drep-discovery-design.md:107`: "Favorites route `/governance/favorites`",
  same card layout, replaced banner, empty state with CTA back to Directory) and the
  §9 tab tokens exist (`tabs.directory` :161, `tabs.favorites` :162,
  `backToDirectory` :163) — while `routes-config.ts:39-43` has no favorites route and
  AC-2 pins the toggle "into the directory". Decisive live seam: the Governance
  section wrapper `containers/voting/Governance.tsx:41-47` **already renders a
  `Navigation` tab bar** with a single "Directory" item — the designed Directory /
  Favorites tabs are one nav item away, with `handleNavItemClick` already pushing
  routes. Smallest truthful surface satisfying AC-1…AC-5 **and** the design: add
  `ROUTES.GOVERNANCE.FAVORITES = '/governance/favorites'`, register it on the
  existing `DRepDirectoryPage`, add the Favorites nav item, and give `DRepDirectory`
  a `view: 'directory' | 'favorites'` prop — favorites membership computed **only**
  through the slice-6 `filterDReps` `favoritedOnly` predicate over `showAllList`
  (full membership, so favorited non-cohort/top-35/inactive entries stay visible;
  AC-2 honored: one predicate, one place). Recorded reconciliations: (a) the shipped
  Directory tab uses message id `governance.tabs.directory`, not §9's
  `governance.drepDirectory.tabs.directory` — pre-existing drift; the new Favorites
  tab uses the exact §9 id `governance.drepDirectory.tabs.favorites` per the locked
  copy decision (R-5) and the old key is NOT renamed (out of scope, locale churn);
  (b) the designed "prominent illustration" in the empty state has no asset in the
  repo — ships as title + body + CTA link, consistent with every existing
  `DRepEmptyState` variant; (c) the favorites view renders no search/filter/sort
  controls (design specifies only cards + replaced banner + empty state); the
  Favorited *filter checkbox* lives in the Directory view's filter row, completing
  slice-6 D-3; (d) a favorite whose registration disappears from the chain entirely
  cannot render a card (no entry data exists) — the persisted id is still never
  purged; accepted edge, recorded for the findings note.
- **R-3 — Stale favorites (AC-5): mechanism now, real signals deferred.** `Retired`
  has no on-chain signal yet (invariant #14; `DRepStatus = 'active' | 'inactive'`,
  `governance.types.ts:35`) and `doNotList` lands in anchor-2 (task-153). Built now:
  the `governance.drepFavorites.staleCaption` message id (exact §9 copy), a pure
  `isStaleFavorite(entry)` helper in `drep-directory/helpers.ts` that checks the
  entry status against a stale-status set containing `'retired'` — type-safe today
  (`ReadonlySet<string>.has` accepts the narrower union) and **false for every
  current entry** — plus an injectable `isStaleFavoriteEntry` prop seam on
  `DRepDirectory`/`DRepDirectoryList` (production default: the helper) so Jest and
  Storybook can render a synthetic stale favorite without inventing a fake status
  value or casting. Stale cards render their **existing** status badge (the
  `Retired` / `Excluded from default cohort` badge variants remain unowned drift —
  slice-6 F-6 — and are NOT built here) plus the inline caption. No auto-purge
  anywhere: `loadFavorites`/`toggleFavorite` never drop ids on status change, and
  the favorites view renders whatever favorited entries exist. **Recorded:**
  verification against real `Retired`/`doNotList` states is deferred to the phases
  that produce those signals (invariant #14 / anchor-2); when they land, only
  `isStaleFavorite` (and the badge module, under its future owner) changes.
- **R-4 — Sanitization reconciliation (invariant #2 vs AC-1/AC-4/invariant #12).**
  Prompt invariant #2's wording sweeps "electron-store payload" into the floor, but
  AC-1 requires persisting favorites per-device, invariant #12 sanctions exactly
  that, and the shipped `TOKEN-FAVORITES` record plus the task-168 DRep-state
  snapshot (the documented-exception precedent named by invariant #2 itself) settle
  the pattern. The spy suite was read in full
  (`tests/jest/security/governance-sanitization.spec.ts`, 23 `it` cases): it spies
  `filterLogData`, the renderer `logger`, `analytics.sendEvent`, and Matomo URL
  masking — it intercepts **no electron-store or `electronStoreConversation` call**,
  so the favorites write cannot trip it and the suite file stays byte-identical at
  23/23. Decision: record the `DREP-FAVORITES` record as the **second documented
  exception** — public on-chain identifiers, per-device (and per-network: the main
  handler prefixes every key with `environment.network`,
  `electronStoreConversation.ts:19,34`), deliberately outside `filterLogData`, and
  never routed through logger or analytics. Enforced in-slice: every new favorites
  code path makes **zero** `logger.*`/analytics calls (load/persist failures are
  silently swallowed with a comment stating why — logging them would carry ids), and
  unlike `AssetsStore._onToggleFavorite` (`AssetsStore.ts:203-206`) **no analytics
  event fires on toggle** — no AC asks for one and any event invites payload creep.
  Jest pins: no logger call during favorites flows, and the stringified logger mock
  calls never contain a favorite id.
- **R-5 — i18n: exact §9 ids and copy; one minted banner key.** The 8 inventoried
  keys ship with the exact §9 ids/copy (shared-design-tokens.md:161-166, 175,
  198-199): `governance.drepDirectory.tabs.favorites` (Favorites),
  `governance.drepDirectory.backToDirectory` (Back to directory),
  `governance.drepFavorites.empty.title` (No favorites yet),
  `governance.drepFavorites.empty.body` ("DReps you favorite from the directory
  appear here. Favorites are stored on this device only."),
  `governance.drepFavorites.staleCaption` ("This DRep is no longer in the default
  cohort."), `governance.drepDirectory.filter.favorited` (Favorited),
  `governance.drepDirectory.card.favorite.add` (Add to favorites),
  `governance.drepDirectory.card.favorite.remove` (Remove from favorites). The
  favorites banner copy is design-specified (`drep-discovery-design.md:109`:
  "{n} DReps you've favorited. Favorites are stored on this device only." — plain
  ASCII apostrophe as in the design source; a lone ICU apostrophe not followed by
  `{`/`}` renders literally, so it is safe unescaped) but has **no §9 id**: minted as
  **`governance.drepFavorites.banner`** in the `drepFavorites.*` family (the D-4 /
  F-5 minting precedent). All 9 keys per locale keep the leading `!!!` (invariant
  #11); ja-JP values are preliminary placeholders. *Residual:* §9 needs the
  `drepFavorites.banner` row at a future docs pass (adds to the F-5 inventory gap).

### Planner decisions P-1…P-10

- **P-1 (store shape):** `@observable favoriteDRepIds: Set<string> = new Set()` on
  the `drepIndex` Map precedent (`GovernanceStore.ts:100` — MobX 5 wraps it; the
  contract is **reassign a fresh `Set`, never mutate in place**, so computeds,
  `useMemo` dep arrays, and mobx-react observers all see a new reference).
  `loadFavorites()` reads via `this.api.localStorage.getDRepFavorites()`, keeps only
  string entries (malformed records degrade to fewer/zero favorites, never a throw),
  and is kicked from `setup()` (fire-and-forget; it catches internally).
  `toggleFavorite(drepId)` computes the next set, assigns it, then persists the full
  array with a silent `.catch` — persistence failure keeps in-memory state and the
  next successful write stores everything.
- **P-2 (persistence record):** a JSON array of canonical CIP-129 drepId strings
  under `DREP-FAVORITES`. The main handler's network prefix makes the record
  per-device **and per-network** (mainnet/preprod favorites never mix) — recorded as
  desirable behavior inherited from the shared handler, not new logic.
- **P-3 (toggle control):** native `<button type="button" aria-pressed={isFavorite}>`
  rendered as the **first** focusable control in the card's top row (tokens §10 tab
  order: favorite → view details → select). Star glyphs (★ favorited / ☆ not),
  aria-hidden, with `aria-label`/`title` from `card.favorite.add`/`.remove`. Native
  element per the slice-5 P-7 / slice-6 P-15 precedent — deterministic under jsdom,
  no new polymorph dependency.
- **P-4 (Favorited filter checkbox):** always visible in `DRepDirectoryFilters`
  (react-polymorph `Checkbox`, the `excludeTop35` pattern), driving the existing
  `favoritedOnly` facet. With zero favorites it yields the `noResults` empty state,
  whose Clear-filters action is the escape hatch — the slice-6 "dead UI" objection
  dissolves now that favorites can be non-empty.
- **P-5 (banner favorites mode):** `DRepDirectoryBanner` gains optional
  `isFavoritesView` (default false) + `favoritesCount` (default 0); favorites mode
  suppresses the cohort claim, Reshuffle, BMVG citation, and the filtered line, and
  renders the minted `drepFavorites.banner` line. Title, Refresh, and last-updated
  stay (the favorites view shows live directory data that still refreshes).
  `DRepDirectoryBanner.stories.tsx` keeps compiling untouched (optional props — the
  slice-6 Step-12 precedent).
- **P-6 (empty state):** `DRepEmptyState` union gains `'noFavorites'` (title + body
  + `backToDirectory` Link → `onBackToDirectory` callback; container navigates to
  `ROUTES.GOVERNANCE.DREPS` preserving `pickDelegationFormReturnState` state).
- **P-7 (view-state interplay):** the directory view's search/filter/sort state stays
  component-local and untouched when the favorites route is active; the reactive
  exact-match-open effect is explicitly gated off in the favorites view. The
  favorites pipeline is a separate `useMemo` (`filterDReps` over `showAllList` with
  `favoritedOnly: true` and the default remaining facets) — sorting stays the seeded
  session order, consistent with the no-ranking-bias posture.
- **P-8 (container/routing):** `DRepDirectoryPage` derives the view from
  `location.pathname` (both routes render the same container; `withRouter` is
  already applied); the Favorites tab and Back-to-directory CTA both travel through
  `history.push` with the inherited delegation-form return state, mirroring
  `handleViewDetails`. `TrackedRoute` analytics for `/governance/favorites` contains
  no DRep id (nothing to mask; `maskAnalyticsRoute` untouched).
- **P-9 (Storybook):** extend `DRepDirectory.stories.tsx` only (global locale
  toggle; no local IntlProvider; no per-locale variants). New stories: interactive
  **Favorite toggle** (`withState`-backed set so the star really flips), **Favorites
  view** (incl. a favorited entry absent from the cohort list), **Favorites view —
  empty**, and **Favorites view — stale favorite** (synthetic staleness via the
  injected `isStaleFavoriteEntry` predicate — no fake status values). The Connected
  flow gains the Favorites nav item + a shared `withState` favorites set so the full
  tab journey works in one story. Stories are verified via `tsc`/`eslint` only
  (Storybook cannot launch in this devcontainer).
- **P-10 (test surfaces):** Jest additions live in the existing five suites
  (`GovernanceStore.spec.ts`, `helpers.spec.ts`, `DRepDirectory.spec.tsx`,
  `DRepDirectoryBanner.spec.tsx`, `DRepDirectoryPage.spec.tsx`) plus two mock fields
  in `VotingGovernancePage.spec.tsx`; the app-restart persistence pin (AC-3) is a
  two-store simulation over one shared in-memory backing record. The sanitization
  suite re-runs untouched at 23/23.

---

## User Stories

### US-7.1 — Favorite from the directory, keep it across restarts
**As a** Daedalus user browsing the directory,
**I want** to favorite/unfavorite DReps with one click and find them still favorited after restarting the app,
**So that** I can build a per-device shortlist without any sync or wallet coupling.

**Acceptance:** star toggle on every card (aria-pressed, first in tab order);
toggling immediately updates every surface reading the set; the set survives an app
restart via `DREP-FAVORITES` (Jest restart simulation); favorites never leave the
device (no wallet payload, no sync, no analytics, no logs).

### US-7.2 — See my favorites as their own page
**As a** user with favorites,
**I want** a Favorites tab at `/governance/favorites`,
**So that** I see exactly my favorited DReps — even ones outside the default cohort — with an honest banner.

**Acceptance:** Favorites nav tab; banner reads the minted per-device line with a live
count; favorited top-35/inactive/non-cohort entries render (full-membership base
through the slice-6 predicate); empty state shows title/body/Back-to-directory CTA.

### US-7.3 — Filter the directory by favorited
**As a** user in the Directory view,
**I want** the Favorited filter,
**So that** I can narrow any directory view to my favorites without leaving it.

**Acceptance:** Favorited checkbox drives the existing `favoritedOnly` facet
(predicate unchanged, `helpers.ts:215`); banner switches to filtered mode; zero
matches fall to `noResults` with working escape actions.

### US-7.4 — Never lose a stale favorite silently
**As a** user whose favorited DRep later retires or opts out of listing,
**I want** it kept in my favorites with its status badge and a caption,
**So that** I decide myself when to unfavorite.

**Acceptance:** stale entries render badge + `staleCaption`; nothing is auto-purged;
synthetic stale state covered by Jest + a Storybook story; real signals deferred (R-3).

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `DREP-FAVORITES` key registered in `electron-store.types.ts` + `electron-store.config.ts`; `LocalStorageApi.getDRepFavorites`/`setDRepFavorites`/`unsetDRepFavorites` on the TOKEN-FAVORITES precedent; no main-process change | task-122 |
| FR-2 | `GovernanceStore`: `favoriteDRepIds` observable (reassign-only), `loadFavorites()` (setup-kicked, silent-failure, string-sanitized), `toggleFavorite(drepId)` (toggle + persist-full-array + silent catch); zero logger/analytics calls | task-122 |
| FR-3 | `DRepCard`: favorite toggle (P-3) + optional stale caption; `DRepDirectoryList` threads `favoriteDRepIds`, `onToggleFavorite`, `isFavoritesView`, `isStaleFavoriteEntry` (default `isStaleFavorite`) | task-122 |
| FR-4 | `DRepDirectoryFilters`: Favorited checkbox bound to `filters.favoritedOnly` (P-4) | task-122 |
| FR-5 | `DRepDirectory`: `view` prop + favorites pipeline (`filterDReps` over `showAllList`, `favoritedOnly: true`), favorites branch (list / `noFavorites` empty state), banner favorites props, exact-open effect gated off in favorites view (P-7) | task-122 |
| FR-6 | `DRepDirectoryBanner` favorites mode (P-5) with minted `governance.drepFavorites.banner` | task-122 |
| FR-7 | `DRepEmptyState` `noFavorites` variant (P-6) | task-122 |
| FR-8 | Route `ROUTES.GOVERNANCE.FAVORITES` + `Routes.tsx` TrackedRoute + Favorites nav item in `Governance.tsx`; `DRepDirectoryPage` wires set/toggle/view/back-CTA (P-8) | task-122 |
| FR-9 | `isStaleFavorite` helper (R-3) + co-located spec coverage | task-122 |
| FR-10 | 9 `!!!` keys per locale (R-5); `yarn i18n:manage` run; tool-managed JSON diffs ride with the commit | task-122 |
| FR-11 | Jest per acceptance rule (P-10) incl. the restart simulation, non-cohort favorites reachability, stale rendering via injection, and the no-logging pins; Storybook per P-9 | task-122 |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | react-intl 2.9 API only (`injectIntl`/`intlShape`/`defineMessages`/`FormattedMessage`); no react-intl hooks |
| NFR-2 | Every new en-US **and** ja-JP string keeps the leading `!!!`; none removed |
| NFR-3 | Storybook: global locale toggle only; no local IntlProvider, no per-locale story variants; stories verified via tsc/eslint (no launch in devcontainer) |
| NFR-4 | New Jest object-argument assertions use `expect.objectContaining` (prettier 2.1.2 oscillation guard); single-array-argument `toHaveBeenCalledWith([id])` is safe and allowed |
| NFR-5 | `node_modules/.bin/tsc --noEmit` zero errors; scoped `node_modules/.bin/eslint` clean; no `.scss.d.ts` committed |
| NFR-6 | Devcontainer discipline: no nix; `npx` broken — every tool via `node_modules/.bin/<tool>` or `yarn <tool>`; scoped prettier on changed `.ts/.tsx/.scss` only (never JSON/locale/defaultMessages/translations/`.snap`); no push, no PR, no gh |
| NFR-7 | Comments only where logic isn't self-evident, 1–3 plain lines, no task IDs/labels/history |
| NFR-8 | Exactly one subject-only Conventional Commits commit: `feat(gov): task-122 …`; no body, no trailers |

---

## Architecture: Data Flow (slice-7 delta)

```
electron-store (per device, per network)          renderer
  DREP-FAVORITES: string[]  ◄──set──┐
        │ get                       │
        ▼                           │
LocalStorageApi.getDRepFavorites    │ setDRepFavorites([...next])
        │                           │
GovernanceStore.setup() ─► loadFavorites() ─► @observable favoriteDRepIds: Set<string>
                                             toggleFavorite(drepId) ─► reassign + persist
        │
DRepDirectoryPage ── favoriteDRepIds · onToggleFavorite · view(pathname) · onBackToDirectory ──► DRepDirectory
        │                       view = 'directory'                        view = 'favorites'
        │     filterDReps(base, filters{favoritedOnly}, ctx)   favoritesEntries =
        │     (Favorited checkbox — slice-6 predicate)           filterDReps(showAllList,
        │                                                        {favoritedOnly: true}, ctx)
        ├── DRepDirectoryBanner (cohort/filtered ⟷ "{n} DReps you've favorited …")
        ├── DRepDirectoryList ── DRepCard (★/☆ aria-pressed toggle · staleCaption)
        └── DRepEmptyState noFavorites ({Back to directory})
Governance.tsx Navigation: [Directory | Favorites] ↔ routes /governance/dreps · /governance/favorites
```

One predicate (`helpers.ts:215`), one persistence key, zero new IPC channels, zero
logging. `VotingStore` never reads `GovernanceStore` (invariant #4 — untouched).

---

## What Slice-7 Deliberately Does NOT Include

- ❌ Any `source/main/` change (R-1 — the generic electron-store handler already serves the key)
- ❌ Favorited-predicate re-implementation or any `filterDReps` semantics change (AC-2)
- ❌ `Retired` status value, `doNotList`, or any `DRepStatusBadge`/`governance.types.ts` change (R-3; invariant #14)
- ❌ The unowned `Retired` / `Excluded from default cohort` badge variants (slice-6 F-6 drift, still unowned)
- ❌ Automatic purge of favorites under any condition (AC-5)
- ❌ Favorites analytics events or any logger call carrying favorites data (R-4)
- ❌ Favorite toggle on the DRep detail view; favorites sync; per-wallet favorites (invariant #12)
- ❌ Renaming `governance.tabs.directory`; empty-state illustration asset (R-2)
- ❌ Removing any `!!!` marker (release-end user-owned review)

---

## Docs / Designs / Research / Workflows / Skills Consulted

- **Orchestration contract:** `prompt.md` (doc structure :45-89, invariants :93-138,
  slice order :147, planning rules :160-172, non-autonomous set :176-182,
  convergence :214-219)
- **Tracker:** `governance-drep-discovery-plan-tasks.json` — slice-7 / task-122
  (description + 5 acceptance criteria verbatim, `dependencies: [task-106, task-121]`
  both `complete`, conflicting `targetPath` — R-1); slice-6/task-121 entry (D-3
  ownership grounding); anchor-2/task-153 (doNotList owner)
- **Plan/README:** plan.md :151 (Favorites scope Key Decision), :88/:114 (goals),
  :226-228 (store ownership + persistence), :234-235 (directory columns/filters),
  :305 (favorite-toggle stories); README.md :41/:58 (per-device, no restore
  carry-over), :48 (single Governance nav entry)
- **Designs:** `drep-discovery-design.md` :107-109 (Favorites route, banner copy,
  empty state), :111 (stale favorites — binding); `shared-design-tokens.md` §9
  :161-166/:175/:198-199 (the 8 inventoried ids + copy), §10 :226-231 (card tab
  order, aria-pressed), §1 :14-15/:20 (Retired deferred; top-35 badge — context for
  R-3), §5 :86 (filtered banner precedent for the replaced-banner pattern)
- **Precedent docs:** `slice-6-PRD.md` (D-3/D-4/P-15/P-16 precedents, doc shape),
  `slice-6-implementation-guide.md` (cross-cutting block + verification style),
  `slice-6-code-review.md` (transcript format)
- **Research:** `research/slice-6-findings.md` (F-5 §9 inventory gap, F-6 unowned
  badges, F-7 favorited contract without UI, F-9 jsdom/bech32 harness fixes);
  `research/slice-4-findings.md` (broken npx); `research/ux-refinement-findings.md`
  (i18n:manage OK, prettier drift)
- **Live seams (verified 2026-07-27 at HEAD `73f983a3a`):** `helpers.ts` :164/:170/
  :175/:186/:215/:222-228; `DRepDirectory.tsx` :84/:104/:139-166/:170-179/:247-258/
  :260-327/:331-341; `DRepDirectoryList.tsx` :30-44/:74-82; `DRepCard.tsx` :42-48/
  :82-113; `DRepDirectoryFilters.tsx` :15-16/:65-66/:190-201; `DRepDirectoryBanner.tsx`
  :51-64/:97-118; `DRepEmptyState.tsx` :37-46; `GovernanceStore.ts` :96-120/:203-211/
  :315-324; `DRepDirectoryPage.tsx` :58-73/:82-100; `Governance.tsx` (containers/voting)
  :11-17/:30-37/:41-51; `routes-config.ts` :39-43; `Routes.tsx` :227-246;
  `localStorage.ts` :52-90/:278-295; `electron-store.config.ts` :9-38;
  `electron-store.types.ts` :2-31; `electronStoreConversation.ts` (main) :19/:31-52;
  `AssetsStore.ts` :17-20/:187-207; `governance-sanitization.spec.ts` (23 cases, no
  storage spy); spec harnesses (`GovernanceStore.spec.ts` :17-64,
  `DRepDirectory.spec.tsx` :85-144, `DRepDirectoryPage.spec.tsx` :37-82,
  `DRepDirectoryBanner.spec.tsx` :9-37, `VotingGovernancePage.spec.tsx` :87-101);
  `DRepDirectory.stories.tsx` :135-159/:236-246
- **Workflows/skills:** `.agent/workflows/frontend.md`, `.agent/workflows/test.md`,
  `.agent/workflows/storybook.md`; skills `i18n-messaging`, `storybook-creation`,
  `git-commit-formatter` (subject-only), `evidence-rules`. NOT needed:
  `.agent/workflows/ipc.md` (no new IPC), `e2e-test-creation`,
  `bech32-encoding-decoding`, `theme-management`

---

## Locked Invariants Touched

| # | Invariant | How slice-7 honors it |
|---|---|---|
| 2 | Sanitization floor | Zero `logger.*`/analytics calls in every new code path; load/persist failures swallow silently (comments state why); no favorites analytics event; the `DREP-FAVORITES` electron-store record is the documented exception (R-4) sanctioned by invariant #12 and the TOKEN-FAVORITES/task-168 precedents; the 23-test spy suite is never edited and re-runs 23/23 |
| 4 | Store boundary | Favorites live in `GovernanceStore` only; `VotingStore` untouched; delegation handoff (`location.state`) byte-identical |
| 7 | Default cohort binding | `displayedDRepList`, `defaultCohort`, `top35DRepIds`, `showAllList` untouched; the favorites view is a separate derived pipeline; the Favorited filter is user-initiated |
| 8 | Badges informational | No badge module imported by any filtering code; `isStaleFavorite` reads `entry.status` only and drives a caption, never ordering/filtering; badges gain no callbacks |
| 11 | Preliminary copy | All 9 new keys per locale `!!!`-prefixed; no marker stripped; `yarn i18n:manage` after copy changes |
| 12 | Favorites per-device | The core of the task: `Set<string>` via Electron local store; not per-wallet, not synced; empty-state body owns the per-device expectation; network-prefixed key recorded (P-2) |
| 14 | Status grounding | `DRepStatus` union untouched; no `retired` value shipped; stale mechanism is forward-compat only (R-3) |

Not touched: #1 (no new data source), #3 (no anchor fetch), #5 (no voting-power
arithmetic added), #6 (zero CLI/IPC changes), #9 (no delegation defaults), #10
(identity display unchanged), #13 (sentinels can never enter `drepList`, hence never
the favorites pipeline).

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| task-106 (bare GovernanceStore) | `complete` (tracker; `GovernanceStore.ts:96`) |
| task-121 (filter framework incl. `favoritedOnly` predicate + `favoriteDRepIds` plumbing) | `complete` (tracker; `helpers.ts:215`, `DRepDirectory.tsx:84,104,148-151`; commit `73f983a3a`) |
| `LocalStorageApi` + electron-store IPC | live (`localStorage.ts:52-90`; main handler generic over `StorageKey`) |
| `Api.localStorage` on stores | live (`api/index.ts:6,18`; `Store` base exposes `this.api`) |
| Governance section Navigation (tab bar) | live (`containers/voting/Governance.tsx:41-58`) |
| `pickDelegationFormReturnState` | live (`DRepDirectoryPage.tsx:13,61,71`) |
| Sanitization floor suite baseline | 23 `it(` cases green (grep-verified; no storage spy) |
| Jest/story harnesses | `GovernanceStore.spec.ts`, `DRepDirectory.spec.tsx`, `DRepDirectoryBanner.spec.tsx`, `DRepDirectoryPage.spec.tsx`, `VotingGovernancePage.spec.tsx`, `DRepDirectory.stories.tsx` |

---

## Risks / Open Questions

| Risk | Mitigation |
|------|-----------|
| Electron-store read fails or holds garbage → favorites lost silently | `loadFavorites` sanitizes to strings and degrades to the empty set; the record is never overwritten until the user toggles; Jest covers malformed + rejecting reads |
| Toggle persists while a second toggle is in flight | Every persist writes the **full current array** (last write wins with the complete set); no incremental deltas |
| MobX Set observability quirks | Reassign-only contract (P-1); Jest pins reference change on toggle |
| Favorites view shows nothing for a favorited-but-deregistered DRep | Accepted edge (R-2d): no entry data exists to render; the persisted id survives; recorded for findings |
| `aria-pressed` button styling drifts between themes | Minimal SCSS using existing theme variables; stories cover both states via the toggle story |
| Stale mechanism never fires in production today | Intentional (R-3): `isStaleFavorite` is false for every current status; injection seam keeps it testable; deferral recorded |
| Route addition breaks nav highlighting or analytics | `Governance.tsx` `activeItem` matching is prefix-based and unambiguous between `/governance/dreps` and `/governance/favorites`; `TrackedRoute` page title carries no id |
| ja-JP copy quality | Preliminary `!!!` values; release-end user-owned review (invariant #11) |
| §9 inventory gains another unlisted key (`drepFavorites.banner`) | Minted per the D-4/F-5 precedent; recorded here + guide key table + findings note |

**Open questions:** none — R-1…R-5 and P-1…P-10 resolve all judgment calls; no
interactive checkpoint exists in this slice.

---

## Definition of Done

- [ ] task-122 acceptance criteria met; focused Jest green; code review clean; one
      subject-only commit (`feat(gov): task-122 …`); tracker JSON synchronized
      (`status`, `statusReason`, `evidence`, `updatedAt`)
- [ ] `node_modules/.bin/tsc --noEmit` zero errors; scoped eslint clean
- [ ] Sanitization floor 23/23; suite file byte-identical; zero logger/analytics
      calls in the favorites diff (grep the diff for `logger.` and `analytics`)
- [ ] AC-1 pinned: restart simulation restores favorites from the shared backing
      record; malformed/rejecting reads degrade silently
- [ ] AC-2 pinned: toggle in the directory fires `toggleFavorite` with the row id;
      Favorited checkbox drives the slice-6 predicate; no predicate re-implementation
      (`filterDReps` call sites only)
- [ ] AC-3 pinned: toggle + persistence Jest across store/component/container;
      Storybook toggle + favorites stories compile; all 9 keys per locale
      `!!!`-prefixed; `yarn i18n:manage` clean
- [ ] AC-5 pinned: favorites view renders synthetic stale entries with badge +
      staleCaption via the injection seam; no auto-purge (persisted ids survive
      status changes by construction); stale Storybook story compiles; real
      `Retired`/`doNotList` verification recorded as deferred
- [ ] Favorites reachability pinned: a favorited entry outside `drepList` (cohort)
      renders in the favorites view (full-membership base)
- [ ] `research/slice-7-findings.md` written at slice close (R-1 targetPath conflict,
      R-2 surface reconciliation + accepted edge, R-3 deferral, R-4 documented
      exception, minted banner key)
- [ ] Final outcome below filled at slice close

---

## Final Outcome

Slice-7 is complete: its single task shipped, passed code review on pass 1 with
zero blockers, and is `complete` in the tracker (not `verified` — AC-5's real
`Retired`/`doNotList` stale-state rendering cannot be exercised until the phases
that deliver those signals land (invariant #14 / anchor-2 task-153), and the
end-to-end journey proof arrives with slice-8's release-verification acceptance).
At close the full diff sits on `wt/slice-7-task-122` (base `73f983a3a`); the
single subject-only commit (`feat(gov): task-122 …`) is the close-out step.

**task-122 — Persist DRep favorites via Electron local store** — **complete**.
Zero `source/main/` changes (R-1: the generic electron-store handler serves the
new key): `DREP-FAVORITES` registered in the shared config/types
(`electron-store.config.ts:18`, `electron-store.types.ts:13`) with
`LocalStorageApi.getDRepFavorites`/`setDRepFavorites`/`unsetDRepFavorites`
(`localStorage.ts:296-301`); `GovernanceStore.favoriteDRepIds` as a reassign-only
`Set<string>` with setup-kicked `loadFavorites()` and `toggleFavorite()`
persisting the full array, both silently failure-tolerant with zero
logger/analytics calls (`GovernanceStore.ts:127,333-364`). The card gains the
aria-pressed star toggle first in the tab order plus the stale caption
(`DRepCard.tsx:107-125`); the Favorited checkbox drives the slice-6
`favoritedOnly` facet (`DRepDirectoryFilters.tsx:199-202` — predicate untouched
at `helpers.ts:215`); the real `/governance/favorites` route + Favorites nav tab
render the same `DRepDirectoryPage`/`DRepDirectory` pair in a `favorites` view
mode whose membership is `filterDReps` over full `showAllList` membership
(`DRepDirectory.tsx:178-185`), with the minted per-device banner line + live
count, the `noFavorites` empty state with Back-to-directory CTA, and the
forward-compat stale mechanism (`isStaleFavorite` `helpers.ts:284-288` +
injectable seam, no auto-purge). 9 `!!!`-prefixed contract keys per locale (8
locale lines added — `backToDirectory` pre-existed) via `yarn i18n:manage`; 4
new Storybook stories + the favorites-aware Connected flow under the global
locale toggle.

**Verification executed at slice close** (2026-07-27, code-review pass 1, all
via `node_modules/.bin/<tool>` — NFR-6): `tsc --noEmit` zero errors. Focused
Jest all green — **130/130** across the six slice suites (helpers,
GovernanceStore, DRepDirectory, DRepDirectoryBanner, DRepDirectoryPage,
VotingGovernancePage), including the two-store app-restart simulation (AC-1),
toggle + full-array persistence + reference-replacement pins,
malformed/rejecting-read degradation, the no-logging pins (AC-4), non-cohort
favorites reachability (AC-2), the injected stale predicate (AC-5), and the
ja-JP empty-state render (AC-3). **Sanitization floor 23/23** with the suite
file untouched by the slice; diff grep shows zero production logger/analytics
calls. Scoped eslint 0 errors; prettier clean on touched files except the
pre-existing `Governance.tsx` parse failure (line-4 inline type import, present
at HEAD — flagged for the pre-merge `nix fmt` outside the devcontainer).

**Findings.** Durable slice decisions and gotchas (F-1 targetPath conflict →
renderer reuse + per-network record, F-2 favorites-surface reconciliations +
accepted deregistered-favorite edge, F-3 stale-favorite mechanism-now /
verification-deferred, F-4 the second documented sanitization exception +
logging-free discipline, F-5 minted `drepFavorites.banner` key + duplicated
`backToDirectory` descriptor) live in
[research/slice-7-findings.md](../research/slice-7-findings.md).

---

## References

- Parent plan: [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
- Implementation guide: [slice-7-implementation-guide.md](./slice-7-implementation-guide.md)
- Design: [drep-discovery-design.md](../designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](../designs/shared-design-tokens.md)
- Research: [slice-6-findings.md](../research/slice-6-findings.md)
