# Slice-7 Findings — DRep Favorites (Persistence, Toggle, Favorites View)

> Durable findings from slice-7 (2026-07-27). Facts only; grounding anchors verified
> against the `wt/slice-7-task-122` working tree at base `73f983a3a` (task-122 diff
> complete and code-review approved; the single subject-only task commit is the
> close-out step).

---

## F-1 (R-1, task-122) — The tracker's `targetPath: source/main/` conflicted with the live persistence pattern; resolved to renderer `LocalStorageApi` reuse with zero main-process changes

The tasks JSON declared `targetPath: "source/main/"` for task-122, but the shipped
per-device persistence pattern is renderer-side and the slice touches no
`source/main/` file at all. The main handler is fully generic over `StorageKey`, so
registering the key is the entire backend: `'DREP-FAVORITES'` in
`electron-store.types.ts:13` and `DREP_FAVORITES` in `electron-store.config.ts:18`,
served by the existing `electronStoreConversation` IPC. `LocalStorageApi` gains
`getDRepFavorites`/`setDRepFavorites`/`unsetDRepFavorites` on the TOKEN-FAVORITES
precedent (`localStorage.ts:296-301`), consumed by the store via
`this.api.localStorage` (`GovernanceStore.ts:335,361` — the AssetsStore precedent).
A useful inherited behavior: the handler prefixes every key with
`environment.network` (`electronStoreConversation.ts:19,34`), so the record is
per-device **and per-network** — mainnet and preprod favorites never mix, with no
new logic. Resolution followed the prefer-live-repo rule; the tracker's stale
`targetPath` is documented in the task-122 `statusReason` rather than rewritten.

## F-2 (R-2, task-122) — The favorites surface is a real route + nav tab + view mode of the existing directory pair, with four recorded reconciliations against the design

The design's Favorites contract (`drep-discovery-design.md:107-111`) shipped as the
smallest truthful surface: `ROUTES.GOVERNANCE.FAVORITES = '/governance/favorites'`
(`routes-config.ts:43`), a `TrackedRoute` on the same `DRepDirectoryPage`
(`Routes.tsx:243`), a second nav item in the **existing** Governance-section
`Navigation` (`containers/voting/Governance.tsx:52-55` — the tab bar predates this
slice), and a `view: 'directory' | 'favorites'` prop on `DRepDirectory`
(`DRepDirectory.tsx:85,109,139`). Favorites membership is computed exclusively
through the slice-6 `filterDReps` `favoritedOnly` predicate over `showAllList`
(`DRepDirectory.tsx:178-185`), so favorited non-cohort/top-35/inactive entries stay
visible (Jest-pinned, `DRepDirectory.spec.tsx:753`); the predicate was never
re-implemented (AC-2). Recorded reconciliations: (a) the pre-existing Directory tab
keeps message id `governance.tabs.directory` while the new tab uses the exact §9 id
`governance.drepDirectory.tabs.favorites` (`Governance.tsx:18`) — the old key is
NOT renamed, remaining inventory drift; (b) the designed empty-state illustration
has no asset in the repo — ships as title + body + Back-to-directory CTA
(`DRepEmptyState.tsx:105-118`); (c) the favorites view renders no
search/filter/sort controls (`DRepDirectory.spec.tsx:769`) — the Favorited
*checkbox* lives in the Directory view's filter row
(`DRepDirectoryFilters.tsx:199-202`), completing slice-6 D-3/F-7; (d) accepted
edge: a favorited id whose registration vanishes entirely from the chain has no
entry data and cannot render a card, yet the persisted id is never purged.

**Tasked:** task-153 (anchor-2) — reconciliation (b) only: the missing empty-state
illustration is resolved one way, either the asset ships in `DRepEmptyState`'s
`noFavorites` branch (`DRepEmptyState.tsx:105-123`, today title + body +
Back-to-directory `Link` with no image import) or "prominent illustration" is struck
from `drep-discovery-design.md:109` — the decision is recorded rather than left as
drift. It rides the same task-153 commit as the show-all membership correction at
`drep-discovery-design.md:228` and the F-3 stale-favorite predicate work.
Reconciliations (c) and (d) gain no owner.

**Considered and dropped:** reconciliation (a) — a docs-only §9 key-inventory pass
(this gap plus the slice-6 F-5 one) was weighed as its own slice-8 row and cut,
because the harm it claims assumes the release-end `!!!` copy review enumerates
reviewable strings from the hand-maintained §9 inventory, when grepping `en-US.json`
for the leading `!!!` enumerates them with no inventory maintenance at all and the
cv-1 ja-JP marker guard (task-171) keeps the two locales in sync. The replacement
named in its place — a one-line instruction in the release-end copy-review handoff to
enumerate by that grep rather than from §9 — has no task owner, so it is recorded
here rather than re-derived. One correction from that pass worth keeping: the
"pre-existing" framing above means pre-slice-7, not pre-feature. `git log -S` over
`source/` returns exactly one commit touching `governance.tabs.directory` — this
feature's own slice-1 (`0f47402b6`), which is not an ancestor of `origin/develop` —
so the key is the feature's own drift from §9, and the age framing (repeated at
`slice-7-PRD.md:95-98`) justifies nothing beyond the locale churn a rename would
cost.

## F-3 (R-3, task-122) — AC-5's stale favorites shipped as a forward-compat mechanism; real `Retired`/`doNotList` verification is deferred to the phases that deliver those signals

`DRepStatus` remains `'active' | 'inactive'` (invariant #14), so nothing can be
stale in production today. Built now: `isStaleFavorite` checks `entry.status`
against a `ReadonlySet<string>` containing `'retired'` (`helpers.ts:284-288`) —
type-safe today and false for every current entry — plus an injectable
`isStaleFavoriteEntry` seam (`DRepDirectory.tsx:88,112`;
`DRepDirectoryList.tsx:36,48` defaulting to the helper) so Jest and Storybook
render synthetic stale favorites without fake status values. The caption
(`governance.drepFavorites.staleCaption`, `DRepCard.tsx:124-125`) renders only in
the favorites view (`DRepDirectoryList.tsx:89` gates on `isFavoritesView`;
Jest-pinned positive/negative at `DRepDirectory.spec.tsx:802,816`). No auto-purge
exists anywhere: `loadFavorites`/`toggleFavorite` never drop ids on status change
(`GovernanceStore.ts:333-364`). Consequence for the tracker: task-122 is
`complete`, not `verified` — rendering against real `Retired` (invariant #14's
future signal) and `doNotList` (anchor-2, task-153) states cannot be exercised
until those owners land, at which point only `isStaleFavorite` (and the badge
module under its future owner — the slice-6 F-6 unowned variants are still
unowned) should need to change.

**Tasked:** task-153 (anchor-2) — the deferred `doNotList` half of AC-5 now has an
owner: `isStaleFavorite` recognizes `doNotList=true` alongside the deferred
`retired` status, reading the flag from the verified-metadata field task-153 adds to
`AppDRepDirectoryEntry`, with `DRepStatus` unchanged — the prediction above holds,
only `isStaleFavorite` changes. Two points the finding did not anticipate: (a) the
real predicate has never returned `true` in any test — both stale-caption render
tests inject the seam (`DRepDirectory.spec.tsx:807,820`) and the direct helper test
asserts only the negative case (`helpers.spec.ts:359-364`) — so the amendment turns
the injected-fake caption test into a real one and re-verifies task-122 AC-5 against
the real predicate in the same commit; (b) the badge half of the prediction stays
unowned, since the slice-6 F-6 variants were weighed for anchor-1 and left below the
line, so after task-153 `drep-discovery-design.md:111` still names two badges
(`Retired`, `Excluded from default cohort`) that nothing renders.

## F-4 (R-4, task-122) — The `DREP-FAVORITES` record is the second documented exception to invariant #2's electron-store wording; in exchange every favorites path is logging-free

Invariant #2's wording sweeps "electron-store payload" into the sanitization floor,
but AC-1 requires per-device persistence and invariant #12 sanctions exactly that;
the shipped TOKEN-FAVORITES record and the task-168 DRep-state snapshot (the
documented-exception precedent invariant #2 itself names) settle the pattern. The
spy suite (`tests/jest/security/governance-sanitization.spec.ts`, 23 `it` cases)
spies `filterLogData`, the renderer logger, analytics, and Matomo URL masking — it
intercepts no electron-store call, so the favorites write cannot trip it; the file
is untouched by the slice and re-ran 23/23 at review. The compensating discipline:
zero `logger.*`/analytics calls in every favorites code path — load and persist
failures are swallowed silently with comments stating why
(`GovernanceStore.ts:327-344,347-363`), and unlike
`AssetsStore._onToggleFavorite`, no analytics event fires on toggle (a deliberate
divergence; any event invites payload creep). Jest pins the absence: rejecting
reads log nothing (`GovernanceStore.spec.ts:760-776`) and persistence failure
never logs the payload, with the stringified logger mock calls checked for the
favorite id (`GovernanceStore.spec.ts:779-800`).

## F-5 (R-5, task-122) — One more §9 inventory gap: `governance.drepFavorites.banner` is minted; one contract key pre-existed, leaving a duplicated descriptor

The design specifies the favorites banner copy ("{n} DReps you've favorited.
Favorites are stored on this device only.", `drep-discovery-design.md:109`) but the
§9 inventory has no id for it; it is minted as `governance.drepFavorites.banner`
in the `drepFavorites.*` family (`DRepDirectoryBanner.tsx:50`; `en-US.json:363`,
`ja-JP.json:363`, both `!!!`-prefixed), extending the slice-6 F-5 inventory gap —
§9 still needs a docs-only pass before it can be treated as complete. The copy's
plain ASCII apostrophe is safe unescaped (a lone ICU apostrophe not followed by
`{`/`}` renders literally). Of the 9 contract keys per locale, only 8 locale lines
were added: `governance.drepDirectory.backToDirectory` already existed (minted by
`DRepDetail.tsx` in an earlier slice), and `DRepEmptyState.tsx:47-51` now defines
a second descriptor for the same id with an identical `defaultMessage` but a
different description — react-intl resolves by id so behavior is correct, and
`defaultMessages.json` lists both; a future docs pass may unify the description.
