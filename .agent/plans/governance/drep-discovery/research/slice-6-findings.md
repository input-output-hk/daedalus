# Slice-6 Findings — DRep ID Search + Show-All / Reachability Filters

> Durable findings from slice-6 (2026-07-24). Facts only; grounding anchors verified
> against the `wt/slice-6` working tree at base `6f828d573` (task-121 diff complete
> and code-review approved; the single subject-only task commit is the close-out step).

---

## F-1 (D-1, task-121) — Search is structurally pre-IPC: no search channel exists, so §11's "before IPC" clauses hold by construction

The only governance channels remain `governanceDRepListChannel` /
`governanceDRepStakeChannel` (`GovernanceStore.ts:4-7`); the slice adds zero files
under `source/main/`. Search is a pure filter over already-loaded state: the index
is built from `showAllList` (`DRepDirectory.tsx:134-136`) and exact-match resolution
is a `drepIndex` map lookup after renderer-side `Cardano.DRepID.isValid` +
CIP-129 canonicalization (`helpers.ts:139-153`). The container spec pins the
structural claim behaviorally: "never triggers a store fetch from search
interactions" (`DRepDirectoryPage.spec.tsx:133`) asserts `refresh` and
`reshuffleCohort` uncalled across prefix and invalid-full-form input. An invalid ID
can never reach the main process because nothing in the search path performs IPC.

## F-2 (D-2, task-121) — The 8-character search minimum counts characters AFTER the HRP; tokens §11 wins over the looser task-JSON wording

The tracker's "minimum 8-character prefix" is imprecise; shared-design-tokens §11
(:241) specifies "8 characters (after the `drep1` / CIP-129 HRP prefix)" and is the
named design contract. As shipped, `normalizeDRepQuery` splits a leading
`drep_script1` / `drep1` HRP from the data part (`helpers.ts:28-41`) and
`getDRepQueryKind` requires `data.length >= MIN_SEARCH_PREFIX_LENGTH` (= 8,
`helpers.ts:4`) to enter prefix mode (`helpers.ts:48-57`). Both boundaries are
Jest-pinned in `helpers.spec.ts` and at the component level
(`DRepDirectory.spec.tsx:483` min-length hint below 8 post-HRP characters,
`:495` filtering at exactly 8).

## F-3 (P-5, task-121) — The 51-character full-form floor: a long-but-incomplete pasted ID transiently shows "Invalid DRep ID"

Full-form detection uses the complete bech32 data-part lengths derived from the
`@cardano-sdk/core` payload sizes — 51 for CIP-105, 53 for CIP-129
(`helpers.ts:12-13`). An HRP-qualified query whose data part is ≥ 51 characters but
fails checksum validation classifies `invalidFullForm` (`helpers.ts:52-54`), so a
CIP-129 id pasted or typed to 51-52 data characters transiently shows the
"Invalid DRep ID" error (`governance.drepDirectory.search.invalidId`,
`en-US.json:342` / `ja-JP.json:342`; `role="alert"`,
`DRepDirectorySearch.tsx:49-51`) instead of the prefix state. §11 is silent on this
boundary; the interpretation is recorded in the PRD (P-5) and both edges are
Jest-pinned (`DRepDirectory.spec.tsx:535` bad-checksum error with zero navigation).

## F-4 (P-7, task-121) — Show-all renders the FULL registration list in the seeded session order, a recorded delta from design :228's narrower phrasing

Design :228 says show-all lists "all eligible + top-35"; the shipped `showAllList`
is every registration — top-35, sub-floor, and inactive included — drepId-
canonicalized then `seededShuffle`d with the session `cohortSeed`
(`GovernanceStore.ts:203-211`), because the task acceptance requires "excluded
top-35 and non-cohort DReps" reachable and design :221 itself contemplates
sub-floor entries surfaced via search/show-all. Sharing the cohort's seed means
enabling show-all adds no ranking bias and keeps order stable across refresh.
Store specs pin full membership (`GovernanceStore.spec.ts:631`) and the
seeded/zero-IPC ordering (`:648`); reachability through the UI is pinned at
`DRepDirectory.spec.tsx:552` (show-all) and `:571` (the critiquer-mandated
non-cohort search + exact-open path with show-all off).

## F-5 (D-4, task-121) — §9 key-inventory gap: `cohortBanner.filtered` plus 14 support keys are minted outside the inventory

Tokens §5 (:86) specifies the filtered-banner copy but the §9 inventory has no
message ID for it. Slice-6 minted `governance.drepDirectory.cohortBanner.filtered`
in the §9 `cohortBanner.*` naming style (`DRepDirectoryBanner.tsx:43-48`;
`en-US.json:314`, `ja-JP.json:314`, both `!!!`-prefixed). Of the 21 new keys per
locale, only 6 are §9-inventoried (`cohortBanner.showAll`, `empty.noResults`,
`searchPlaceholder`, `search.minLengthHint`, `search.invalidId`,
`showAll.sortBiasWarning`); the other 15 (filtered line, filter facet labels/options,
sort labels/options, `empty.noResults.clearFilters`) are mints. The §9 inventory
needs a docs-only pass to add the rows before it can be treated as complete.

## F-6 (D-5 + P-13, carried from slice-5 F-4) — Both designed badges remain unowned drift: "Expiring soon" (status) and "Excluded from default cohort" (top-35)

Design :221-222 and tokens §1 (:13-15) describe both badges for entries surfaced
via search/show-all, but no tasks-JSON task owns either, neither has a §9 message
ID, and task-121's acceptance does not mention them. As shipped, such entries
render only the existing status + category badges: `DRepStatus` stays
`'active' | 'inactive'` (`governance.types.ts:35`, invariant #14) and
`DRepStatusBadge.tsx` / `DRepCategoryBadge.tsx` are absent from the slice diff.
Both variants still need a future owner before the design/tokens staging claims
are true (slice-5 R4/F-4 precedent, reaffirmed this slice).

## F-7 (D-3, task-121) — The favorited filter contract exists without any favorites UI; task-122 only feeds the set and renders the control

`helpers.ts` owns `favoritedOnly` in `DRepFilterState` (:164) and its predicate
`favoriteDRepIds.has(entry.drepId)` (:215-217) reading the
`DRepFilterContext.favoriteDRepIds` set (:184-187); `DRepDirectory` threads an
optional `favoriteDRepIds` prop defaulting to the shared frozen
`EMPTY_DREP_ID_SET` (`DRepDirectory.tsx:84,104`; `helpers.ts:175`). No Favorited
control and no `filter.favorited` key ship — an always-empty source would be a
guaranteed-zero-results trap. The predicate is Jest-covered with an injected
non-empty set in `helpers.spec.ts`, so slice-7 (task-122) changes zero filter
semantics: it persists the set, renders the toggle, and feeds the prop.

## F-8 (P-14, pre-existing) — Dedupe-by-credential is fully met, but the dual-ID stacked card row remains an unbuilt display gap

§11 :244 says the deduped row "shows both ID forms stacked". The dedupe
requirement itself holds by construction — the search index has exactly one row
per credential keyed by the canonical CIP-129 id, matched against both encodings
(`helpers.ts:59-84,106-131`) — but cards still render only the single CIP-129 form
(`DRepIdDisplay.tsx:69-77`), a gap pre-existing since slice-2/3 (reviews approved).
Card identity is outside task-121's acceptance; the stacked display needs a future
owner.

## F-9 (build, task-121) — jsdom realm vs the SDK bech32 encoder: three test-only harness fixes any future suite touching `Cardano.DRepID` encoding will need

The `@cardano-sdk/core` bech32 encoder brand-checks its payload against the
ambient `Uint8Array`, and jsdom's constructor lives in a different realm than
Node's `Buffer`, so encoding (`cip129FromCredential` / `toCip105DRepID`) throws
under the default jsdom environment. Fixes shipped, all test-only and file-scoped
(Jest isolates globals per file): (a) `@jest-environment node` docblock on the
DOM-free `helpers.spec.ts` (:1-6); (b) a commented `global.Uint8Array` realm patch
in `DRepDirectory.spec.tsx` (:21-26) pointing the suite's global at Buffer's
prototype constructor; (c) react-polymorph `ThemeProvider` wrappers in
`DRepDirectory.spec.tsx` (:117) and `DRepDirectoryPage.spec.tsx` (:64), because
the Input/Checkbox skins require theme context. Decode-only paths (`isValid`) are
unaffected.

## F-10 (review observations, task-121) — Two accepted runtime edges, both harmless by construction

(a) If ranking transitions Loaded→Failed mid-session after the user enabled
exclude-top-35, the checkbox hides (`DRepDirectoryFilters.tsx:192` gates on
`isShowAll && isRankingAvailable`) while the facet stays `true`; the filter is a
no-op because `top35DRepIds` is empty unless ranking is Loaded
(`GovernanceStore.ts:193-201`), and the banner truthfully stays in filtered mode.
Clear-filters via the noResults state remains the escape hatch when results are
empty. (b) `stripHrp` (`helpers.ts:95-99`) produces a garbage slice for
non-bech32 ids in bare-query matching; unreachable for production ids (all derived
via `cip129FromCredential`, `GovernanceQueryService.ts:624-641`) and unable to
false-positive in practice.
