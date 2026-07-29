# CV-2 Findings — Doc-vs-Repo Conflicts Resolved at Planning

> Durable findings from cv-2 planning (2026-07-28). Facts only; every `path:line`
> below was opened in the cv-2 worktree at base `504b44c1a` (branch
> `feat/drep-discovery`) and verified there. Resolution rule applied throughout
> (prompt.md:39-41): when the plan, the README, the tasks JSON and the live repo
> disagree, **the live repo wins**, the conflict is recorded here, and the
> governing doc is reconciled only where the planning mandate says so.
>
> Each finding carries **Resolution** (what is true and what cv-2 does about it),
> **Disposition** (reconciled now / rides with task-NNN / deferred to phase X /
> record-only) and **Owner** (who discharges it).

---

## F-1 — `drepIndex` is a `Map`, but six corpus sites prescribe bracket access; under this repo's `tsconfig` a bracket read compiles and silently evaluates to `undefined` (live repo authoritative; access rewritten to `resolveExactDRepMatch`)

The store field is a `Map`, not a record:
`@observable drepIndex: Map<string, AppDRepDirectoryEntry> = new Map();`
(`source/renderer/app/stores/GovernanceStore.ts:100`). It is **rebuilt, never
mutated** — `this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));` at
`:254` (list fetch) and `:297` (voting-power refresh) — which is what makes an
`@observer` consumer re-render when the index refreshes. Every live consumer uses
the `Map` API: `governanceStore.drepIndex.get(drepId) ?? null`
(`source/renderer/app/containers/governance/DRepDetailPage.tsx:91`), the whole map
prop-drilled at `source/renderer/app/containers/governance/DRepDirectoryPage.tsx:93`
into a `drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>` prop
(`source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx:81`),
consumed through `resolveExactDRepMatch(searchQuery, drepIndex)` at
`DRepDirectory.tsx:195`.

**Six corpus sites prescribe `drepIndex[drepId]`**, all verified by grep:

- `governance-drep-discovery-plan-tasks.json:1171` (task-136 description) and
  `:1181` (task-136 AC-1) — "sourced from `GovernanceStore.drepIndex[drepId]`".
- `governance-drep-discovery-plan-tasks.json:1241` (task-139 AC-3) — "reads
  `givenName` from `GovernanceStore.drepIndex[drepId]?.givenName`".
- `designs/current-vote-display-design.md:59`, `:101`, `:189`.

`research/cv-1-findings.md:619` also contains the string, but it is a **quotation
of design §9.1 inside F-18**, not a prescription — nothing to correct there.

**Why this is not cosmetic.** `tsconfig.json:79` sets `"strict": false` and `:80`
sets `"noImplicitAny": false`, so `drepIndex[drepId]` type-checks against a `Map`
and evaluates to `undefined` at runtime. `undefined` is exactly the value the
design assigns to "the store has no record yet" (`current-vote-display-design.md:189`
— "If the store has no record yet, the badge is omitted"), so a literal
implementation of the AC would render "Status unavailable" for **every** DRep,
including ones that are indexed, and would pass a naive review.

**A second, independent trap: the key form.** The index is keyed by canonical
**CIP-129** — the main process emits CIP-129 for both credential kinds via
`Cardano.DRepID.cip129FromCredential` (`source/main/governance/GovernanceQueryService.ts:631`,
`:638`) and the store keys on `e.drepId` verbatim (`GovernanceStore.ts:254`).
The delegated id in the wallet domain is byte-untouched and may be CIP-105:
`normalizeDRepIdentity` returns `cip129: raw` for a CIP-129 input
(`source/renderer/app/utils/governance/normalizeDRepIdentity.ts:40`) and
`cip105: raw` for a CIP-105 input (`:56`). So even a correct `.get(raw)` misses
for every CIP-105 delegation, and BIP-173 case variance compounds it.

**Resolution.** cv-2 does not hand-roll the lookup. The exported, generic
`resolveExactDRepMatch<T>(rawQuery, drepIndex: ReadonlyMap<string, T>): T | null`
(`source/renderer/app/components/governance/drep-directory/helpers.ts:139-153`)
trims and lower-cases through `normalizeDRepQuery` (`helpers.ts:28-41`,
`const full = raw.trim().toLowerCase();` at `:29`), canonicalizes via
`Cardano.DRepID.toCip129DRepID` (`:146-148`), and returns
`drepIndex.get(canonical) ?? null` (`:149`). Its own doc
comment pins the invariant cv-2 needs — "Runs entirely in the renderer — an
invalid ID can never reach the main process because nothing here performs IPC"
(`helpers.ts:133-138`) — which is the same constraint
`current-vote-display-design.md:189` states ("MUST NOT spawn a `cardano-cli`
invocation or a fallback IPC lookup"). It is a lookup-only transform; the
submitted string is never touched, so invariant 10 (byte-equality) is unaffected.
This is PRD D-6.

**The helper does not rescue a CIP-105 query.** Canonicalization is gated behind
a validity check that CIP-105 fails: `helpers.ts:144` is
`if (!Cardano.DRepID.isValid(full)) return null;`, and measured in this worktree
`Cardano.DRepID.isValid('drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l')`
returns `false` while the `drep1…` form returns `true`. The helper therefore
canonicalizes only the `drep1…` / `drep_script1…` forms and returns `null` for
the CIP-105 form — the same silent "no record yet" miss described above. The
cv-2 query is consequently `currentVote.drep.cip129 ?? currentVote.drep.raw`,
never `raw`. `cip129` is always populated on the production path:
`normalizeDRepIdentity` fills it for both encodings (`normalizeDRepIdentity.ts:40`,
`:55`) and `parseVoting` returns `null` when `normalizeDRepIdentity` fails
(`source/renderer/app/api/api.ts:3009-3023`), so `?? raw` covers hand-built
fixtures only.

**Disposition.** Reconciled in the cv-2 PRD (D-6, and the C-1 row of
"Corpus-vs-Repo Corrections cv-2 Inherits"); the code lands with **task-136**
(badge read) and **task-139** (mount + container wiring), and the lookup is
pinned executably by **task-147** — task-136's unit cases pass `drepEntry` in
directly and exercise no lookup, and task-139 adds no spec file. The three
`designs/current-vote-display-design.md` bracket sites (`:59`, `:101`, `:189`) are
**not** edited — no cv-2 acceptance criterion owns design-doc access notation, and
D-4 confines cv-2's design-doc edits to the comparator paragraph. Recorded here so
a later reader does not treat the design's notation as a contract.

**Owner.** task-136 / task-139 for the code, **task-147** for the executable pin
on the `drepIndex` → `drepEntry` → badge chain and on the `cip129 ?? raw` query;
the tasks-JSON AC-1 and AC-3 wording is reconciled at Scribe time on those rows'
own build commits.

---

## F-2 — `givenName` has no data source anywhere in cv-2, and `DRepIndexEntry` does not exist in code; task-139 AC-3 is three clauses of which only one is buildable (split, not deleted)

**No name reaches the renderer in cv-2.** Enumerated, not sampled:

- `AppDRepDirectoryEntry` (`source/renderer/app/stores/GovernanceStore.ts:20-31`)
  carries exactly `drepId` (`:22`), `votingPower` (`:24`), `status` (`:26`),
  `drepActivity` (`:28`), `anchor` (`:30`). **No name field, no expiry field.**
- Its IPC counterpart `DRepDirectoryEntry`
  (`source/common/types/governance.types.ts:51-62`) is the same shape, and
  `DRepAnchorPresence` (`:66-72`) is a `{ url, hash }` **reference**, never fetched
  content.
- `DRepIdentity` (`governance.types.ts:20-31`) has no name either — `raw`,
  `cip129`, `cip105`, `credentialHex?` (`:28`), `credentialType` (`:30`).
- `_rehydrateDReps` maps all five fields and invents none
  (`GovernanceStore.ts:381-385`).

**`DRepIndexEntry` — the type `designs/current-vote-display-design.md:101` names as
the owner of `givenName` / `anchorUrl` — does not exist.** A repo-wide
`grep -rn "DRepIndexEntry" --exclude-dir=node_modules --exclude-dir=.git` returns
only that design line plus the cv-2 PRD's own citations of it. The **index** does
exist (`GovernanceStore.ts:100`); the **entry type** the design names does not.

**`givenName` appears exactly once in `source/`, `storybook/` and `tests/`** and it
is a *negative* regression fixture asserting the field never renders:
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx:89`
(`givenName: 'Sneaky Unverified Name'`), inside the test "never renders a name
field, even if extra fields sneak into the identity" (`:85`), whose assertion is
`screen.queryByText('Sneaky Unverified Name')` at `:95`. No CIP-119 parse exists;
the fetch / hash-verify / parse pipeline is task-149 + task-150 (`anchor-1`), both
`pending`.

**AC-3 is three clauses, not one.** Verbatim from
`governance-drep-discovery-plan-tasks.json:1241`: "The `drep` state reads
`givenName` from `GovernanceStore.drepIndex[drepId]?.givenName`. The panel updates
reactively when `drepIndex` is populated or updated; no wallet re-poll is
triggered. A Storybook story covers the transition from unverified to verified
name." Clause 1 (read `givenName`) and clause 3 (unverified→verified story) are
unbuildable for the reasons above. **Clause 2 is buildable and is already required
elsewhere in cv-2** — task-136 AC-1 (`:1181`) sources the live status badge from
the index "(no new IPC / cardano-cli invocation issued by this component)", and
the reactivity comes free because the store reassigns the `Map`
(`GovernanceStore.ts:254`, `:297`) and `VotingPowerDelegation` is already
`injectIntl(observer(...))`. Deleting AC-3 wholesale would drop a constraint cv-2
must still honor.

**No anchor task currently owns the struck work.** Checked row by row: task-151
(anchor-1) AC-1 scopes verified `givenName` to "the DRep detail view" and its AC-3
exposes only "verified metadata-completeness state … for the slice-5 cohort rule"
— **not** a name field on the index entry; task-154 and task-157 AC-2 (anchor-2)
scope `givenName` to the **delegation confirmation dialog**; task-155 AC-3 covers
labeling in "DRep detail and delegation confirmation". `CurrentVoteSummary` is
named in none of them. The only statement that anchor-1 populates a name into the
index is **untasked prose** at `designs/shared-design-tokens.md:250`.

**Resolution.** PRD D-5 splits AC-3: clause 2 is retained and folded into the same
`drepIndex`-sourced read task-136 already mandates; clauses 1 and 3 are struck from
cv-2 with the evidence anchored at `GovernanceStore.ts:20-31`. cv-2 does **not**
edit anchor rows — but it records that re-pointing the struck clauses needs two
tracker edits nobody has made: (1) extend **task-151** with a criterion exposing a
verified-name field on the store's index entry, and (2) add or extend an
**anchor-2** row owning the `CurrentVoteSummary` name render and the
unverified→verified story. Without both, the deferred work is **orphaned, not
rescheduled**.

**Disposition.** AC-3 rewrite rides with **task-139**'s build commit (tracker AC-3
reduced to the reactivity clause; `statusReason` naming `GovernanceStore.ts:20-31`
as the evidence). The struck clauses are **deferred to `anchor-2`**, conditional on
the two tracker edits above. This is PRD R-1 (high).

**Owner.** task-139 for the split and the `statusReason`; the **anchor-1 and
anchor-2 planning passes** own the two tracker edits. If they do not act, the
clauses die silently — that is the risk this finding exists to prevent.

---

## F-3 — there is no `expiring` status-badge variant, and adding one to `CurrentVoteSummary` creates the **fourth** independent copy of an expiry window; the obvious label and copy are already taken

**The shared badge cannot express it.** `DRepStatus` is a closed two-value union —
`export type DRepStatus = 'active' | 'inactive';`
(`source/common/types/governance.types.ts:35`). `DRepStatusBadge` holds exactly two
descriptors, `active` (`source/renderer/app/components/governance/_shared/DRepStatusBadge.tsx:8`)
and `inactive` (`:13`), a `labelMap: Record<DRepStatus, string>` with only those two
keys (`:26-28`, exhaustive by type), and its only glyph is
`<span className={styles.dot} aria-hidden="true" />` (`:36`) — no warning triangle.
`DRepStatusBadge.scss` has `.active` (`:25`) and `.inactive` (`:34`) rules and no
third class. The producer agrees: `const status: DRepStatus =` … `expiry <=
currentEpoch ? 'inactive' : 'active'`
(`source/main/governance/GovernanceQueryService.ts:507`), and so does the filter
type `DRepStatusFilter = 'all' | 'active' | 'inactive'`
(`source/renderer/app/components/governance/drep-directory/helpers.ts:155`). The
badge has exactly two consumers, `DRepCard.tsx:119` and
`DRepDetailOnchainSection.tsx:99` — both outside cv-2's fence.

**Four live expiry windows, not one.** The corpus claims the only live 7-12 logic
is in `DRepCategoryBadge`. Live it is one of four:

1. `source/renderer/app/components/governance/_shared/DRepCategoryBadge.tsx:50-51`
   (`THRESHOLD_WINDOW_MIN = 7` / `MAX = 12`), consumed at `:62-64`.
2. `source/renderer/app/components/governance/drep-directory/helpers.ts:181-182`
   (`EXPIRY_WINDOW_MIN` / `MAX`), consumed by the `thresholdWindow` filter at
   `:205-208`. The duplication is **deliberate and documented**: "The 7-12
   remaining-epoch window is restated here on purpose: filter code must never
   import from the badge module (badges are informational only)"
   (`helpers.ts:178-179`).
3. `source/renderer/app/stores/GovernanceStore.ts:62`
   (`const COHORT_MIN_REMAINING_EPOCHS = 6;`) applied at `:183`
   (`entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS`) — the same `>= 7` lower
   edge expressed as `> 6`, gating default-cohort membership (invariant 7).
4. The `expiryAsc` sort ordering on `drepActivity` (`helpers.ts:249`).

`DRepCategoryBadge`'s constants are **not exported** (`:50-51`; the file's `export`s
are at `:43`, `:45`, `:60`, `:101`), so importing them is not an option — a fifth
statement is unavoidable if cv-2 renders an expiring overlay.

**The obvious label string is already taken.** `Expiring in 7–12 epochs` is the
directory *filter option*: descriptor id
`governance.drepDirectory.filter.expiry.thresholdWindow` with
`defaultMessage: '!!!Expiring in 7–12 epochs'`
(`source/renderer/app/components/governance/drep-directory/DRepDirectoryFilters.tsx:61-62`),
shipped in both catalogs at `source/renderer/app/i18n/locales/en-US.json:332` and
`ja-JP.json:332` ("!!!7〜12エポックで失効"). And a `{n}`-style epoch count already
renders **adjacent to** (not inside) the status badge:
`governance.drepDetail.expiresInEpochs` = `'!!!{count} epochs'`
(`DRepDetailOnchainSection.tsx:26`), rendered at `:107-109` behind the guard
`entry.status === 'active' && entry.drepActivity != null`, directly under
`<DRepStatusBadge status={entry.status} />` at `:99`.

**cv-2's panel does not use the shared badge at all today**, and a committed test
asserts the absence: `CurrentVoteSummary`'s `styles.statusBadge`
(`source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx:59`,
`:84`) is a local vote-kind chip, and
`CurrentVoteSummary.spec.tsx:62` asserts `screen.queryByText(/Active|Inactive|Expiring/)`
is not in the document.

**Resolution.** PRD D-1 + D-2: reuse `<DRepStatusBadge status={entry.status} />`
unchanged for the two ledger-grounded states (zero new keys — the directory's
`status.active` / `.inactive` already ship) and render the derived expiring overlay
as a **component-local** badge in `CurrentVoteSummary`, following that component's
own precedent for local status rendering (`CurrentVoteSummary.tsx:59`). The
threshold is `≤ 12` with the constant declared locally, carrying a
`helpers.ts:178-179`-style comment stating *why* this window differs from the badge
module's (the panel is not cohort-scoped) — no task id, no change history, no
defense of correctness in that comment. New copy takes fresh ids under
`voting.governance.currentVote.status.*`; nothing is reused from the filter or the
detail view, only the ICU shape, with the argument named `{n}`.

**Disposition.** Reconciled in the PRD (D-1, D-2, D-9). Rides with **task-136**
(local badge + descriptors) and **task-146** (catalogs). The spec rewrite at
`CurrentVoteSummary.spec.tsx:61-63` and the four colocated snapshots are **in
task-136's scope**, not a surprise at review (PRD R-2). `DRepStatus`,
`DRepStatusBadge` and its two existing consumers are **not** touched — invariant 14
keeps `expiring` renderer-derived.

**Owner.** task-136 (component + spec + snapshots), task-146 (both catalogs).

---

## F-4 — no *per-DRep expiry epoch* crosses IPC, but the renderer does hold a current epoch; and the corpus names the wrong drop site for `payload.epoch`

Two corrections to the inherited claim, both material to how an AC may be worded.

**(1) The drop site is `fetchDRepList`, not `_rehydrateDReps`.** `payload.epoch`
exists on the wire — `epoch: number | null` at
`source/common/types/governance.types.ts:92-93`, populated at
`source/main/governance/GovernanceQueryService.ts:245` from `_parseTipEpoch`
(`:239`, defined `:419`). The renderer discards it in the `runInAction` inside
`fetchDRepList` (`source/renderer/app/stores/GovernanceStore.ts:251-259`), which
reads only `payload.dreps` (`:252`) and `payload.fetchedAt` (`:256`).
`_rehydrateDReps` (`:379-387`) **drops nothing** — it operates on
`DRepDirectoryEntry`, which has no `epoch` field at all
(`governance.types.ts:51-62`), and maps all five fields (`:381-385`). Any guide step
pointing at `_rehydrateDReps` for an epoch change is mis-anchored; the one-line
landing site is `GovernanceStore.ts:251-259`.

**(2) "the renderer has no epoch" is false.** `NetworkStatusStore` holds
`networkTip: TipInfo | null | undefined` (`source/renderer/app/stores/NetworkStatusStore.ts:123`)
and `TipInfo` carries `epoch: number` (`source/renderer/app/api/network/types.ts:2`);
`nextEpoch` (`NetworkStatusStore.ts:125`) and `futureEpoch` (`:127`) are there too.
What is genuinely absent is **any per-DRep expiry epoch**: main collapses it —
`status = expiry <= currentEpoch ? 'inactive' : 'active'`
(`GovernanceQueryService.ts:507`) and
`const drepActivity: DrepActivity = Math.max(0, expiry - currentEpoch);` (`:511`) —
and `AppDRepDirectoryEntry` has no expiry field (`GovernanceStore.ts:20-31`).

**The generalization over consumers holds.** Every remaining-epochs derivation in
the renderer reads `drepActivity`: `GovernanceStore.ts:183` (cohort floor),
`helpers.ts:205-208` (window filter) and `:249` (`expiryAsc` sort),
`DRepCategoryBadge.tsx:62-64` (category), `DRepDetailOnchainSection.tsx:107-109`
("Expires in {count} epochs"). No `currentEpoch >= expiry` comparison exists
anywhere in the renderer.

**Resolution.** The constraint is stated as **"no per-DRep expiry epoch crosses
IPC"**, never as "the renderer has no epoch". `drepActivity` is therefore the only
admissible source for cv-2's expiring derivation — not because an epoch is
unavailable, but because the *expiry* is. A real expiry comparison would be a
main-process + shared-type change (add `expiry` to `DRepDirectoryEntry` at
`governance.types.ts:51-62` and stop collapsing at `GovernanceQueryService.ts:507-511`),
not a renderer-only edit; it is explicitly out of cv-2.

**Disposition.** Record-only for the code (cv-2 stores no epoch and changes neither
file). Reconciled in the PRD's C-4 row and in "What cv-2 Deliberately Does NOT
Include". Any AC or guide sentence implying "the renderer has no epoch" is wrong
and must not be authored.

**Owner.** task-136 consumes `drepActivity`; no row owns the IPC widening, and none
should in cv-2.

---

## F-5 — the earlier findings audit (`2ee5f74cf`) already discharged task-173 AC-5 and task-140 AC-7's design-doc conjunct; two corrections are still owed, at two different files, under two different edit rules

`git log --oneline -- designs/current-vote-display-design.md` returns three commits
— `2ee5f74cf`, `0f47402b6`, `503540034` — so **`2ee5f74cf`** ("docs(gov): fold
findings audit into tracker rows and plan docs") is the only one since slice-1. It
split the old single line 95 into two paragraphs.

**Already discharged at HEAD:**

- `designs/current-vote-display-design.md:95` now classifies CIP-129 ids by the
  header byte — "for CIP-129 it comes from the payload rather than the HRP: a
  `drep1...` id carries its type in the leading header byte — `0x22` -> `'key'`,
  `0x23` -> `'script'`" — which is exactly what **task-173 AC-5**
  (`governance-drep-discovery-plan-tasks.json:1283`) asks for. **Satisfied; do not
  re-edit the line or double-write the paragraph.**
- `:97` is now a dedicated comparator paragraph and already states "A
  case-sensitive bech32 string comparison — including canonical CIP-129 with its
  type-byte header — is not acceptable". That satisfies the first conjunct of
  **task-140 AC-7** (`:1263`). The `case-insensitive cip129` alternative `:97`
  still offers is *not* what AC-7 forbids: AC-7 forbids a canonical CIP-129 string
  as a key, and `:97` forbids exactly the case-sensitive form of it.

**Still owed — and the tracker never recorded the pre-discharge:** both task-140
and task-173 are still `status: "pending"`.

1. **`task-plans/cv-1-code-review.md:736-738` is stale.** It reads: "any same-vote
   comparator must key on `cip129` or on the (`credentialHex`, `credentialType`)
   pair, never on `credentialHex` alone." The file is **append-only**
   (`README.md:14`), so the discharge is an **appended entry**, never an in-place
   edit. The substance is already recorded in the promise block at `:1224-1234`
   ("Correction owed on the comparator note at `:737-739`"), so what is owed is the
   **formal discharge, not new analysis**. Note the promise block's own
   self-reference is **off by one**: it says `:737-739` at both `:1224` and `:1234`,
   but the note occupies `:736-738` — `:739` begins a different refutation item
   ("The tracker says this function classifies `abstain` / `no_confidence`…").
   task-140 AC-7's `:736-738` is the correct anchor; the log's `:737-739` is not.
2. **`research/cv-1-findings.md:220-227` (F-9's "Tasked:" paragraph) is false at
   HEAD.** It states that "`designs/current-vote-display-design.md:95` still offers
   'canonical CIP-129 string including the type-byte header' as an acceptable
   comparison key, and `task-plans/cv-1-code-review.md:736-738` still offers
   `cip129` alone" (`:222-225`). The first half was fixed by the very commit that
   wrote the sentence; only the second half is still true. Findings files are **not
   append-only** — `README.md:14` marks only `<id>-code-review.md` — so this one is
   **corrected in place**.
3. **task-140 AC-7's `:95` anchor is stale**; the comparator sentence lives at
   `:97`. task-173 AC-5's `:95` anchor is still correct (and already satisfied).

**Scope check.** A corpus-wide grep for `comparator|credential bytes|canonical
CIP-129|type-byte header` across the plan `*.md` files finds the stale
case-key claim at only two sites: `cv-1-code-review.md:736-738` and
`cv-1-findings.md:222-224`. `current-vote-display-design.md:259`, the cv-1-PRD
mentions, and the slice-6/slice-7 "canonical CIP-129" references are about
different concerns (header-byte round-tripping, id persistence, key/script
collision) and are **not** stale.

**Resolution.** PRD D-4. task-140 appends **one** sentence to `:97` recording which
of the two acceptable comparison forms cv-2 actually ships — the (`credentialHex`,
`credentialType`) pair — so the design and the code cannot drift; it does **not**
rewrite the paragraph. task-173 verifies `:95` and records the pre-discharge rather
than re-editing.

**Disposition.** Two acts, both riding with **task-140**'s build commit: an
**appended** discharge entry in `cv-1-code-review.md` (which must also note the
`:737-739` → `:736-738` off-by-one) and an **in-place** fix at
`cv-1-findings.md:220-227`. AC-7's `:95` → `:97` re-anchor rides the same commit.
task-173 records its AC-5 as pre-discharged by `2ee5f74cf` in its `statusReason`.

**Owner.** task-140 (both corrections + the re-anchor + the appended design
sentence); task-173 (verify-and-record only).

---

## F-6 — the corpus's `L`-prefixed prose anchors have drifted and under-count; its `path:line` anchors have not (task-145 gains a reuse site; task-142's anchor was never correct at any commit)

**task-145 — `GOVERNANCE_WALLETS`.** The tracker
(`governance-drep-discovery-plan-tasks.json:1380`) says it is "currently at L57-83
with reuse at L228 / L427-458". Live in
`storybook/stories/voting/Governance.stories.tsx` (507 lines),
`grep -n GOVERNANCE_WALLETS` returns exactly five hits and no others anywhere in
`storybook/` or `source/`:

```
:63  const GOVERNANCE_WALLETS = [
:233       wallets={GOVERNANCE_WALLETS}
:420         wallets={GOVERNANCE_WALLETS}
:457           selectedWallet={GOVERNANCE_WALLETS[0]}
:492           selectedWallet={GOVERNANCE_WALLETS[1]}
```

The definition block is `:63-97` (`:96` is the third `generateWallet` call's
closing `),`, `:97` is `];`) — not `:63-96`. There are **four** reuse sites, not
three. The extra one is `:420`, inside `.add('Voting power delegation - prefilled
from directory', …)` at `:403`, a story that did not exist when the tracker text
was written. **No corpus anchor mentions `:420` at all**, so task-145's stated
migration scope is *incomplete*, not merely shifted.

**task-142 — the HW status section.** AC-3
(`governance-drep-discovery-plan-tasks.json:1318`) reads "HW status section (lines
~L118-L127) is untouched." Live, the block is
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.tsx:179-185`
— `{selectedWallet.isHardwareWallet ? (` at `:179`, `<HardwareWalletStatus …` at
`:180`. The block was never at L118-127 at any commit in the file's history (it was
`:160-166` when the plan was authored, `:180-181` at `bdad1d227`), so a mechanical
`+N` re-anchor yields a wrong line — the criterion needs **semantic**
re-identification.

**The drift is confined to the prose-style citations.** Every `path:line`-style
anchor in the corpus that targets these two files checks out against live code:
`slice-3-implementation-guide.md:840` already cites `:179-185`; the tracker's
task-175 description (`governance-drep-discovery-plan-tasks.json:1324`) cites
`:151-172` (live: the `{drepIdentity ? (` block opens at `:151`, with
`<code className={styles.drepIdValue}>{drepIdentity.raw}</code>` at `:160`);
`cv-1-code-review.md:831` cites `Governance.stories.tsx:58-61` (live:
`toStoryDRepIdentity` at `:58`).

**Resolution.** PRD D-8 re-anchors task-145 from a live read (definition `:63-97`;
four reuse sites `:233`, `:420`, `:457`, `:492`) and PRD D-3 replaces task-142
AC-3's line citation with the semantic one — *the `selectedWallet.isHardwareWallet ?`
branch rendering `<HardwareWalletStatus …/>` is untouched* — matching the anchor
`slice-3-implementation-guide.md:840` already uses. The corpus's `path:line`
citation style is adopted going forward; the `L`-prefixed prose style is not.

**Disposition.** Rides with **task-145** (the fourth reuse site is real scope, not
bookkeeping) and **task-142** (criterion re-worded on its own row). Both tracker
rows carry the corrected anchors in their `statusReason` at Scribe time.

**Owner.** task-145, task-142.

---

## F-7 — the storybook fixture surface is genuinely net-new, but not from scratch: `_utils/` is absent while the logic it formalizes already ships inline, `GovernanceWrapper` is named only in docs, and `generateWallet` cannot express `votingTarget`

**Net-new, confirmed.** `storybook/stories/governance/` contains exactly five files
— `CurrentVoteSummary.stories.tsx`, `DRepCategoryBadge.stories.tsx`,
`DRepDetail.stories.tsx`, `DRepDirectory.stories.tsx`,
`DRepDirectoryBanner.stories.tsx` — and **no `_utils/` subdirectory**. The
convention exists in siblings: `storybook/stories/news/_utils`,
`storybook/stories/nodes/_utils` and `storybook/stories/wallets/_utils`, the last
holding `HardwareWalletsWrapper.tsx` and `WalletsWrapper.tsx` — the naming
precedent for a `*Wrapper.tsx`.

**`GovernanceWrapper` is absent from code but already specified.** Zero hits under
`source/`, `storybook/`, `tests/`; the name is already fixed by
`designs/current-vote-display-design.md:25` — "pure `makeGovernanceWallets(option)`
factory + `GovernanceWrapper` `key`-derivation rule, with any previous module-level
mutable `GOVERNANCE_WALLETS` retired" — by the tracker rows for task-144
(`governance-drep-discovery-plan-tasks.json:1363-1371`) and by
`cv-1-code-review.md:1680`. So the contract is cited, not invented.

**What the "from scratch" framing hides.** The logic task-143 formalizes already
ships inline, delivered by cv-1, in
`storybook/stories/governance/CurrentVoteSummary.stories.tsx`:
`CURRENT_VOTE_OPTIONS` at `:23-28` with only **four** options (`noDelegation`
`:24`, `drepUnverified` `:25`, `abstain`, `noConfidence` `:27`) — `drepVerified` is
absent and task-143 AC-3 requires five; `resolveCurrentVote(option)` at `:30-52`,
the existing option-id → `WalletVotingTarget` mapper; checksum-verified vectors
`KEY_CIP129` / `KEY_CIP105` / `KEY_CREDENTIAL_HEX` at `:17-21`; and the
`key={option}` remount pattern task-144 asks a wrapper to own, already applied at
`:73`. The committed vectors are **not** the CIP-119 vectors task-143's AC-4 names.

**`generateWallet` cannot express a `votingTarget`.**
`storybook/stories/_support/utils.ts:104-142` is the sole definition in the repo:
nine positional params (`name, amount, assets, reward, delegatedStakePool,
hasPassword, status, isHardwareWallet, id` — `:105-116`) and a `new Wallet({…})`
literal (`:118-142`) whose last key is `delegatedStakePoolId: get(delegatedStakePool,
'id'),` (`:141`). No `votingTarget` — even though the domain accepts one
(`source/renderer/app/domains/Wallet.ts:130` `votingTarget?`, `:166` the
`@observable`, `:200` in the `update()` pick list, `:255-256` `get currentVote()`).

**Resolution.** PRD S-6 + D-7. task-143 is scoped as **extracting and superseding**
the inline block at `CurrentVoteSummary.stories.tsx:17-52` — adding the fifth
`drepVerified` id, with the story migrating onto the shared module rather than
gaining a local fifth entry — and `makeGovernanceWallets(option)` constructs
`new Wallet({…})` directly rather than widening the shared `generateWallet` helper
(widening a nine-positional-param helper used across the whole story tree is a
larger blast radius than cv-2 needs). task-144 cites its name and `key`-derivation
rule from `current-vote-display-design.md:25`. AC-4's "with verified hash" clause
cannot be satisfied in cv-2 — no anchor fetch or Blake2b-256 verify exists until
anchor-1 task-149/task-150 — and is recorded as satisfied-in-part with that reason.

**Disposition.** Rides with **task-143** (fixtures module + fifth option +
provenance) and **task-144** (wrapper). AC-4's hash half is **deferred to
`anchor-1`** as a documented partial, not silently claimed green.

**Owner.** task-143, task-144.

---

## F-8 — three governance story files are git-tracked but registered nowhere, so they never render; the gap is real, already recorded as a deferral, and cv-2 deliberately leaves it open

`storybook/main.ts:8` is `stories: ['../storybook/stories/index.ts']` — a single
manual index, **not** a glob — so a story that `index.ts` does not import never
enters the preview bundle. `storybook/stories/index.ts` registers
`./governance/DRepDirectory.stories` (`:17`) and
`./governance/CurrentVoteSummary.stories` (`:18`) and nothing else from that
directory; `:15-16` are `./voting/Voting.stories` and `./voting/Governance.stories`,
which are **voting**, so the correct anchor for the governance registrations is
**`index.ts:17-18`**, not `:15-18`. `DRepDetail.stories.tsx` (96 lines),
`DRepDirectoryBanner.stories.tsx` (45) and `DRepCategoryBadge.stories.tsx` (53)
exist on disk and are imported by no file in the repo.

**Not a new discovery.** It is written down twice: the cv-1 guide carries a
"**Record-only observation (do NOT fix in cv-1)**" naming all three files
(`task-plans/cv-1-implementation-guide.md:1851-1857`), and task-133's
`statusReason` repeats it (`governance-drep-discovery-plan-tasks.json:1020` —
"remain absent from storybook/stories/index.ts and still never render — a
pre-existing gap outside cv-1 scope"). *(That `statusReason` cites the guide note
as `:1850-1856`; live it is `:1851-1857` — a one-line drift, record-only.)*

**No cv-2 task owns it.** task-145's `targetPath` is
`storybook/stories/governance/` and all four of its acceptance criteria concern the
`currentVote` knob and the `GOVERNANCE_WALLETS` migration; none mentions
`index.ts`. Separately, none of the three unregistered files renders a wallet or a
current vote at all, so they are not current-vote surfaces:
`DRepDirectory.stories.tsx:134` is `wallets: null` inside a sidebar-menus object,
and the only `VotingPowerDelegation` render sites in the whole corpus are
`storybook/stories/voting/Governance.stories.tsx:214` and `:405`.

**Resolution.** PRD D-12: cv-2 does **not** register them — registering three
stories no cv-2 task owns is scope creep against an explicit prior deferral. The
consequence is recorded honestly instead: task-145 AC-1's "every governance story"
is *unverifiable* for those three because they never render, which is why PRD D-8
scopes AC-1 to the stories that actually render a wallet (`Voting / Governance >
Connected flow`, `> Voting power delegation`, `> Voting power delegation - prefilled
from directory`, and `Governance / Current Vote Summary > Core states`).

**Disposition.** **Deferred** — carried forward unchanged as a residual gap for a
later slice, with the corrected anchor `storybook/stories/index.ts:17-18`. Nothing
rides with a cv-2 row.

**Owner.** Nobody in cv-2, deliberately. It belongs to whichever later slice next
edits `storybook/stories/index.ts`, or to a standalone hygiene row if a reviewer
would rather schedule it than keep accepting it.

---

## F-9 — `filterLogData`'s guarded set is 20 request/response key names; the entire renderer-domain surface cv-2 makes live is unguarded, and cv-1's F-15 under-states both the set and its own anchors

**The guarded set, enumerated.** `source/common/utils/logging.ts:24-49` holds
exactly 20 exact-match strings: `spendingPassword`, `oldPassword`, `newPassword`,
`mnemonic`, `recoveryPhrase`, `passphrase`, `password`, `votingKey`, `stakeKey`,
`signature`, `accountPublicKey`, `extendedPublicKey`, `publicKeyHex`,
`chainCodeHex`, `signedTransactionBlob`, `withdrawal` (`:25-40`), plus the four
governance entries `drepId`, `dRepId`, `vote`, `voting` (`:45-48`). Matching is
**exact string equality** (`sensitiveData.includes(key)`, `:59`) and a hit
**deletes the whole subtree** rather than masking it (`:59-61`).

"Wire-keyed" is imprecise: `spendingPassword` is a Daedalus request-param name and
`dRepId` never appears in a wire body at all — it is the `DelegateVotesParams`
field interpolated into the URL path. The accurate statement is that the list is
keyed to **request/response** names and knows nothing of the cv-1 domain-object
shape.

**Everything cv-2 makes live is unguarded**, and the gap is wider than the two
names F-15 records:

- `votingTarget` and `currentVote` — `Wallet.ts:130`, `:166`, `:255-256`. mobx
  5.15.7 (`package.json:243`) defines `@observable` props with
  `enumerable: true` (`node_modules/mobx/lib/mobx.js:4362`), so
  `Object.keys(wallet)` carries `votingTarget`; `currentVote` is a prototype
  getter and is invisible to the recursion, and it never matches `vote` because
  the match is exact.
- `drepIdentity` and its members `raw` / `cip129` / `cip105` / `credentialHex` /
  `credentialType` (`source/common/types/governance.types.ts:20-31`) — live in cv-2
  via `source/renderer/app/containers/voting/VotingGovernancePage.tsx:75-83` and
  `VotingPowerDelegationConfirmationDialog.tsx:56`, both touched by task-173 /
  task-175.
- `chosenOption` — `VotingStore.ts:285`, `:372`; `VotingPowerDelegation.tsx:26`,
  `:160` — touched by task-137 / task-138 / task-140.
- The `abstain` / `no_confidence` sentinels ride the `kind` member of
  `WalletVotingTarget` (`source/renderer/app/api/wallets/types.ts:86-93`), an
  unguarded key — so the **domain shape defeats the sentinel guard too**, not just
  the id guard.
- Key-position ids that key-name filtering can never reach:
  `stakeByDRepId` (`governance.types.ts:98`, built at
  `GovernanceQueryService.ts:583-616`) keeps DRep ids as object **keys**. These are
  public ledger data by the repo's own stance (`source/main/utils/setupLogging.ts:180`
  — "deliberately bypasses filterLogData: every value is public ledger data") — a
  note, not a defect.

**The existing governance loggers already sit inside the safe envelope**:
`GovernanceStore.ts:264` and `:303` log only `{ errorType }`; `VotingStore.ts:357`,
`:362` and `:409` carry only an `errorCode`.

**F-15's own anchors have drifted** (`research/cv-1-findings.md:472`): it cites the
`filterLogData` describe as `:58-136` — live it is
`tests/jest/security/governance-sanitization.spec.ts:70-216` (the `describe` opens
at `:70` and closes at `:216`; `:218-456` is "call boundaries", `:458-494` is URL
masking), and `grep -n 'votingTarget\|currentVote'` over that file returns nothing.
It cites the mapper at `api.ts:3153` — live `:3145` — and the `Wallet` constructor
at `Wallet.ts:175-177` — live the constructor opens at `:176` with
`Object.assign(this, data)` at `:177`. Also, the comment at `logging.ts:42-44`
credits `omit-deep-lodash`, but the module imports nothing and the recursion is the
hand-rolled `redact` closure at `:51-71` — an editor of this list must not go
looking for a library call.

**Resolution.** PRD S-9: cv-2 discharges F-15 with the **stricter invariant**
rather than a key-list patch — *no domain `Wallet` and no `DRepIdentity` ever
enters a logger or analytics payload from a cv-2 code path* — asserted with the
task-111 spy pattern over the flows task-137/138/140 (`chosenOption`) and
task-173/175 (`drepIdentity`) create. cv-2 adds no logging anywhere. If a review
nonetheless finds a payload that must be logged, the specified fallback is to
extend `sensitiveData` with `votingTarget`, `currentVote`, `drepIdentity`,
`chosenOption`, `cip129`, `cip105`, `credentialHex` **and** add domain-shaped cases
to the floor suite — the whole surface, not two keys (PRD R-6).

**Disposition.** Rides with **task-147** (the slice's regression harness and the
natural home of the "no DRep id / abstain / no_confidence in logs" criterion).
cv-1's F-15 is thereby discharged in substance; its drifted anchors are corrected
here rather than in place, since F-15 is cv-1's record of cv-1's state.

**Owner.** task-147, with task-173/task-175 and task-137/138/140 as the rows that
make the surface live.

---

## F-10 — `source/renderer/app/containers/voting/Governance.tsx` cannot be **parsed** by prettier 2.1.2, so any invocation that includes it exits 2; and the pre-existing dirty set is 238 files, not four

Measured at HEAD in this worktree:

```
$ node_modules/.bin/prettier --check source/renderer/app/containers/voting/Governance.tsx
[error] source/renderer/app/containers/voting/Governance.tsx: SyntaxError: ',' expected. (4:27)
exit=2
```

`source/renderer/app/containers/voting/Governance.tsx:4` is
`import { withRouter, type RouteComponentProps } from 'react-router-dom';` — an
inline `type` import specifier (TS 4.5+) that TypeScript 4.9.5 accepts (hence
`tsc --noEmit` is green) but prettier 2.1.2's bundled parser rejects. The file is
genuinely in scope, not ignored. Consequence: `--write` on it is a guaranteed
no-op that exits **2** (error), not 1 (style drift), and any explicit-path or glob
invocation that *includes* it fails as a whole.

**The dirty set is not the four files the corpus names.** A repo-wide check over
`source/`, `storybook/` and `tests/` `*.{ts,tsx}` reports **238 dirty files**
(239 `[warn]` lines minus the trailing summary) and exits 2 because of the parse
error above. The four commonly cited files are dirty — measured, all four flagged,
exit 1: `VotingPowerDelegation.tsx`,
`VotingPowerDelegationConfirmationDialog.tsx`, `VotingGovernancePage.tsx`,
`storybook/stories/voting/Governance.stories.tsx` — but they are a **sample**.
Within the governance/voting surface alone the measured dirty set is **15** files,
adding `source/main/governance/GovernanceQueryService.ts`,
`source/main/ipc/governanceChannel.ts`,
`source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`,
three `voting-registration-wizard-steps` files (`…ChooseWallet.tsx`,
`…EnterPinCode.tsx`, `widgets/ConfirmationDialog.tsx`),
`source/renderer/app/containers/voting/dialogs/VotingRegistrationDialogContainer.tsx`,
`source/renderer/app/stores/VotingStore.ts`,
`storybook/stories/governance/DRepDirectory.stories.tsx`,
`tests/jest/governance/GovernanceQueryService.spec.ts` and
`tests/jest/security/governance-sanitization.spec.ts` — the last of which is the
sanitization floor suite every cv-2 row re-runs.

**Method warning for anyone re-verifying:** `.prettierignore` ignores everything
(`/*` at `:2`) and un-ignores only `source/`, `features/`, `storybook/`,
`hardware-wallet-tests/`, `tests/` (`:5-9`). A prettier run on a path outside
those roots (a scratch copy under
`/tmp`, say) is silently **skipped** and exits 0 — an exit-0 run on an out-of-tree
copy is not evidence of cleanliness.

**Resolution.** The formatting rule is stated as a blanket **"format only files
this slice creates"**, not as a four-file exception list, and
`source/renderer/app/containers/voting/Governance.tsx` carries an explicit
prohibition: it may never be batched into a prettier invocation. cv-2 does not edit
that file; if a later slice does, its DoD cannot require a prettier-clean pass on
it — new code there is hand-matched to surrounding style and the file is flagged
for the pre-merge `nix fmt` run.

**Disposition.** Reconciled in the cv-2 PRD's Definition of Done (formatting rule)
and its C-12 row. Record-only for the code — no cv-2 task fixes the drift, because
reformatting 238 pre-existing files would bury the slice's real diff.

**Owner.** Every cv-2 row (as a standing constraint); the pre-merge `nix fmt` pass
is the user's.

---

## F-11 — refreshed cv-2 baselines at `504b44c1a`, measured rather than asserted

Run from the cv-2 worktree root, working tree otherwise clean:

| gate | command | result at HEAD |
|---|---|---|
| typecheck | `node_modules/.bin/tsc --noEmit` | **exit 0**, zero diagnostics (TypeScript 4.9.5) |
| focused Jest | `node_modules/.bin/jest --testPathPattern='(governance\|Governance\|voting\|Voting\|DRep)' --no-coverage --runInBand` | **exit 0** — 17 suites passed, 1 skipped, 18 total; 269 passed, 12 skipped, 281 total; 6 snapshots; ~6-8 s |
| lint | `yarn lint` | **exit 0** with exactly **5591 warnings**, ~38 s |
| formatting | `prettier --check` over `source/`+`storybook/`+`tests/` `*.{ts,tsx}` | **238 dirty files**, exit 2 (see F-10) |

The single skipped Jest suite is environment-gated, not broken:
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts:28` self-skips because
`cardano-cli` is not on PATH in this devcontainer, and its 12 tests are the 12
skipped. `--no-coverage` is load-bearing — `jest.config.js:20` sets
`collectCoverage: true`. `tests/jest` alone is a small minority of the suite; the
governance/voting specs are overwhelmingly colocated under `source/`.

Two standing traps re-confirmed: `yarn check:all` is red at HEAD for unrelated
reasons (`storybook:build`'s manager-webpack JSX loader gap, plus `prettier:check`
per F-10) and must never be read as a cv-2 regression; and `yarn i18n:manage`
**writes** to both locale catalogs and `translations/messages.json`, so anything
that runs it must `git restore` every file that was clean at HEAD.

**Resolution.** These numbers are the comparison basis for every cv-2 gate report.
A slice-close report that says "green" without numbers is not acceptable evidence.

**Disposition.** Record-only; re-measured at slice close and written into the PRD's
"Gates at close" section.

**Owner.** Every cv-2 row's verification step; the Planner at slice close.

---

## F-12 — `nix` is absent in this devcontainer, so `nix fmt` could not run; explicit-path `prettier` is the recorded substitute and `nix fmt` remains a pre-merge obligation the user owns

`command -v nix` returns nothing in this container (measured). The repo's canonical
formatter is therefore unavailable for the whole of cv-2. The substitute used and
recorded is `node_modules/.bin/prettier --write <explicit file paths>` on **files
the slice creates**, never `yarn prettier` — whose script is
`./node_modules/.bin/prettier "**/*.*"` (`package.json:47`, with `prettier:check`
at `:48` chaining onto it), a repo-wide glob that would reformat ~240 unrelated
files — and never a
tool-managed JSON (the tasks tracker, the two locale catalogs,
`translations/messages.json`).

This is the same deviation cv-1 recorded as its F-5; it is re-recorded here because
it applies per-slice and because the substitute is *not* equivalent: prettier 2.1.2
disagrees with the committed formatting on 238 files (F-10) and cannot parse one of
them at all.

Two adjacent environment facts that shape what cv-2 can claim:

- **`gh` and push credentials are absent** — `command -v gh` returns nothing, so no
  PR or push tooling exists here and all cv-2 work stays local.
- **No browser** — the ja-JP visual/overflow pass (task-145 AC-4, task-146 AC-3)
  cannot execute in this container. The longest new string is the badge label
  `Expiring in {n} epochs` / `あと{n}エポックで失効`, which is the specific overflow
  candidate to check.

**Resolution.** Recorded as an explicit deviation rather than papered over: **the
user must run `nix fmt` before merge.** Nothing in cv-2 may report a formatting
gate as fully green on that basis, and the ja-JP visual pass is reported as OWED,
exactly as cv-1 did.

**Disposition.** Deferred to pre-merge (outside any task). Carried in the PRD's
Dependencies section, its Definition of Done, and its OWED-at-close list.

**Owner.** The user, at merge time.

---

## F-13 — invariant 4 holds at HEAD, and the working `same_vote` path spans six sites the corpus cites as one (record-only anchor refinement)

`grep -n "GovernanceStore" source/renderer/app/stores/VotingStore.ts` returns
nothing (exit 1) — `VotingStore` never reads `GovernanceStore`, so locked invariant
4 ("No second delegation backend") holds at HEAD and cv-2 only has to avoid
introducing a `this.stores.governance` read or a second delegation call path. PRD
D-6 keeps it that way: the `drepIndex` read lands in the **container**
(`VotingGovernancePage`), not in `VotingStore`.

The corpus cites the `same_vote` safety net as `VotingStore.ts:61-64`. Live, the
literal `'same_vote'` sits at `VotingStore.ts:62` inside a declaration that opens at
`:61`, and the *working* path needs five more sites:

1. `source/renderer/app/stores/VotingStore.ts:61-65` — the
   `expectedInitializeVPDelegationTxErrors` tuple (`:62` is the literal).
2. `VotingStore.ts:74-81` — the generic `parseApiCode` guard.
3. `VotingStore.ts:348-362` — the `initializeVPDelegationTx` catch that parses the
   code and logs only `{ errorCode }`.
4. `source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:89`
   — `same_vote: messages.initializeTxErrorSameVote` in the intl map.
5. `VotingPowerDelegation.messages.ts:72-73` — the descriptor
   (`voting.governance.initializeTxError.sameVote`).
6. `source/renderer/app/i18n/locales/en-US.json:973` and `ja-JP.json:973` — the
   shipped copy (both **unmarked**, i.e. released copy, not preliminary).

**Resolution.** PRD D-15: task-148 **pins** this path at the store level — where
task-140's client-side disable does not apply — plus a render assertion, and edits
none of the six sites. Any future cv-2 row that adds or reuses an initialize-tx
error code must budget all six.

**Disposition.** Record-only anchor refinement; rides with **task-148** as its
scope statement. No doc is stale — the corpus's `:61-64` is a near-miss, not a
false claim.

**Owner.** task-148.

---

## F-14 — locked invariant 2's ban on the `abstain` / `no_confidence` literal does not hold for the analytics vote kind; cv-2 pins the literal as a required argument, an accepted deviation carried from task-110

Locked invariant 2 reads, literally, that no `abstain` / `no_confidence` literal
may enter a logger, analytics or electron-store payload. The shipped analytics
payload sends exactly that literal: `VotingStore._getVoteKind`
(`source/renderer/app/stores/VotingStore.ts:196-202`) returns
`'drep' | 'abstain' | 'no_confidence'`, and it is the third
`this.analytics.sendEvent(...)` argument at `VotingStore.ts:399-403` (hardware
path) and `:430-434` (software path).

This is not a regression and not new in cv-2. It is the task-110 decision to
reduce the `Casted governance vote` payload to a vote *kind* — a classification
that reveals no DRep identity — already recorded at
`research/slice-3-findings.md:132-141` (F-5), which asks that no later slice
"fix" it in either direction.

**Resolution.** The operative reading of invariant 2 for cv-2, now stated in the
PRD where the invariant is declared (Non-Functional Requirements and
"Locked Invariants Touched" (2)) and in S-9: no DRep identifier and no domain
`Wallet` / `DRepIdentity` object in any logger, analytics or electron-store
payload; no sentinel literal in any **logger** payload; the analytics vote kind
is the one reviewed exception. task-147 AC-5 is scoped to that reading, and no
cv-2 test asserts `'abstain'` is absent from an analytics payload.

**Accepted deviation.** cv-2 goes one step further than slice-3 did: task-147
Step 5's sentinel case pins the literal as a **required** argument —
`expect(analytics.sendEvent).toHaveBeenCalledWith(EventCategories.VOTING, 'Casted governance vote', 'abstain')`
plus a three-argument length assertion. That converts a tolerated shape into a
regression-guarded one, so a future slice cannot quietly widen the payload; it
also means a future decision to drop the sentinel from analytics must update
that test deliberately. Recorded here so the deviation from invariant 2's
literal text is visible rather than buried in a guide judgment call.

**Disposition.** Reconciled in the cv-2 PRD (NFR, invariant 2, S-9, and the
Definition of Done exception list) and in the guide's task-147 resolved judgment
calls and AC-5 record. No source or design file is edited.

**Owner.** task-147.

---

## F-15 — task-143 AC-4 is partial on **both** halves, but F-7 dispositions only the hash half; and no half of it is test-provable, because `jest.config.js`'s `roots` exclude `storybook/` entirely

**Measured at build, not asserted.** The shipped module carries four bech32
constants. `fixtures.ts:36-41` (`VERIFIED_CIP129` / `VERIFIED_CIP105` /
`VERIFIED_CREDENTIAL_HEX`) encodes DRep key hash
`e68fb144f40ed30764fba34ca21cdea2400b1b7f02cb27c04a515bdc`, which is committed at
`research/drep-state-preprod-epoch295-sample.json:2849` — the Cardano Academy
preprod DRep, one of the three provenances the plan names
(`governance-drep-discovery-plan.md:103`,
`designs/current-vote-display-design.md:227`). `fixtures.ts:43-48`
(`UNVERIFIED_*`) is copied byte-for-byte from the repo's own committed story
vector at `CurrentVoteSummary.stories.tsx:17-21`; it is neither the SIPO mainnet
credential nor the canonical CIP-119 example, and cv-2 mints no credential to
close the gap. Both pairs decode cleanly (CIP-129 header byte `0x22`, each CIP-105
partner decoding to the same 28 credential bytes), so what the constants carry is
**bech32 checksum provenance**, not CIP-119 test-vector provenance.

**F-7 under-dispositions this.** F-7's prose already says the committed vectors
are not the CIP-119 vectors AC-4 names (`:474`), but its Resolution's AC-4 clause
(`:492-494`) and its Disposition (`:496-498`) defer only the "with verified hash"
clause to `anchor-1`. The named-provenance clause is the *second* partial and is dispositioned
nowhere in this note. The guide's AC-4 record
(`cv-2-implementation-guide.md:679-696`) is explicit that both shortfalls must be
recorded and that the shortfall must **not** be scoped to the hash half alone.
The task-143 tracker row now records both; this finding is the note-level entry
F-7 was missing.

**Neither half is provable by an executing test — nor is any other cv-2 storybook
task.** `jest.config.js:129` sets `roots: ['<rootDir>/tests', '<rootDir>/source']`,
so a spec placed under `storybook/` never runs. AC-1 through AC-3 are therefore
adjudicated structurally (the guide's Step 3 bech32 decode at `:581-638` and Step 5
grep at `:652-665`) and by static reading, never by an assertion. The same
constraint binds **task-144** and **task-145**, and the visual proof stays owed on
both: the knob renders nowhere until task-145 wires it in, and this container has
no browser (F-12).

**What `anchor-1` inherits, concretely.** Both `makeDRepIndex` entries ship
`anchor: null` (`fixtures.ts:154`, `:164`) — that null is the seam anchor-1 fills.
And the verified fixture already has a **real** anchor committed against the very
same key hash: `drep-state-preprod-epoch295-sample.json:2852-2855` holds
`dataHash` `9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1` and
the `Cardano Academy.jsonld` URL. So task-149/task-150 do not need to mint a
hash-verification vector; one is already in the repo, paired with the fixture that
will consume it.

**Resolution.** Recorded as **two** documented partials on one criterion, not one.
The provenance half is closed only by committing a SIPO mainnet or canonical
CIP-119 credential, which no slice has scheduled; the hash half is closed by
anchor-1. Nothing in cv-2 may report AC-4 green, and a slice-close report that
defers "the hash half" alone repeats the omission this finding exists to fix.

**Disposition.** Provenance half — **open, unscheduled**; carry it into the
anchor-2 planning input rather than letting it lapse. Hash half — **deferred to
`anchor-1` (task-149, task-150)**, as F-7 already states. Coverage constraint —
record-only, binding on task-144 and task-145.

**Owner.** task-143 (recorded); the Planner at slice close (carry-forward);
task-149/task-150 (hash half).

---

## F-16 — task-136 moved the cv-2 Jest baseline off F-11's numbers; the slice sweep is now 276/288 with 9 snapshots, and the guide's Step 6 `25 tests green at HEAD` is HEAD-scoped arithmetic that three readers in a row took for a stale figure

**Measured after task-136's fix pass**, HEAD `0fc92fcab`, working tree carrying
only that task's six paths. The slice-wide sweep F-11 defines —
`node_modules/.bin/jest --testPathPattern='(governance|Governance|voting|Voting|DRep)' --no-coverage --runInBand`
— exits 0 at **17 passed / 1 skipped of 18 suites, 276 passed / 12 skipped of 288
tests, 9 snapshots** (~6.4 s), against F-11's 269 / 12 / 281 and 6 snapshots for
the identical command. The whole delta is task-136's own suite and nothing else:
`CurrentVoteSummary.spec.tsx` carries 4 `it(` blocks at HEAD
(`git show HEAD:… | grep -cE '^\s+it\('`) and 11 in the working copy, and its
stored snapshot keys go 4 → 7 (`grep -c '^exports\['` on
`__snapshots__/CurrentVoteSummary.spec.tsx.snap`) — **+7 tests, +3 snapshots**,
exactly the sweep deltas. The 12 skipped have not moved; they are still
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts:28`'s environment self-skip.

**The neighbour sweep's `25` is right, and each reader who assumes otherwise costs
a round.** The guide's Step 6 comment (`cv-2-implementation-guide.md:1333`) reads
"neighbouring suites must be untouched (3 suites / 25 tests green at HEAD)".
Measured after the change, `--testPathPattern="voting-governance|VotingGovernancePage"`
is 3 suites / **32** tests / 7 snapshots, exit 0. The two reconcile exactly —
`32 - 11 + 4 = 25` — because the parenthetical is scoped **at HEAD**, before this
task's own spec grows. Three independent readers in task-136's build reported the
25 as stale anyway; the review dropped it as a misread in round 1
(`cv-2-code-review.md:1163-1171`) and re-dropped it in round 2 (`:1348-1354`). No
doc edit is owed and none was made.

**Resolution.** F-11 stays the baseline **at `504b44c1a`** and is not amended.
From task-137 onward the comparison basis for the slice-wide sweep is
**276 / 12 / 288 with 9 snapshots**; a row that diffs its own result against
F-11's 269 / 281 / 6 will re-attribute task-136's `+7` and `+3` to itself and
report a regression that does not exist. Every later count in a guide Step 6 is to
be reconciled arithmetically against HEAD *before* it is called a guide defect —
these parentheticals state the pre-change number by design.

**Disposition.** Record-only; binding on every remaining cv-2 row's verification
step, and re-measured at slice close into the PRD's "Gates at close" section, as
F-11's disposition already directs.

**Owner.** task-136 (recorded); every cv-2 row from task-137 onward; the Planner
at slice close.

---

## F-17 — task-138 moved the sweep basis again, to 282/294 with 9 snapshots, by over-delivering two review-mandated cases; and the guide's locked `ItemsDropdown` mock makes any assertion about the *absence* of the DRep input vacuous, which is why branch 2 ships unpinned

**Measured after task-138's round-3 fix pass**, HEAD `31cadffd9`, working tree
carrying `VotingPowerDelegation.tsx`, `VotingGovernancePage.spec.tsx` and the
review log. The sweep F-11 defines and F-16 re-based —
`node_modules/.bin/jest --testPathPattern='(governance|Governance|voting|Voting|DRep)' --no-coverage --runInBand`
— exits 0 at **17 passed / 1 skipped of 18 suites, 282 passed / 12 skipped of 294
tests, 9 snapshots** (~6.4 s), against F-16's 276 / 12 / 288 and the same 9
snapshots. The whole delta is one spec file and no snapshot:
`VotingGovernancePage.spec.tsx` carries 8 `it(` blocks at HEAD
(`git show HEAD:… | grep -c '  it('`) and 14 in the working copy — **+6 tests, +0
snapshots**, exactly the sweep delta. The 12 skipped have not moved; still
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts:28`.

**The `+6` is `4 + 2`, and the `2` is over-delivery, not drift.** The guide's Step
7 comment (`cv-2-implementation-guide.md:2232`) predicts "`VotingGovernancePage.spec.tsx`
grows from 8 to 12 tests" and Step 6(c) prescribes four cases; both are correct
for what the guide asked for. The two extra cases were *mandated by round 2 of
this task's own code review* — CR138R2-2 and CR138R2-3
(`cv-2-code-review.md:2073-2115`), landing as `'leaves a typed DRep id untouched
when a refreshed snapshot carries a new vote'` (`:513-540`) and `'prefers the
wallet current vote over the inherited vote type and DRep id on mount'`
(`:542-559`). So `8 + 4 + 2 = 14`, the focused pattern
`voting-governance|VotingGovernancePage` reads **3 suites / 38 tests / 7
snapshots**, and the unfiltered suite moves `1072 → 1074`. This is **not** the
F-16 failure mode — the guide's parenthetical is not stale arithmetic — so the
guide edit is genuinely owed rather than a misread, and it is **deferred, not
mandated inside task-138**: the per-task DoD allows one commit, and the cv-1
precedent for reconciling a guide against what shipped is its own docs commit
(`a3e352841`).

**Vacuous-by-construction assertions in this spec file.** The DRep input renders
only behind `{selectedWallet && state.selectedVoteType === 'drep' && (<Input`
(`VotingPowerDelegation.tsx:326`), and the guide locks `ItemsDropdownMock` to
`function ItemsDropdownMock(props: { value: string })`
(`VotingGovernancePage.spec.tsx:57-61`) — no handler, and Step 5 names the
`WalletsDropdown` mock as the **only** permitted change. A test therefore cannot
drive the vote type at all. The consequence bit any assertion of the form
"…and the DRep id is *not* displayed": in `'seeds the vote type and no DRep id
from a sentinel on-chain vote'` (`:461-474`), once the first assertion has
established that the dropdown reads `abstain`, the input is not in the DOM and
`queryByDisplayValue(...)` at `:473` returns null on every outcome in which the
first assertion passes. `deriveFormSeed`'s branch 2 (`:119-124`) really does blank
`drepInputState` so a sentinel never enters the id field, and that behaviour is
**untested**. The only permitted seam that observes it is indirect: seed the
sentinel, then `rerenderWithWallets` the same wallet id carrying a `drep` vote and
assert the id appears — red if branch 2 ever leaks a sentinel with `dirty: true`,
because the `:180` guard would then suppress the re-seed. That is a partial pin
only (a leak with `dirty: false` still re-seeds), which is why task-138 recorded
the gap instead of taking it (CR138R3-2, `cv-2-code-review.md:2331-2356`).

**Resolution.** From task-139 onward the comparison basis for the slice-wide sweep
is **282 / 12 / 294 with 9 snapshots**, superseding F-16's 276 / 288; a row that
diffs against F-16's numbers will re-attribute task-138's `+6` to itself and
report a regression that does not exist. F-16's rule still governs the *reading*
of every guide parenthetical — reconcile arithmetically against HEAD before
calling it a defect — but this one reconciles to `12`, not `14`, and the residue
is a real doc edit. Any cv-2 task that adds cases to
`VotingGovernancePage.spec.tsx` (task-139 and task-140 both will) must assume it
cannot select a vote type from a test, and must not write a negative assertion
about the DRep input's contents believing it discriminates.

**Disposition.** Sweep basis — record-only, binding on every remaining cv-2 row's
verification step and re-measured at slice close, as F-11 and F-16 already direct.
Guide reconciliation (Step 7's `8 to 12`, Step 6(c)'s case list, and the AC-3
acceptance line at `:2257` naming only "Step 6 case 4" as the pin) — **deferred to
the slice-close doc pass**, as its own commit. Branch 2's missing pin and the
locked vote-type mock — **open**; closing it properly needs either a permitted
handler on `ItemsDropdownMock`, which is a guide change, or the indirect
re-seed case above, which pins only half.

**Owner.** task-138 (recorded); task-139 and task-140 (both add cases to this
file); the Planner at slice close (sweep basis re-measure and the guide
reconciliation commit).

---

## F-18 — the jsdom/Node realm split makes `resolveExactDRepMatch` return `null` in an **unshimmed** component spec, so the mounted panel reads `unavailable` throughout `VotingGovernancePage.spec.tsx`; `DRepDirectory.spec.tsx` already defeats it with a three-line global shim, so the `drepIndex` → `drepEntry` → badge chain *is* pinnable in jsdom; task-139's mount is otherwise test-neutral (+0 tests, +0 snapshots)

**Measured at task-139's build**, HEAD `144c5153d`, working tree carrying only
`VotingPowerDelegation.tsx` and `VotingGovernancePage.tsx`. The slice-wide sweep
F-17 re-based —
`node_modules/.bin/jest --testPathPattern='(governance|Governance|voting|Voting|DRep)' --no-coverage --runInBand`
— exits 0 at **17 passed / 1 skipped of 18 suites, 282 passed / 12 skipped of 294
tests, 9 snapshots** (~6.5 s), **identical** to F-17's post-task-138 basis. The
focused pattern `voting-governance|VotingGovernancePage` is 3 suites / 38 tests /
7 snapshots, also unchanged, and `tsc --noEmit` exits 0. task-139 adds no spec
file by design (`cv-2-implementation-guide.md:2398`), so the comparison basis for
task-140 onward stays **282 / 12 / 294 with 9 snapshots**.

**The one predicted regression did not occur.** The guide's by-eye check
(`cv-2-implementation-guide.md:2578-2589`) expects the mounted panel to inject a
`noDelegation` heading, a `role="alert"` paragraph and a `!!!Choose a delegation`
button into `VotingGovernancePage.spec.tsx`'s DOM, with a risk of query
ambiguity. All 14 cases pass unedited; no query was weakened and none needed
re-pointing.

**The panel's badge is dead on arrival in an unshimmed jsdom spec.**
`jest.config.js:147` sets `testEnvironment: 'jest-environment-jsdom'` with no
`projects` and no `testEnvironmentOptions` override, and `resolveExactDRepMatch`
canonicalizes through `Cardano.DRepID.toCip129DRepID` inside a `try` whose
`catch` returns `null` (`helpers.ts:146-151`). Under that environment the call
**throws** — `radix2.encode input should be Uint8Array` — because `Buffer` and
`Uint8Array` come from different realms and the SDK's bech32 encoder brand-checks
its argument. `Cardano.DRepID.isValid` (`helpers.ts:144`) returns `true` first,
so the failure is silent: the function returns `null` for a canonical CIP-129 id
that **is** a key in the map. Reproduced in this worktree with a throwaway
default-environment spec, deleted after the run:
`toCip129DRepID(Cardano.DRepID(VALID_DREP_ID))` threw with that message and
`resolveExactDRepMatch(VALID_DREP_ID, new Map([[VALID_DREP_ID, entry]]))`
returned `null`. `helpers.spec.ts:1-6` carries `@jest-environment node` for the
same reason, with the docblock "The node environment keeps Buffer in the same
realm as Uint8Array, which the SDK's bech32 encoder brand-checks", which is why
its positive cases (`helpers.spec.ts:188-195`) are green.

**A component spec can still pin the positive chain, and the precedent is
in-repo.** `DRepDirectory.spec.tsx` runs in the default jsdom environment — it
carries no `@jest-environment` docblock, its first line is
`import React from 'react';` — and repoints the suite's global at Node's realm at
module scope (`:23-26`):

```ts
(global as { Uint8Array: unknown }).Uint8Array = Object.getPrototypeOf(
  Buffer.prototype
).constructor;
```

That is why its exact-match cases are green. `it('opens the detail view once for
an exact CIP-129 match')` (`:528`) and `it('canonicalizes an exact CIP-105 match
to the CIP-129 detail id')` (`:540`) each assert `onViewDetails` fired once with
`realDrepId(1)`, and the only non-click call site is the `resolveExactDRepMatch`
effect at `DRepDirectory.tsx:192-198` — so they pass only because the
canonicalization succeeded and the canonical key hit the map. Verified by running
the CIP-105 case alone: 1 passed, 46 skipped. Re-running the throwaway probe with
that same shim at its module scope flipped both measurements —
`toCip129DRepID` returned the input id unchanged and the lookup returned the
entry. So `@jest-environment node` is **not** the only route; the constraint is
"shim the realm or the lookup is silently `null`", not "no component spec can pin
it".

**The consequence is already observable in the container spec.** `buildStores`
supplies `drepIndex: new Map([[VALID_DREP_ID, drepEntry]])`
(`VotingGovernancePage.spec.tsx:128` — the guide's `:89` is pre-task-138
numbering) with `drepActivity: 12` (`:105`) and `status: 'active'` (`:107`), and
`votingWallet` delegates to that exact id (`:91`). With the panel mounted those
renders should produce badge state `expiring` (`CurrentVoteSummary.tsx:19`,
`:29-37`). They do not: the run logs
`voting.governance.currentVote.status.unavailable` **7 times and
`…status.expiring` / `…status.expiringBadge` zero times**, so
`currentDRepEntry` (`VotingPowerDelegation.tsx:209-215`) is `null` on every
render. `CurrentVoteSummary.spec.tsx` passes `drepEntry` in directly, runs no
lookup, and does log the expiring ids — so the ids really are absent from
`en-US.json` until task-146 and the two counts discriminate. The `7` is not a
clean count of the trap: `:532` re-renders the wallet with
`drep.raw = OTHER_DREP_ID`, which `buildStores`' one-entry index deliberately
does not key, so at least one of the seven is a correct `unavailable` that the
realm shim would not change.

**task-173's stated pin needs the shim, not a re-plan.** The guide
(`cv-2-implementation-guide.md:5307-5314`) calls its two badge assertions and its
CIP-105 case "the slice's **only** executable pin on the `drepIndex` →
`drepEntry` → badge chain" and justifies them on `buildStores` already supplying
the index. Against `VotingGovernancePage.spec.tsx` as it stands every one of them
reads `!!!DRep status is loading.` and fails, because that file carries no realm
shim. The available fix is the three lines at `DRepDirectory.spec.tsx:23-26`, at
that file's module scope. The alternatives are worse and are recorded only to
close them off: `@jest-environment node` cannot host a
`@testing-library/react` render, changing the canonicalization is a production
edit no cv-2 row owns, and a `jest.mock` of the helpers module makes the
assertion vacuous. None of this is task-139's to choose.

**AC-3's struck clauses.** The findings-note half of task-139's recording
obligation (`cv-2-implementation-guide.md:2610-2612`) was discharged at planning
time by **F-2**, which names `GovernanceStore.ts:20-31` as the evidence and defers
the `givenName` read and the unverified→verified story to `anchor-2`; nothing is
restated here. The tracker half — the `acceptanceCriteria` string that opens
"The `drep` state reads `givenName` from
`GovernanceStore.drepIndex[drepId]?.givenName`" (currently
`governance-drep-discovery-plan-tasks.json:1267`; the guide's `:1241` is
pre-137/138 numbering) and the matching `statusReason` — was **not** made in the
build and was completed in the task-139 review-fix pass.

**Resolution.** Sweep basis unchanged at 282 / 12 / 294 with 9 snapshots. The
jsdom realm trap is a **standing constraint on every cv-2 component spec that
exercises the lookup**, not a task-139 defect: in a spec with no realm shim, any
assertion that a badge, name or status reached the DOM *through*
`resolveExactDRepMatch` is guaranteed to read `unavailable`, and an assertion
that it reads `unavailable` is guaranteed to pass for the wrong reason. The
standing rule for the rest of cv-2: a component spec that asserts anything on the
far side of the lookup installs the `DRepDirectory.spec.tsx:23-26` shim at module
scope first.

**Disposition.** Sweep basis — record-only, binding on task-140 onward.
jsdom realm trap — **open** on `VotingGovernancePage.spec.tsx` and blocking on
task-173's Step 6 as written; it must be resolved there, not deferred, because
the guide names those cases as the slice's only pin, and the remedy is three
lines with in-repo precedent. AC-3 tracker edit — **discharged** in the task-139
review-fix pass.

**Owner.** task-139 (recorded); task-173 (installs the realm shim in
`VotingGovernancePage.spec.tsx` before its badge cases can pass); task-147 (any
end-to-end badge assertion inherits the same constraint).

---

## F-19 — the guide's three tracker-JSON line anchors are pre-slice numbering and drift further with every row that completes; task-140's Step 10 target is already 34 lines off, and F-6's "its `path:line` anchors have not" does not extend to the one anchored file cv-2 writes to as it goes

F-6 measured the corpus's `path:line` anchors against **source** files and found
them sound. `governance-drep-discovery-plan-tasks.json` is the exception it could
not have caught: cv-2 writes to that file once per task, and every row that closes
gains `statusReason`, `evidence` and `updatedAt`, so each completed task pushes
every later anchor down. The guide states the pre-slice caveat for its source
anchors (`cv-2-implementation-guide.md:2290-2291`, "Every line number below is the
pre-slice (`504b44c1a`) number … **Re-anchor by the quoted content, never by the
number.**"), but its three tracker anchors are given as bare line numbers with no
such warning, and two of them are instructions to edit *one specific line*.

Measured, the `504b44c1a` copy against the working tree at the close of task-139:

| guide site | what it says | pre-slice | at `144c5153d` | live now | offset |
| --- | --- | --- | --- | --- | --- |
| `:2316`, `:2529` (task-139 Step 6) | `line 1241` — task-139's own AC-3 string | 1241 | 1267 | **1275** | +34 |
| `:2641`, `:3110` (task-140 Step 10) | "**line 1263 only**" — task-140's own AC-7 | 1263 | 1297 | **1297** | +34 |
| `:3112` (task-140 Step 10) | "Leave line 1283" — task-173's AC | 1283 | 1317 | **1317** | +34 |

The trap is the middle column. An anchor moves **again** when the reader's own row
closes: F-18 above pins task-139's AC-3 string at `:1267`, which was exact when it
was written and went stale eight lines later in the same pass, the moment
task-139's `statusReason` / `evidence` / `updatedAt` were written above it. Both
numbers describe the same string; neither is wrong about the commit it was measured
at. The file is 2046 lines at `504b44c1a` and 2087 now, and the `cv-2` phase object
this file's own References section cites as `:1162-1457` is live at `:1162-1498`,
by the same +41.

Both live anchors resolve unambiguously by content: `:1297` is the task-140 AC-7
string opening "The comparator sentence of
`designs/current-vote-display-design.md:95`", and `:1317` is the task-173 AC string
opening "The first sentence of `designs/current-vote-display-design.md:95`" — the
same design-doc line under two different owners, which is why the guide already
tells task-140 to re-read that line at edit time (`:3112`).

**Resolution.** Re-anchor by the quoted content, never by the number — the guide's
own source-file rule, which binds harder on the tracker because the slice itself is
what moves it. The numbers above are recorded as a measurement, not as a correction
to chase: task-140's row will push `:1317` down again before task-173 reads it, so
neither F-18's `:1267`, nor the guide's anchors, nor this file's References line is
rewritten here. A tracker edit that lands on the wrong line silently corrupts a
sibling task's acceptance criteria and `node -e "JSON.parse(…)"` still passes, so
after any tracker edit re-read the changed string and confirm it is the one the
guide quotes.

**Disposition.** Record-only, binding on task-140 onward.

**Owner.** task-139 (recorded); task-140 (Step 10 touches two of the three
anchors); task-173 and every later task that closes a row.

---

## F-20 — task-140 moves the sweep basis to 291/303 with 9 snapshots through a standalone unit spec, not the component file; F-17's "task-139 and task-140 both add cases to `VotingGovernancePage.spec.tsx`" holds for neither, and its locked-mock caution re-homes untouched to the rows that actually edit that file

F-17 set the comparison basis at **282 / 12 / 294 with 9 snapshots** and named
task-139 and task-140 as "both add cases to this file", meaning
`VotingGovernancePage.spec.tsx` (`:1000-1009`, `:1020-1022`). Measured at the close
of task-140, neither row did. task-139's mount was test-neutral at +0 tests and +0
snapshots (F-18's own headline), and task-140's +9 lands in a file that is not
`VotingGovernancePage.spec.tsx` at all.

Measured in the recording pass for task-140,
`jest --testPathPattern="(governance|voting)" --no-coverage --runInBand`:

| | suites | tests | snapshots |
| --- | --- | --- | --- |
| F-17 basis (task-138 close) | 17 passed + 1 skipped of 18 | 282 passed + 12 skipped of 294 | 9 |
| live (task-140 close) | **18 passed + 1 skipped of 19** | **291 passed + 12 skipped of 303** | **9** |
| delta | +1 | +9 | +0 |

The whole delta is one new file, `tests/jest/governance/isSameVoteTarget.spec.ts`.
The cause is the slice's file-ownership map, not an under-delivery: the guide
assigns the rendered end-to-end disabled-submit flow to **task-147**, twice and
explicitly — "task-140 owns the comparator's unit vectors only"
(`cv-2-implementation-guide.md:2745-2747`) and again in the AC-1 and AC-4
acceptance records (`:3164-3166`) — so task-140's entire proof surface is a
pure-function unit spec and `VotingGovernancePage.spec.tsx` is byte-untouched by
this row.

Two consequences a later reader will otherwise misread:

1. **F-17's second half never bound task-140.** Its caution — that the locked
   `ItemsDropdown` mock means a test cannot select a vote type, so any negative
   assertion about the DRep input's contents is vacuous — was addressed to the two
   rows F-17 expected to edit that spec. It is still entirely unspent, and it
   re-homes to the rows that do edit it: task-173 and task-141
   (`cv-2-implementation-guide.md:3190-3191`), then task-147's end-to-end case.
2. **The spec is invisible to source-scoped patterns.** It sits in
   `tests/jest/governance/` beside `normalizeDRepIdentity.spec.ts` rather than
   colocated with its util under `source/renderer/app/utils/governance/`. That is
   the convention for this util directory rather than a deviation — the sibling
   helper splits the same way — but it means `--testPathPattern="voting-governance"`
   does not see the +9, and only the `governance`-scoped and slice-wide patterns do.

**Resolution.** From task-141 onward the comparison basis for the slice-wide sweep
is **291 / 12 / 303 with 9 snapshots**, superseding F-17's 282 / 12 / 294. F-16's
reading rule still governs every guide parenthetical — reconcile arithmetically
against HEAD before calling a number a defect. F-17's Owner line is **not**
rewritten: F-16, F-17 and this finding are a chain of what was measured when, read
forward, and amending an earlier link would destroy the audit trail the chain
exists for. A reader who diffs `VotingGovernancePage.spec.tsx` looking for
task-140's cases finds none; that is the design, not a gap.

**Disposition.** Record-only; binding on every remaining cv-2 row's verification
step and re-measured at slice close, as F-11, F-16 and F-17 already direct. F-17's
locked-mock item stays **open** and is re-homed, not closed.

**Owner.** task-140 (recorded); task-141 onward (basis); task-173, task-141 and
task-147 (inherit F-17's locked-mock caution whole); the Planner at slice close
(basis re-measure).

---

## F-21 — F-18 declared its `Uint8Array` realm shim **blocking on task-173**, and at task-173's build it is not applicable: that row decodes through the `bech32` package, never through the SDK's realm-branded encoder; and AC-2's rendering half leaves a bounded interim gap in which a legacy 28-byte `drep1…` id renders the generic *Delegate to DRep* label instead of its own string

**The shim was predicted, not needed.** F-18 dispositioned its jsdom realm trap
**open** on `VotingGovernancePage.spec.tsx` and "blocking on task-173's Step 6 as
written; it must be resolved there, not deferred" (`:1141-1146`), and its Owner
line names task-173 as the row that "installs the realm shim … before its badge
cases can pass" (`:1148-1150`). Measured at task-173's build, the row ships **no
badge case** and never calls `resolveExactDRepMatch`. Both its cases assert the
`drepIdentity` prop handed to the confirmation dialog
(`VotingGovernancePage.spec.tsx:614-625`, `:627-634`), and that value comes from
`normalizeDRepIdentity` (`VotingGovernancePage.tsx:87`), whose decode path is
`bech32.decode` / `bech32.fromWords` from the `bech32` npm package
(`normalizeDRepIdentity.ts:1`, `:21-23`), returning a plain `number[]`. Nothing on
that path constructs or brand-checks a `Uint8Array`, so the realm split F-18
measured through `Cardano.DRepID.toCip129DRepID` (`helpers.ts:145-152`) is not on
it at all. `grep -n "Uint8Array"` over the spec exits 1 and the suite is 16 of 16
green. F-18's thesis is unharmed — it was scoped to specs that assert on the far
side of the lookup, and this row is not one.

**The interim rendering gap is real and narrower than "sentinel labels".** The
guide partitions AC-2 across two commits (`cv-2-implementation-guide.md:3268-3276`,
`:3505-3509`): task-173 proves only that a `null` identity leaves the submitted
string byte-equal, and task-175 Step 3 owns the dialog's branch predicate. What the
guide does not state is what the dialog does in between, and it is not the sentinel
label a reader infers from "would fall into the sentinel label branch". With
`drepIdentity` `null` the dialog takes the `else` arm at
`VotingPowerDelegationConfirmationDialog.tsx:163-172` and renders
`mapVoteToIntlMessage(chosenOption)`, whose `default` case (`:31-40`) returns
`sharedGovernanceMessages.delegateToDRep`. So the confirmation screen for such an
id shows neither the id nor a wrong sentinel: a correct but generic *Delegate to
DRep* label, with the id withheld and the submitted bytes unaffected. Reachability
is narrow and was measured, not assumed: only the legacy 28-byte `drep1…` form
reaches it, because `Cardano.DRepID.isValid` accepts it at the form gate
(`VotingPowerDelegation.tsx:221`, live anchor — the guide's `:133` is pre-slice
numbering, as its own re-anchoring rule at `:3192-3195` warns) while
`normalizeDRepIdentity` rejects it on payload length
(`normalizeDRepIdentity.ts:27-30`). The guide's checksum-verified vector
`drep1pu0z60z…` decodes here to 28 bytes under the `drep` HRP.

**Resolution.** F-18's blocking item is **not applicable to task-173**, and no
shim was installed; the trap itself stands unchanged for any spec asserting past
`resolveExactDRepMatch`. The AC-2 gap is bounded and deliberately timed: it opens
at task-173's commit and closes at task-175 Step 3, whose judgment call 1 names the
predicate fix (`cv-2-implementation-guide.md:3886-3892`). The two rows in between
are test-only pins (`:3200-3201`), so nothing renders that branch differently in
the window, and task-173 is forbidden the dialog file, so nothing is owed earlier.

**Disposition.** Shim — **re-homed, not closed**. F-18's Owner line stays as
written, because F-18 and this finding are a chain of what was predicted versus
what was measured, and amending the earlier link would destroy that trail. Interim
gap — **open until task-175**; AC-2's rendering half must not be reported green at
slice close on task-173's evidence.

**Owner.** task-173 (recorded); task-175 (closes AC-2's rendering half via the
branch predicate); task-147 and any later component spec that asserts through
`resolveExactDRepMatch` (inherit F-18's realm shim whole).

---

## F-22 — task-141's pin makes the guide's own Step 1 premise grep self-hitting, and it is the third `describe` to land after the HW block, so task-147's "at the end, after the HW describe" placement instruction now names two different places; sweep basis moves to 295/307

**The premise check inverts after the edit.** task-141 Step 1
(`cv-2-implementation-guide.md:3561-3568`) says to run
`grep -nE "previousVote|newVote|previousDRepId|historicalVote" source/renderer/app storybook -r`
and, if it prints anything, "stop and report — the row's premise has changed." Run
before the edit it was silent, which is what licensed the row. Run after the edit it
returns exactly one line,
`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:673`, which is
the negative assertion's own forbidden-key list. The hit is the pin naming what it
forbids, not a leak: the terms appear inside
`['previousVote', 'newVote', 'previousDRepId', 'currentVote'].forEach(…)`, an array
of string literals handed to `expect(props).not.toHaveProperty(key)`. Two details a
re-runner needs. First, the fourth literal in the code is `currentVote`, **not** the
`historicalVote` the Step 1 pattern lists — `historicalVote` appears in no source
file at all (a repo-wide grep outside `.agent/` exits 1), so the grep's fourth
alternative is and stays unmatched. Second, the
correct post-task-141 form of the premise check is the same grep with the spec file
excluded, or `grep` over the production tree only; `VotingGovernancePage.tsx` and
every other production file remain silent.

**The placement instruction task-147 inherits no longer resolves.** task-147 Step 2
(`cv-2-implementation-guide.md:5188`) says: "Add at the **end** of
`VotingGovernancePage.spec.tsx`, after the HW describe's closing `});`". Those were
the same location when the guide was written. They are not now. The file's top-level
blocks at this commit are `DRep selection handoff via location.state` (`:251`),
`Hardware-wallet delegate flow via location.state handoff` (`:390`), `Delegation form
pre-fill from the selected wallet` (`:479`, task-138), `Confirmation dialog identity
derivation` (`:599`, task-173) and `Confirmation dialog prop contract` (`:637`,
task-141). Three describes now sit between the HW block and the end of the file, so
"after the HW describe" would insert at `:479` and split the later blocks apart.
**Append at the end of the file** is the instruction that survives; the HW clause is
a stale synonym and must be ignored. The same reading applies to task-148 if it
appends to this file.

**A guide `path:line` anchor drifted from work inside this slice.** F-6 concluded
that the corpus's prose anchors had drifted while its `path:line` anchors had not,
and F-19 narrowed the exception to the tracker JSON. This row adds a second, different
exception: task-141's Context cites `VotingGovernancePage.tsx:85-111` for the ten-prop
JSX (`:3534`) and Step 2's preamble cites `:85-111` again (`:3344`), but the live span
is `:89-114` — `chosenOption` at `:90` through `selectedWallet` at `:114`. The four-line
shift is task-173's own doing (the `normalizeDRepIdentity` import plus the retained
sentinel guard), i.e. a guide anchor invalidated by an earlier row of the same slice,
not by pre-slice drift. The prop list itself is unchanged, so the anchor is stale and
not wrong. `VotingPowerDelegationConfirmationDialogProps` at
`VotingPowerDelegationConfirmationDialog.tsx:54-70` is still exact: those ten props
plus `intl`, which `injectIntl` supplies.

**Sweep basis.** Measured in this recording pass,
`jest --testPathPattern="(governance|voting)" --no-coverage --runInBand`:

| | suites | tests | snapshots |
| --- | --- | --- | --- |
| F-20 basis as moved by task-173 | 18 passed + 1 skipped of 19 | 293 passed + 12 skipped of 305 | 9 |
| live (task-141 close) | **18 passed + 1 skipped of 19** | **295 passed + 12 skipped of 307** | **9** |
| delta | +0 | +2 | +0 |

The whole delta is this row's two cases; no suite was added and no snapshot written.

**Resolution.** From task-142 onward the slice-wide sweep basis is **295 / 12 / 307
with 9 snapshots**, superseding F-20's 291 / 303 and task-173's 293 / 305. F-16's
reading rule still governs every guide parenthetical. Step 1's grep for this row is
**spent** — it must not be re-run as a gate after the edit without excluding the spec
file. F-6's and F-19's Owner lines are **not** rewritten; this finding is the next link
in that chain, and amending an earlier link would destroy the trail.

**Disposition.** Record-only for the anchor and basis items. The task-147 placement
ambiguity is **open and blocking on task-147's Step 2**: it must append at the end of
the file, not after the HW describe. One forward note, out of task-141's row by AC-1's
own scope and deliberately not widened here: the pin covers the dialog's **prop set**
only, so a future history feature that reached the dialog through
`selectedWallet.currentVote` rather than through a new prop would not trip it. That
surface belongs to task-142 (dialog layout) and task-175 (dialog identity block).

**Owner.** task-141 (recorded); task-147 and task-148 (append at end of file, ignore
the HW clause; inherit the corrected Step 1 grep form); task-142 onward (sweep basis);
the Planner at slice close (basis re-measure, and the guide anchor `:85-111` → `:89-114`
if the guide is ever reconciled).

---

## F-23 — Section 4's blanket promise that its snippets are already prettier-formatted is false, and the line it breaks on next is in task-175's snippet; plus one assertion task-142 ships knowingly vacuous. Sweep basis moves to 299/311

**The formatting warranty does not hold.** `cv-2-implementation-guide.md:3205-3208`
tells every Section 4 row to match the surrounding style by hand because
`prettier --check` is already red at HEAD on three of the files it touches, and then
warrants that "the snippets below are already formatted to prettier 2.1.2's 80-column
output". Measured across every fenced `tsx` / `ts` block in Section 4 (`:3188-4216`),
sixteen lines exceed 80 columns. Thirteen are false positives — import paths, `it(…)`
and `describe(…)` name strings, and template literals, none of which prettier can
break, so it prints them over-width and `--check` stays green. **The discriminator is
whether prettier has a break point inside the line, not the raw width.** Three lines
have one:

| guide line | cols | what prettier does | owner |
| --- | --- | --- | --- |
| `:3739` | 85 | moves the `queryByText` string onto its own argument line | task-142 |
| `:3750` | 83 | same, for `getByText` | task-142 |
| `:4122` | 81 | splits `Cardano.DRepID.toCredential(Cardano.DRepID(cip129))` across three lines | **task-175** |

task-142's two were hand-wrapped at
`VotingPowerDelegationConfirmationDialog.spec.tsx:187-191` and `:200-204`, and
`prettier --check` now reports the whole file clean at exit 0. task-175's was verified
empirically rather than by eye: fed the `:4122` line at the six-space indent its
`it.each` nesting actually produces (`:4113-4131`), prettier 2.1.2 emits

```tsx
      const { hash, type } = Cardano.DRepID.toCredential(
        Cardano.DRepID(cip129)
      );
```

**task-175 must apply that break by hand.** It appends to the same spec file, which is
pre-existing and currently green, so `prettier --write` is unavailable to it by the
slice's own formatting rule and `--check` is the only safe instrument. Pasting `:4122`
verbatim turns a green file red.

**One assertion is knowingly vacuous, and the pattern must not be copied.** The
software-wallet case at `VotingPowerDelegationConfirmationDialog.spec.tsx:187-191`
asserts the device-status copy is absent. It can never be present, for two independent
reasons. `renderDialog` (`:30-54`) defaults `selectedWallet` to `softwareWallet`
(`:19-23`), so the `selectedWallet.isHardwareWallet ?` branch at
`VotingPowerDelegationConfirmationDialog.tsx:179-185` never mounts at all; and even
if it did, the harness's default `HwDeviceStatuses.READY` resolves through
`HardwareWalletStatus` to *Device ready* (`en-US.json:1145`), a different string from
the one queried. Its two neighbours at `:185-186` fail correctly, so the *case* is
sound and only the third expectation is dead. It ships as written because the guide
prescribes it verbatim (`:3736-3742`) and it hides no regression. The rule for anyone
extending this file: **a mirrored absence assertion is only load-bearing when the
override actually flips the branch.** The hardware case at `:194-207` is the one that
carries weight, and it earns it by overriding `selectedWallet` *and* `hwDeviceStatus`
together — its `Spending password` absence at `:206` genuinely proves branch
exclusivity.

**F-22's forward note is now half-closed.** F-22 handed task-142 the question of a
history feature that reached the dialog through `selectedWallet.currentVote` rather
than through a new prop, which task-141's prop-set pin could not see. The chrome case's
DOM-level `queryByText(/previous vote/i)` and `queryByText(/new vote/i)` at `:215-216`
do see it, whatever the data source. What stays open is a comparison row carrying
neither label; the descriptor half at `:217-218` is exact by contrast, because
`VotingPowerDelegationConfirmationDialog.messages.ts` exports exactly eight descriptors
— `title`, `vote`, `drepId`, `fee`, `password`, `errorGeneric`, `buttonCancel`,
`buttonConfirm` — and task-175 adds only `drepIdCip105` and `signedPayload`
(`cv-2-implementation-guide.md:3917-3928`), so neither forbidden key can appear by
accident.

**Sweep basis.** Measured in this recording pass,
`jest --testPathPattern="(governance|voting)" --no-coverage --runInBand`:

| | suites | tests | snapshots |
| --- | --- | --- | --- |
| F-22 basis at task-141 close | 18 passed + 1 skipped of 19 | 295 passed + 12 skipped of 307 | 9 |
| live (task-142 close) | **18 passed + 1 skipped of 19** | **299 passed + 12 skipped of 311** | **9** |
| delta | +0 | +4 | +0 |

The whole delta is this row's four cases; no suite was added and no snapshot written.
The focused run `--testPathPattern=VotingPowerDelegationConfirmationDialog` is 17 of 17
in one suite, 13 pre-existing plus 4.

**Resolution.** From task-175 onward the slice-wide sweep basis is **299 / 12 / 311
with 9 snapshots**, superseding F-22's 295 / 307. Section 4's formatting warranty at
`:3205-3208` is **not trustworthy as written** — measure the snippet before pasting and
close every row with `prettier --check` (never `--write`) on the file it appended to.
F-6's conclusion that the corpus's `path:line` anchors had not drifted is untouched by
this finding; the defect here is a factual claim in guide prose, not an anchor. AC-3's
bogus `~L118-L127` citation, which F-6 already recorded, is re-anchored in the task-142
tracker row to the semantic `isHardwareWallet` branch as the guide's Acceptance block
(`:3797-3801`) requires.

**Disposition.** The `:4122` hand-wrap is **open and blocking on task-175's Step 3**.
The vacuous assertion is **accepted and closed** — record-only, no change requested.
The sweep basis and the F-22 half-closure are record-only.

**Owner.** task-142 (recorded); task-175 (the `:4122` break, and the sweep basis);
task-147 and task-148 (the same measure-before-pasting rule, since Section 4 is not the
only place the guide makes that promise); the Planner at slice close (basis re-measure,
and the `:3205-3208` warranty if the guide is ever reconciled).

---

## F-24 — a `DRepIdentity` never holds four distinct strings: `normalizeDRepIdentity` aliases `raw` onto whichever form it was given, so the guide's Step 4 template renders the same bech32 twice for a CIP-105 input; plus the on-chain label's ja-JP copy is not what the guide's prose says. Sweep basis moves to 309/321

**The aliasing is by construction, and it cuts both ways.**
`source/renderer/app/utils/governance/normalizeDRepIdentity.ts` has two success
branches. For a CIP-129 `drep1…` input it returns `raw` and `cip129: raw` (`:38-40`)
and computes `cip105` freshly. For a `drep_vkh…` / `drep_script…` input it returns
`raw` and `cip105: raw` (`:54`, `:56`) and computes `cip129` freshly. So of the four
string fields, **exactly one is always an alias of `raw`**, and which one depends on
the input encoding. Nothing in the type says so — every field is a non-optional
`string` — and no producer can avoid it, since both live call sites go through this
helper (`VotingGovernancePage.tsx:87`, `Governance.stories.tsx:59-62`).

**What that did to the guide's template.** task-175's Step 4 block
(`cv-2-implementation-guide.md:3961-4013`) is a *replace with exactly* block, and it
gates the CIP-105 section on `drepIdentity?.cip105 &&` alone. Pasted verbatim, a DRep
selected by its CIP-105 form renders its own bech32 string twice — once under
*!!!DRep ID* and once under *!!!CIP-105 DRep ID* — which is not the §7 template and
reads as two different ids to a user comparing against a device. Code review filed it
as CR175-1 and the live guard at
`VotingPowerDelegationConfirmationDialog.tsx:170` is one conjunct longer:

```tsx
{drepIdentity?.cip105 && drepIdentity.cip105 !== drepIdentity.raw && (
```

That inequality is the only deviation from the whole template — `diff -u` of the guide
block against the live block (`tsx:157-209`) returns exactly that one hunk — and it is
pinned in both directions by the case at
`VotingPowerDelegationConfirmationDialog.spec.tsx:296-309`, whose
`getAllByText(SCRIPT_CIP105)).toHaveLength(1)` would read `2` if the guard were
reverted.

**The rule anyone rendering a `DRepIdentity` inherits.** Any surface that shows more
than one form must suppress the alias by value comparison, not by presence. That
lands squarely on anchor-2: `DRepIdDisplay`'s dual-form mode (task-154's extension of
this same block) will hit the identical case, and so will any future summary or detail
view that prints CIP-129 and CIP-105 side by side. A `cip129 !== raw` guard is the
mirror-image need for a CIP-105-primary layout.

**A second, smaller doc error, at a line every later row reads.** The task-175 Context
paragraph (`cv-2-implementation-guide.md:3854`) states that `DRepSourceLabel`'s
`'on-chain'` copy is `!!!On-chain` "in both catalogs (`en-US.json:354`,
`ja-JP.json:354`)". `en-US.json:354` is `"governance.drepDirectory.source.onChain":
"!!!On-chain"`; `ja-JP.json:354` is `"governance.drepDirectory.source.onChain":
"!!!オンチェーン"` — already translated, still `!!!`-marked as preliminary. No code or
test impact today, because the dialog spec's `IntlProvider` loads `en-US.json` only
(`spec.tsx:11`, `:42`), but the claim must not be reused as catalog truth by task-146
or by any later row that asserts ja-JP copy.

**Sweep basis.** Measured in this recording pass,
`jest --testPathPattern="(governance|voting)" --no-coverage --runInBand`:

| | suites | tests | snapshots |
| --- | --- | --- | --- |
| F-23 basis at task-142 close | 18 passed + 1 skipped of 19 | 299 passed + 12 skipped of 311 | 9 |
| live (task-175 close) | **18 passed + 1 skipped of 19** | **309 passed + 12 skipped of 321** | **9** |
| delta | +0 | +10 | +0 |

The whole delta is this row's ten cases — four plain `it`s plus three `it.each`
tables of two — in the existing dialog spec; no suite was added and no snapshot
written. The focused run `--testPathPattern=VotingPowerDelegationConfirmationDialog`
is **27 of 27** in one suite, 17 pre-existing plus 10. F-23's `:4122` hand-wrap item
is **closed**: the break was applied at `spec.tsx:340-342` and `prettier --check`
reports that file clean.

**One interim state a reader will meet before task-146.** The two new descriptors
exist with no catalog key, so the dialog suite logs `[React Intl] Missing message` for
`voting.governance.confirmationDialog.drepIdCip105` and `.signedPayload` on every
render. That is D-9's deliberate split (`cv-2-implementation-guide.md:188-200`), not a
defect, and the assertions match the `!!!`-prefixed `defaultMessage` react-intl falls
back to. They survive task-146 because the seeded values keep the `!!!` prefix
(`cv-2-PRD.md:685-686`, invariant 11 at `:1510-1512`).

**Resolution.** From task-175 onward the slice-wide sweep basis is **309 / 12 / 321
with 9 snapshots**, superseding F-23's 299 / 311. The alias rule is authoritative over
the guide's template: where a template gates a secondary form on presence alone, add
the value comparison. The `ja-JP.json:354` copy is what the catalog says, not what the
guide prose says.

**Disposition.** The template guard is **reconciled in code and closed** for cv-2. The
alias rule **rides with task-154** in anchor-2, where `DRepIdDisplay` gains its
dual-form mode. The ja-JP copy correction is **record-only** for cv-2 — no cv-2 row
asserts ja-JP dialog copy — and is a **read-before-trusting** note for task-146. The
missing-message noise is **record-only until task-146**.

**Owner.** task-175 (recorded); task-146 (the ja-JP claim, and the two catalog keys);
task-154 in anchor-2 (the alias rule for `DRepIdDisplay`); the Planner at slice close
(basis re-measure, and `:3854` if the guide is ever reconciled).

---

## F-25 — task-144's AC-2 discharge pointer lands on a step the guide itself declares unexecutable, so the *observed* remount is owed past task-145 to manual verification; and the wrapper ships with no consumer and no reachable test, leaving cv-2's storybook arm entirely on static proof. Sweep basis unchanged at 309/321

**The chain, read end to end.** The guide splits task-144's AC-2 at
`cv-2-implementation-guide.md:4353-4362`: the structural half is carried by the
file's shape plus `tsc`, and the observed half — type a DRep id, switch the knob,
the field is blank again — is handed on at `:4361`, "record it **OWED** in the
task's `statusReason` and discharge it with task-145's visual pass." That pass is
task-145's Step 8 (`:4685`), and Step 8 closes at `:4739-4741` with "If no browser
is available in this environment, record the console-error and ja-JP overflow pass
as **OWED** … never assert it green"; task-145's own AC-4 (`:4760-4764`) is written
in advance as "**not satisfiable in this container**: there is no browser". The
discharge target is therefore itself owed, and **no cv-2 row observes the remount.**

**Why it is lost if it is not written down.** task-145 will file an OWED of its
own, but against a *different* criterion — console errors and ja-JP overflow, not
the remount. A reader who follows task-144's row to "discharged by task-145", finds
task-145 `complete`, and stops there ticks AC-2 with nothing having been observed.
That is the failure shape F-15 exists to prevent, one row over.

**What cv-2 can prove about the remount, traced rather than asserted.** `react` and
`react-dom` are `16.14.0` (`package.json:259`, `:264`), and the reconciler unwraps a
top-level fragment only when its key is null:
`node_modules/react-dom/cjs/react-dom.development.js:14281` carries
`newChild.key === null` inside the `isUnkeyedTopLevelFragment` conjunction, and
`:14283-14284` replaces the child with its own children in that case alone. A
*keyed* fragment survives as a single element, so a changed key deletes and remounts
the subtree — the mechanism at `GovernanceWrapper.tsx:21` is real, not an article of
faith. The corollary is the refactor hazard: drop the fragment or hoist the key onto
the wrapper's own element and the remount degrades to an in-place update with no
test to catch it. What it clears is the lazy `useState` initializer at
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:163`
— the guide's `:115` is pre-slice numbering, task-139 having inserted the
`drepIndex` prop (`:56`) and its `EMPTY_DREP_INDEX` default (`:153`) above it, which
is the drift the guide's own preamble at `:4227-4231` tells the reader to expect.

**Nothing consumes it and nothing can test it.** `grep -rn GovernanceWrapper` over
`.ts` / `.tsx` outside `node_modules` returns exactly one hit — its own declaration
at `GovernanceWrapper.tsx:19` — and `jest.config.js:129` roots the suite at
`<rootDir>/tests` and `<rootDir>/source`, so a spec placed under `storybook/` would
never execute (F-15's coverage constraint, recorded there as binding on this row).
The slice sweep is unmoved at 18 passed / 1 skipped of 19 suites, 309 passed / 12
skipped of 321, 9 snapshots — identical to F-24's task-175 basis — and it proves
**no regression**, not new behaviour. The first code that exercises the wrapper is
task-145's rewiring of `storybook/stories/voting/Governance.stories.tsx`.

**Resolution.** task-144's AC-2 observed half is recorded OWED in its tracker row
and **does not close at task-145**. It escalates, alongside task-145's AC-4, to the
first environment with a browser — manual release verification at slice close. The
two are distinct observations against distinct criteria, and a slice-close report
that collapses them into a single browser pass repeats the omission.

**Disposition.** Structural half — **closed at task-144**. Observed half —
**escalated past task-145**, owned by no cv-2 row, to manual release verification.
The reconciler mechanism and the no-consumer / no-reachable-test constraint are
**record-only**, binding on task-145 as the first consumer.

**Owner.** task-144 (recorded); task-145 (must not absorb AC-2 into its own AC-4
OWED); the Planner at slice close (carry both observations forward).

---

## F-26 — F-25's discharge target has now landed and discharged nothing: cv-2's storybook arm is complete in code with **zero** observed evidence. And the real reason per-render fixtures are safe is structural, not the effect-deps argument the reviews ran — with no lint rule anywhere protecting it. Sweep basis unchanged at 309/321

**The chain closes empty.** F-25 (`:1598`) escalated task-144's AC-2 observed
half — type a DRep id, switch the knob, the field is blank again — past task-145,
on the grounds that task-145's own visual pass is declared unexecutable. task-145
has now landed `complete`, and it executed no browser step: Step 8's manual
Storybook block (`cv-2-implementation-guide.md:4718-4741`) needs `yarn storybook`
and there is no browser in this container, which its AC-4 (`:4760-4764`) states in
advance. **task-145 was the last cv-2 row that could have observed anything**, so
the whole storybook arm — task-143's fixtures, task-144's wrapper, task-145's
wiring — ships on `tsc`, `eslint` and `grep` alone. Enumerated, so the slice-close
report cannot lose them: the five knob labels and the `Not delegated (warning)`
default on `Connected flow` / `Voting power delegation` / `Voting power delegation
- prefilled from directory`; the per-value badge and caption rendering; the remount
proof; the *absence* of a knob on the two dialog stories; the English → Japanese
re-check; console errors and layout overflow in either locale.

**Why per-render fixtures are actually safe — the structural argument.** Both
task-144's and task-145's reviews cleared the "wallets are minted fresh every
render" change by enumerating `useEffect` dependency arrays. That is the weaker
half. The load-bearing fact is that **`VotingPowerDelegation` never stores a wallet
object at all**: its state holds `selectedWalletId: string | null`
(`source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx:102`
in `initialState`, `:172` when seeded from `initialFormState`) and the object is
re-derived on every render at `:180-181`,
`const selectedWallet = wallets.find((w) => w.id === state.selectedWalletId) ?? null;`.
Id equality is the only equality in play, so a fresh `makeGovernanceWallets(option)`
array cannot strand a stale reference no matter how often it is rebuilt. The
dependency arrays are the corroborating half: the lazy `useState` initializer at
`:163` reads `wallets` once at mount; the re-seed effect keys on primitives
(`:209`, `[currentVoteKind, currentVoteDRepId]`); and the two sites that do touch
the object touch it in the effect **body** only, never in the deps — `:282` is
`[initiateTransaction, intl, state]` with `wallet: selectedWallet` at `:265`, and
`VotingPowerDelegationConfirmationDialog.tsx:115` is
`[intl, onSubmit, redirectToWallet, state]` with `selectedWallet.id` at `:105`.

**Nothing mechanical protects any of it.** `eslint-plugin-react-hooks` 4.4.0 is
**installed** (`package.json:139`) but is **not configured**: `.eslintrc:96` lists
`["@typescript-eslint", "import", "promise", "react", "jest"]` and `grep -rn
"react-hooks" .eslintrc*` returns nothing, so neither `rules-of-hooks` nor
`exhaustive-deps` ever runs. Two consequences. Adding `selectedWallet` to the
`:282` or `:115` dependency arrays — precisely the edit `exhaustive-deps` would
demand if it were switched on, and the kind of edit an IDE quick-fix offers
unprompted — re-arms per-render identity as an effect trigger, and no gate in this
repo would report it. And `useCurrentVoteKnob`
(`storybook/stories/governance/_utils/fixtures.ts:29-31`) is a plain
`return select('Current vote (mock)', currentVoteOptions, 'noDelegation');`, not a
React hook, despite the name; task-145 calls it from two non-component arrow bodies
and from a `withState` callback and lint is silent either way. The `use` prefix
here is the storybook-knobs idiom, not a claim about React, and no tool will tell a
future reader which it is.

**Basis unchanged.** The slice sweep is 18 passed / 1 skipped of 19 suites, 309
passed / 12 skipped of 321, 9 snapshots — identical to F-24 and F-25. No spec
ships and none could: `jest.config.js:129` roots the suite at `<rootDir>/tests` and
`<rootDir>/source`, so a spec under `storybook/` never executes (F-15).
`prettier --check` on `storybook/stories/voting/Governance.stories.tsx` stays exit
1 with exactly the two hunks F-10 records — the `initializeTxErrorOptions` reflow
and the `STAKE_POOLS_LIST` double assertion, both reproducible against the HEAD
blob — and `--write` was not run on it.

**Resolution.** cv-2's storybook arm is code-complete and observation-free. The
structural safety of per-render fixtures rests on `VotingPowerDelegation` keying
its state on an id string, not on any dependency array, and that invariant has no
lint guard: turning `eslint-plugin-react-hooks` on, or hand-adding a
`selectedWallet` dependency, are the two edits that would break it silently.

**Disposition.** Observation gap — **escalated to manual release verification at
slice close**, now carrying three items (task-145 AC-4, task-144 AC-2's observed
half, and the fixture/wrapper surface having never rendered). The id-keyed state
invariant and the unconfigured `react-hooks` plugin are **record-only**, binding on
any future row that edits `VotingPowerDelegation`'s effects or the storybook
fixtures.

**Owner.** task-145 (recorded); the Planner at slice close (carry all three
observations, not one collapsed browser pass); any later row touching
`VotingPowerDelegation.tsx:180-181`, `:282` or
`VotingPowerDelegationConfirmationDialog.tsx:115`.

---

## F-27 — the i18n preliminary-copy guard now has one factually inverted comment and one assertion that passes on an empty set, both approved as minors with no fix pass behind them; and the key the comment misdescribes is the one key in the namespace that **no** assertion guards. Sweep basis unchanged at 309/321

**Two review minors ship unabsorbed.** task-146's code review closed `approved`
after one round with zero blockers and zero majors, but lens 3 filed two **minor**
findings that survived adjudication, and the rubric that lets minors pass also meant
no round 2 ran. The closing pass was a recording pass that writes no code, so both
are still live in the working tree at the moment task-146 was marked `complete`.
They are recorded here rather than in the tracker alone because task-147 edits the
neighbouring test surface and slice close owns the release-end marker sweep.

**CR146-1 — the comment inverts what the catalogs say.**
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts:14-15` reads "Only these two
confirmation-dialog keys are preliminary; the rest of that namespace predates the
feature and is legitimately unmarked." Enumerated over the post-task catalogs, the
`voting.governance.confirmationDialog.` namespace holds **10** keys of which
**three** carry a leading `!!!`: `drepId`, `drepIdCip105`, `signedPayload`. The
comment is a correct reading of the guide's **pre-slice** measurement
(`cv-2-implementation-guide.md:4822-4824`, "8 keys … only `:948` is marked — the
other seven legitimately predate the feature") restated as a post-slice claim, and
the restatement is false: `drepId` is preliminary too, at `en-US.json:948` and
`ja-JP.json:948`, both `"!!!DRep ID"`. The guide's own Step 5 snippet
(`:4959-4964`) ships the constant with **no comment at all**.

**Why that particular key is the expensive one to misdescribe.** `drepId` is the
only marked key in the file's blast radius that **nothing asserts**. The committed
case (`:25-34`) flags en-marked/ja-unmarked pairs only, so it stays silent if the
marker is stripped from *both* locales at once; the namespace case (`:45-52`) is
scoped to `voting.governance.currentVote.` by a deliberate judgment call
(`cv-2-implementation-guide.md:4872-4875`); and `drepId` is deliberately absent from
`PRELIMINARY_CONFIRMATION_KEYS` (`:16-19`). So the comment is the only artefact in
the repo pointing a maintainer at the locked invariant
(`cv-2-implementation-guide.md:4860-4862`, "`…confirmationDialog.drepId` keeps its
existing value"), and it points away from it. Secondarily, "predates the feature"
narrates change history, which the comment conventions exclude.

**CR146-2 — the namespace assertion is vacuously green on an empty match.**
`:45-52` filters `Object.keys(en)` by `CURRENT_VOTE_NAMESPACE` and asserts the
survivors are `[]`. Re-run first-hand with the prefix constant lower-cased to
`voting.governance.currentvote.`, the filter matches **0** keys, `unmarked`
evaluates to `[]`, and the case passes while protecting nothing. The real prefix
matches **17** keys today. A non-empty length guard is the fix; because the guide's
Step 5 snippet is verbatim and carries no such guard, whoever adds it should record
it as deliberate hardening so a later reader does not score it as drift.

**What is genuinely closed.** Neither minor has a runtime, catalog or gate effect,
and the catalog work itself is clean: 17 `currentVote` and 10 `confirmationDialog`
keys per locale, 1618 keys per locale with zero one-sided, all 14 new values
byte-identical to the guide's quoted blocks (`:4905-4912`, `:4923-4930`), ICU
argument names and branch keys identical across locales, `drepId` untouched (proved
structurally by an insert-only `--numstat`), both catalogs still mode `100755`, and
`yarn i18n:manage` idempotent on a second run. `tsc --noEmit` exit 0; `yarn lint`
exit 0; the two guide-named Jest patterns green with **zero** snapshots written;
slice sweep 18 passed + 1 skipped of 19 suites and 309 passed + 12 skipped of 321
tests with 9 snapshots — the F-24 / F-25 / F-26 basis, unmoved.

**The escalation stack gains a fourth item.** AC-3's second half — the ja-JP length
and layout overflow review — needs a running Storybook with the global Japanese
toggle, which this container cannot provide. The guide books it in advance
(`:4881-4883`, `:5041-5043`), so it is an environment limit rather than a discovered
gap, but it now rides to slice close alongside F-25's and F-26's three storybook
observations. It is a **fourth distinct** observation against a distinct criterion:
a slice-close report that collapses the ja-JP overflow pass into the storybook
browser pass repeats exactly the omission F-25 exists to prevent. Note also that
every cv-2 catalog value still carries `!!!`, so no copy in this slice is final —
clearing the markers is a separate, user-owned release-end review.

**Disposition.** Two open minors — **carried to the next row that opens this file,
or to slice close, whichever comes first**; both are one-line edits and the spec is
prettier-clean today, so hand-edit it and never run `--write` on it. AC-3's second
half — **escalated to manual release verification at slice close**. The `drepId`
coverage hole is **record-only** and binding on any future row that widens this
guard: adding `drepId` to `PRELIMINARY_CONFIRMATION_KEYS` would close it, and the
guide's scoping judgment call does not forbid that, it only forbids asserting the
whole `confirmationDialog.` namespace.

**Owner.** task-146 (recorded); task-147 or the Planner at slice close (absorb
CR146-1 and CR146-2); the Planner at slice close (carry the ja-JP overflow pass as
its own line item, and the `!!!` marker sweep as the user's release-end review).

---

## F-28 — cv-2's flow-level sanitization evidence is a tripwire over an **empty** set, and the one leak its own tree can produce is the one shape it cannot see; F-18's realm shim finally lands, seven build-order rows past where F-18 aimed it; and F-27's task-147 absorption pointer resolves to nothing. Sweep basis moves to 323/335

**AC-4's flow half asserts against nothing.** task-147's Step 2 logger case
(`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:852-877`)
spies all four renderer `logger` levels, drives select → edit → submit → dialog,
and runs seven `not.toContain` checks over the collected calls. Measured, not
inferred: `grep -rl "utils/logging"` over `components/voting`,
`components/governance`, `containers/voting` and `containers/governance` returns
exactly two paths, the spec itself and
`source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx:7`. Both of
that module's calls sit inside `handleCopy` (`:52-54`, `:62-65`), reachable only
by a copy-button click this test never performs, and every store in the tree is a
plain `jest.fn` mock, so `VotingStore.ts:354` / `:412` cannot fire either. The
collected value is therefore `[[],[],[],[]]` and all seven checks pass over an
empty string. This is **not** a defect — it is a forward regression net that fails
the day a leak lands, and the guide ships it verbatim (`:5279-5303`) as AC-4's
flow-side evidence (`:5557-5558`). It is recorded because the distinction is
invisible from the tracker: cv-2's *executable* proof that the flow sanitizes is
Step 5's store/analytics case against a real `VotingStore`
(`tests/jest/security/governance-sanitization.spec.ts:466-514`), and the flow case
is a guard, not an observation. A slice-close report that reads the green run as
"the flow was watched and leaked nothing" overstates it.

**And the guard is blind to the only leak its own tree can emit.** Review minor
**CR147-1** ships **open** — the review closed `approved` with zero blockers and no
fix pass ran before the closing pass, which writes no code.
`VotingGovernancePage.spec.tsx:868` builds the haystack with bare
`JSON.stringify`, while the one reachable site, `DRepIdDisplay.tsx:62-65`, logs
`{ error, drepIdLength }`. `Error.message` and `.stack` are **non-enumerable**, so
that call serialises as `{"error":{}}` and a DRep id carried inside an error
message passes all seven checks untouched. The slice already owns the fix and
documents this exact hole one file over: the comment plus `jsonStrWithErrors` at
`tests/jest/security/governance-sanitization.spec.ts:62-69`, which every sibling
logger case uses (`:373`, `:410`, `:511`). The remedy is a one-token substitution
at `:868`; it is a **hardening beyond** the guide's verbatim snippet, so whoever
applies it must say so or a later reader will score it as drift.

**F-18's shim lands at the row F-18's Owner line predicted, not at the row its
disposition did.** F-18 dispositioned the jsdom/Node realm trap **blocking on
task-173** (`:1142-1146`), F-21 measured that row and found it not applicable —
`normalizeDRepIdentity` decodes through the `bech32` package, never through the
SDK's brand-checking encoder — and the shim was never installed. task-147 needs it
for real: its CIP-105 case reaches `Cardano.DRepID.toCip129DRepID` through
`resolveExactDRepMatch`, which without the shim throws inside its own `catch` and
returns `null`, leaving the badge silently `unavailable`. The three lines now sit
at `VotingGovernancePage.spec.tsx:33-38` and are **byte-identical** to the
established shim at
`source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx:21-26`,
so precedent was copied rather than invented. F-18's *Owner* line — "task-147 (any
end-to-end badge assertion inherits the same constraint)" (`:1148-1150`) — was the
accurate pointer all along; its *Disposition* line was not. The durable rule for
any future reader: **a jsdom spec that asserts on the far side of
`resolveExactDRepMatch` needs this shim; one that only decodes through the
`bech32` package does not.**

**F-27's absorption pointer resolves to nothing.** F-27 named "task-147 or the
Planner at slice close (absorb CR146-1 and CR146-2)" (`:1815-1817`). task-147's
change set is three spec files and does **not** include
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts`, so neither minor was absorbed.
Both escalate intact to the Planner at slice close, joined now by CR147-1. Three
review minors are open across cv-2 with no fix pass behind any of them; the pattern
is structural, not accidental — the rubric that lets a minor pass also means no
round 2 runs, and every closing pass in this slice is a recording pass that writes
no code.

**Basis.** The slice sweep `--testPathPattern="(governance|voting)"` moves to **18
passed / 1 skipped of 19 suites, 323 passed / 12 skipped of 335 tests, 9
snapshots**, from the **309 / 321** basis F-24 through F-27 share. The `+14`
reconciles exactly against the per-file HEAD counts — `VotingGovernancePage` 18 →
27, `isSameVoteTarget` 9 → 12, `governance-sanitization` 24 → 26 — so no
pre-existing test was deleted, renamed away or disabled, which is the real hazard
on an append-only edit and is ruled out arithmetically rather than by eye. Skips
stay 12 (all `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`, self-skipping
because `cardano-cli` is off PATH), snapshots stay 9 with none written,
`tsc --noEmit` exits 0 and `yarn lint` exits 0 at 5596 warnings with a task delta of
zero (`tests` is eslint-ignored at `.eslintignore:8`; the one lintable changed file
reports the same 9 warnings at HEAD and in the tree).
`prettier --check` on `tests/jest/security/governance-sanitization.spec.ts` is exit
1, **wholly pre-existing** — the same single hunk, the doubled `MatomoTracker`
cast, at `HEAD:484` and worktree `:546`, the line moved only by the 62 inserted
above it. That is a fourth file outside the three-file list the slice brief carries,
exactly as F-10 predicts (238 dirty files; the short list is a sample), so it
confirms F-10 rather than extending it.

**Disposition.** CR147-1 — **open**, one-token fix, owner is whoever next opens
`VotingGovernancePage.spec.tsx`. The empty-tripwire characterisation and the shim
rule are **record-only**, binding on any future row that adds a jsdom governance
spec or reads cv-2's sanitization evidence at slice close. F-18's disposition line
is **superseded** by this entry; its Owner line stands.

**Owner.** task-147 (recorded); the Planner at slice close (carry CR146-1, CR146-2
and CR147-1 as three open minors, and do not read AC-4's green flow case as an
observed no-leak); any later row adding a jsdom spec that asserts past
`resolveExactDRepMatch`.

---

## F-29 — cv-2's lint number moves for the first time, because this is the first row whose spec is **not** eslint-ignored, and reading that `+3` as drift would be wrong; the guide's own cast snippet is prettier-unusable and would have turned a green file red; four task-148 guide anchors have drifted and two more were *reported* drifted that are not. Sweep basis moves to 325/337

**The `tests` is eslint-ignored shortcut expires at this row.** F-26 and F-28
both recorded a *zero* lint delta by the same argument — `tests` is ignored
(`.eslintignore:8`), so a spec-only row cannot move the repo count. That argument
is file-location-dependent, not row-type-dependent, and task-148's only changed
file is `source/renderer/app/stores/VotingStore.spec.ts`, **under `source/`**, so
it is linted like any production module. Measured, not inferred:
`eslint --format compact` reports **11** problems (0 errors) in the tree against
**8** for the `HEAD` blob piped through `--stdin-filename`, and `yarn lint` moves
**5596 → 5599**, exit 0. All three new warnings are
`@typescript-eslint/no-explicit-any` — `:247` (the `ApiError({ code: 'same_vote' }
as any)` fixture, unavoidable because `ErrorType.code`
(`source/renderer/app/domains/ApiError.ts:57`) is typed as the closed
`KnownErrorType` union at `:8-50`, which does not contain `same_vote` at all),
`:333` (`React.ComponentType<any>`) and `:345` (the wallet
fixture) — and the file already carried eight of the same pattern. The durable
rule: **a cv-2-style row moves the repo warning count iff its changed spec lives
under `source/` or `storybook/`; `tests/jest/**` never does.** A slice-close
reader comparing 5591 → 5596 → 5599 across the wave must attribute the last `+3`
here and the earlier `+5` to the task-144 / task-145 *component* commits, not read
either as unexplained drift.

**The guide's cast snippet cannot be typed as written.** `cv-2-implementation-guide.md:5753`
ships `const DelegationForm = VotingPowerDelegation as unknown as React.ComponentType<any>;`
as one line. prettier 2.1.2 rewrites a double assertion into the parenthesized,
wrapped form, so copying the guide verbatim would have made a **currently-clean**
file fail `--check` — and per F-10 the slice's standing rule is that a red file
must never be `--write`-repaired, which would have left the row with a
self-inflicted violation and no sanctioned fix. What shipped
(`VotingStore.spec.ts:332-334`) is the hand-written
`const DelegationForm = (VotingPowerDelegation as unknown) as React.ComponentType<\n  any\n>;`,
`--check` exit 0. This is the **second** measured instance of the same 2.1.2
behaviour in cv-2 — F-28's pre-existing red at
`tests/jest/security/governance-sanitization.spec.ts:546` is the identical doubled
`MatomoTracker` cast — so it is a repo property, not a one-off: **every
`X as unknown as Y` written by hand in this repo must be parenthesized and wrapped,
and no guide snippet containing one can be trusted verbatim.**

**Guide anchor drift, with two false positives corrected.** Genuinely drifted in
the task-148 section: `VotingPowerDelegation.tsx:89 → :95` (the intl map entry),
`:84-92 → :90-98` (the map declaration), `:304-308 → :401-405` (the render site),
and `en-US.json` / `ja-JP.json` `:973 → :980`. **Not** drifted, contrary to two
review lenses and the verification gate, which each reported them as such:
`parseApiCode` is at `VotingStore.ts:74` and the `initializeVPDelegationTx` catch
is at `:347`, exactly as the guide states (`:5576`, `:5577`). One further inline
citation is loose rather than drifted: the resolved judgment call at `:5648-5651`
cites `ApiError.ts:8-59` for the `KnownErrorType` union, which actually spans
`:8-50`. Every quoted code
and copy string still matches byte for byte, including the long English same-vote
sentence the render test asserts verbatim, so **no** behavioural adaptation was
required and the drift is documentation debt only. Recorded because a corrective
edit to that guide section must not "fix" the two anchors that were already right.

**One structural note, record-only.** A *store* spec now renders a *renderer*
component and asserts on its DOM and its en-US copy. That coupling is
guide-fixed (`:5570` names this file and `:5653-5656` forbids renaming it to
`.tsx`, because `tsconfig.json` has no `include` so specs are typechecked and
TypeScript rejects JSX in a `.ts` file), both dropdowns are mocked to render
nothing, and the `React.createElement`-inside-a-`.ts`-spec pattern is committed
precedent at `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.spec.ts:129`, `:136`.
It was raised and dropped in review. It is noted only so a future reader who finds
a renderer-copy assertion in a store spec knows it was a deliberate,
constraint-forced choice rather than a stray.

**Basis.** The slice sweep `--testPathPattern="(governance|voting)"` moves to **18
passed / 1 skipped of 19 suites, 325 passed / 12 skipped of 337 tests, 9
snapshots**, from F-28's **323 / 335**. The `+2` is exactly this row's two cases
(`grep -cE "^\s+(it|test)\(|it\.each"` on the spec: 6 at `HEAD`, 8 in the tree),
and the single deleted line in a 118/1 diff is the widened import
(`import VotingStore, { FundPhase } from './VotingStore';`), so nothing was
deleted, renamed away or disabled. Skips stay 12 (all
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts`, `cardano-cli` off PATH),
snapshots stay 9 with none written, `tsc --noEmit` exits 0, and `prettier --check`
is green on the changed file and on
`source/renderer/app/containers/voting/VotingGovernancePage.tsx`, with the three
F-10 files re-checking red and unmodified — net new violations zero.

**The escalation chain terminates here.** task-148 is the **last** row in cv-2's
canonical build order, its change set is one file, and it therefore absorbed none
of CR146-1, CR146-2 or CR147-1 — it touches neither
`tests/jest/i18n/preliminaryCopyMarkers.spec.ts` nor
`VotingGovernancePage.spec.tsx`. F-27 and F-28 each pointed their minors at "the
next row or the Planner"; there is no next row. All three are now unambiguously
the Planner's at slice close. Its review was cv-2's **only** unanimously clean
round — three lenses, empty lists, zero survivors at any severity, not one line of
the deliverable changed — which makes the pattern F-28 named (a minor passes, so
no round 2 runs, so no fix pass ever happens) structural rather than incidental.

**Disposition.** The lint-attribution rule and the parenthesized-cast rule are
**binding** on any future row that adds a spec under `source/` or hand-writes a
double assertion. The anchor list is **documentation debt** against
`cv-2-implementation-guide.md`'s task-148 section, to be applied only as corrected
above. The store-spec/renderer-component coupling is **record-only**. CR146-1,
CR146-2 and CR147-1 remain **open** with no downstream absorber.

**Owner.** task-148 (recorded); the Planner at slice close (carry the three open
minors, re-base the lint figure to 5599, and do not read cv-2's clean review
rounds as evidence that its minors were resolved); whoever next edits the guide's
task-148 section (apply the four real anchor corrections, leave `:74` and `:347`
alone).

---

## F-30 — the release-blocking hardware byte-equality assertion is already green in two pre-cv-2 suites cv-2 never opened, which is what makes task-175's non-executing proxy acceptable; and anchor-2's task-154 AC-3 is satisfied as worded before anchor-2 starts

`designs/shared-design-tokens.md:139` — "A hardware-wallet Jest test must assert
that the identifier surfaced by the device prompt is byte-equal to
`vote.chosenOption` … This is a release-blocking assertion", over the identity
equality rule at `:137`. task-175's own spec does not execute it:
`VotingPowerDelegationConfirmationDialog.spec.tsx:331-351` derives the credential
inline with `Cardano.DRepID.toCredential(Cardano.DRepID(cip129))` and compares it
to the rendered signed payload; neither hardware mapper runs.

**The assertion nevertheless exists in the repo, in files no cv-2 row touched.**
`source/renderer/app/utils/shelleyLedger.spec.ts:32`, `:41`, `:50`, `:59` and
`source/renderer/app/utils/shelleyTrezor.spec.ts:35`, `:48`, `:59`, `:70` each map
a `cast_vote` certificate through the real `toLedgerCertificate` /
`toTrezorCertificate` and assert the device field (`keyHashHex` / `scriptHashHex`,
`keyHash` / `scriptHash`) equals
`Cardano.DRepID.toCredential(Cardano.DRepID(vote)).hash` across four vectors —
CIP-129 key, CIP-129 script, CIP-105 key, CIP-105 script.
`git log --oneline -1 --` over both paths returns `a463c31d0`
("docs(gov): close out slice-3"), i.e. before cv-2's first commit, so cv-2 neither
wrote nor weakened them.

**Two consequences, one backward and one forward.** Backward: task-175 AC-3's
shortfall is narrower than its wording suggests — what is missing is an *execution
inside the confirmation-dialog spec*, not repo coverage of the design's release
blocker. A reader who takes the AC-3 exception row to mean "the hardware
byte-equality assertion is unproved" will be wrong. Forward: anchor-2's
**task-154 AC-3** — "Hardware-wallet test asserts on-device DRep ID equals
`vote.chosenOption`" (tracker, `pending`) — is already satisfied verbatim by those
eight cases. If anchor-2 means something stronger, which its own AC-2 implies
(byte-equality preserved *across* the delegate → verified-name transition), the
criterion has to say so; otherwise the row closes on a test that predates it and
the transition ships unpinned.

**Resolution.** Record-only. cv-2 adds no hardware coverage and needs none; no
file is edited by this finding.

**Disposition.** Backward half — **closed**, recorded in the PRD's Definition of
Done exception table as the mitigation on task-175 AC-3. Forward half —
**carried to the anchor-2 planning pass**.

**Owner.** The anchor-2 planning pass (re-word task-154 AC-3, or accept it as
pre-satisfied and say so); any later row that edits the two mappers (the
four-vector byte-equality cases are the release blocker and must survive).

---

## F-31 — the S-9 sanitization gate has two anchors, not one, so re-proving it from the floor suite alone returns a false green; and that suite's own docblock now contradicts an assertion in its own body

**Anchor 1** is the inherited floor suite
`tests/jest/security/governance-sanitization.spec.ts` (26 cases), which proves
key-name redaction through `filterLogData` and the api call boundaries. It knows
nothing about the `DRepIdentity` object task-173 and task-175 created. **Anchor 2**
is `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx:852-877`,
the only cv-2 assertion that follows a vote target across the rendered flow. A
reader who re-proves S-9 by running `--testPathPattern=governance-sanitization`
alone therefore gets a green that never touched half of what cv-2 made live: the
two anchors must be run and cited **together**, and any future statement of the
sanitization floor that names only the security suite is incomplete.

**And anchor 2 is F-28's tripwire over an empty set**, which is what makes the
pairing a caution rather than a reassurance: the rendered tree's only `logger`
importer is `components/governance/_shared/DRepIdDisplay.tsx`, whose calls sit
behind a copy click the test never performs, and every store is a `jest.fn` mock —
so the seven `not.toContain` checks at `:869-875` pass over zero calls. CR147-1
compounds it: `:868` serialises with a bare `JSON.stringify` while the sibling
suite ships `jsonStrWithErrors` (`governance-sanitization.spec.ts:62-69`, used at
`:511`) for exactly the non-enumerable `Error.message` / `.stack` case. The honest
statement of cv-2's sanitization evidence is: **key-level redaction proved by
execution; domain-object containment proved structurally only.**

**The docblock contradiction, unrecorded until now.** The floor suite's own header
at `:4-5` claims no "DRep ID, abstain/no_confidence literal, or CIP-129/CIP-105
bech32 string reaches any logger call **or analytics payload**", while `:500-504`
in the same file *requires*
`sendEvent(EventCategories.VOTING, 'Casted governance vote', 'abstain')` and `:505`
pins the argument count at three. The operative rule is F-14's, and it is
narrower than the docblock: the vote **kind** is a sanctioned analytics dimension,
the vote **target** never is, and no sentinel may enter a **logger** payload —
which is what `:511-515` actually asserts, through `jsonStrWithErrors`. The
docblock predates cv-2 and was never renarrowed.

**F-9's fallback is carried verbatim, and must not be improvised down.**
`filterLogData` matches key names **exactly**
(`source/common/utils/logging.ts:59`, `sensitiveData.includes(key)`) and the list
at `:24-49` guards none of `drepIdentity`, `currentVote`, `votingTarget`,
`chosenOption`, `raw`, `cip105`, `credentialHex`. cv-2 is safe only because it
adds no sink at all. anchor-1 and anchor-2 add anchor fetch and name-render paths,
and that is where it stops being theoretical; the specified fallback is F-9's —
extend `sensitiveData` with all seven names **and** add domain-shaped cases to the
floor suite — not a two-key patch.

**Resolution.** No code change at slice close: the closeout is docs-only, and
both the docblock and CR147-1 are edits to source files.

**Disposition.** The two-anchor wording is **binding** on the PRD's Final Outcome
and on any future re-proof of S-9. The docblock contradiction is **record-only
documentation debt**. CR147-1 stays **open** (one-token swap). F-9's fallback
stays **deferred** to the first anchor row that adds a logging or fetch sink.

**Owner.** Whoever next edits
`tests/jest/security/governance-sanitization.spec.ts` (renarrow the docblock in
that pass, not as a standalone closeout edit); whoever next opens
`VotingGovernancePage.spec.tsx` (CR147-1); the anchor-1 / anchor-2 rows that add a
sink (F-9's fallback).

---

## Slice close — dispositions at `fb025e44e` (2026-07-29)

Everything below was re-opened and re-measured in this worktree at slice head
`fb025e44e`, working tree clean. The shipped-commit table, the per-task status
rationale and the gate figures live in `cv-2-PRD.md`'s Final Outcome and in the
tracker's `statusReason` fields and are **not** repeated here. This section carries
only what a future reader needs from *this* file: which findings survive the
slice, who owns each one, and the environment the slice was built in.

### Closed in-slice

**F-1, F-3, F-5, F-7, F-11, F-13, F-14, F-16, F-17, F-18, F-19, F-20, F-21, F-22,
F-23, F-24** close with nothing carried. Three closures are new information:

- **F-1** now has the executable pin its Owner line assigned to task-147. The
  store-backed chain is asserted end to end at
  `VotingGovernancePage.spec.tsx:775` ("shows the current delegation and disables
  submit while the form matches it") and `:795` ("resolves the directory entry for
  a CIP-105 delegation through its CIP-129 form"), over a real
  `drepIndex: new Map([[VALID_DREP_ID, drepEntry]])` at `:182`, through
  `resolveExactDRepMatch` at `VotingPowerDelegation.tsx:215-217` into
  `deriveCurrentVoteBadgeState` (`CurrentVoteSummary.tsx:26`, called at `:74`).
  These two cases are the **only** executable pin on the store-backed lookup
  anywhere in the slice.
- **F-21**'s bounded interim gap — a legacy 28-byte `drep1…` id rendering the
  generic *Delegate to DRep* label instead of its own string — is **closed** by
  task-175 (`b699d176c`): `VotingPowerDelegationConfirmationDialog.tsx:119` keys
  `isSentinelVote` on the vote kind rather than on decode success and `:157`/`:167`
  render `{drepIdentity?.raw ?? chosenOption}` verbatim, pinned by
  `VotingPowerDelegationConfirmationDialog.spec.tsx:353` ("renders only the
  verbatim primary line when the decoder rejects the id").
- **F-11 and every sweep-basis entry (F-16 … F-24, F-28, F-29)** are superseded by
  the single unfiltered measurement in the PRD's Final Outcome. Per-row sweep
  arithmetic in those findings is historical from this point and must not be
  quoted as a current figure.

### Open at close

- **F-2 — the `givenName` orphan. Open, unmitigated, highest residual risk.**
  Re-verified at `fb025e44e`: `AppDRepDirectoryEntry`
  (`GovernanceStore.ts:20-31`) and `DRepDirectoryEntry`
  (`governance.types.ts:51-62`) still carry exactly `drepId`, `votingPower`,
  `status`, `drepActivity`, `anchor` — **no name field** — and neither tracker edit
  F-2 asks for has been made. task-151 (anchor-1, `pending`) AC-1 still scopes
  verified `givenName` to "the DRep detail view" and its AC-3 still exposes only
  "verified metadata-completeness state … for the slice-5 cohort rule"; task-154
  AC-1 and task-157 AC-2 (anchor-2, both `pending`) still scope it to the
  delegation confirmation. So **no** anchor row owns a `CurrentVoteSummary`
  verified-name render, **no** anchor row owns the unverified→verified Storybook
  story, and **no** anchor row adds the field either would read. Two actions, in
  order: anchor-1 planning extends task-151 to add a name field to both interfaces
  from the verified CIP-119 payload; anchor-2 planning extends task-154 or opens a
  new row for the render and the story. Without the first, the second has no data
  source. **Owner:** the anchor-1 and anchor-2 planning passes.
- **F-15 — task-143 AC-4, both halves. Open.** Provenance half (named CIP-119
  provenance for the `drepUnverified` vector) unscheduled, carried to the anchor-2
  planning input; hash half deferred to anchor-1 (task-149, task-150). The seam is
  unchanged at close — both `makeDRepIndex` entries still ship `anchor: null`
  (`storybook/stories/governance/_utils/fixtures.ts:154`, `:164`) — and the
  verification vector anchor-1 needs is already committed at
  `drep-state-preprod-epoch295-sample.json:2852-2855`. **Owner:** the anchor
  planning passes; task-149 / task-150 for the hash.
- **F-8 — three unregistered governance stories. Open, unchanged by design
  (D-12).** `storybook/stories/index.ts:17-18` still imports only
  `DRepDirectory.stories` and `CurrentVoteSummary.stories`, while
  `DRepDetail.stories.tsx`, `DRepDirectoryBanner.stories.tsx` and
  `DRepCategoryBadge.stories.tsx` are imported by no file in the repo.
  **Owner:** nobody in cv-2; the next slice that edits `index.ts`.
- **F-9 — the unguarded renderer-domain key surface. Open as a forward risk**;
  fully dispositioned in F-31 above. **Owner:** the first anchor row that adds a
  logging or fetch sink.
- **F-6 — task-142 AC-3's anchor. Half-open, as documentation only.** The tracker
  criterion still reads "HW status section (lines ~L118-L127) is untouched", an
  anchor that matches `VotingPowerDelegationConfirmationDialog.tsx` at no commit in
  this repo's history, and the semantic re-anchor lives only in the row's
  `statusReason`. That `statusReason` anchor has itself drifted since: it cites the
  `selectedWallet.isHardwareWallet ?` branch at `:179-185`, correct at task-142's
  own commit `218f853f7` (`:179` there), but task-175 inserted the identity block
  above it and at `fb025e44e` the branch is `:216-222`. Deliberately not amended at
  close — rewriting an acceptance criterion inside a docs-only closeout is out of
  scope. **Owner:** whoever next edits that row.
- **F-25 and F-26 — the storybook arm, three distinct owed observations.** All to
  manual release verification, and F-25's caution stands: they must **not** be
  collapsed into one browser pass. (i) task-145 AC-4 — five knob values × two
  locales, console errors plus layout overflow; (ii) task-144 AC-2's *observed*
  remount — type an id, switch the knob, field blank; (iii) the fixtures and
  wrapper surface has never rendered at all. **Owner:** the user's release
  verification.
- **F-27 — two i18n minors, still open with no absorber.** CR146-1: the comment at
  `tests/jest/i18n/preliminaryCopyMarkers.spec.ts:14-15` ("Only these two
  confirmation-dialog keys are preliminary") is false —
  `voting.governance.confirmationDialog.drepId` parses to `!!!DRep ID` in **both**
  catalogs, so three of that namespace's keys are marked, and `drepId` is the one
  marked key no assertion guards. CR146-2: the current-vote assertion filters by
  `CURRENT_VOTE_NAMESPACE` and asserts the filtered list is empty, so it passes
  vacuously if that prefix constant is ever mistyped. Both are one-line hand edits;
  the file is prettier-green (measured at close, exit 0), so it must be hand-edited
  and never `--write`-repaired. The ja-JP length/layout review rides with the
  browser-owed set above. **Owner:** whoever next opens that spec.
- **F-28 — CR147-1 and the empty-set tripwire. Open**; dispositioned in F-31.
- **F-29 — binding rules plus documentation debt.** The lint-attribution rule (a
  row moves the repo warning count iff its changed spec lives under `source/` or
  `storybook/`) and the parenthesized-double-assertion rule stay binding on future
  rows. The four real task-148 guide anchor corrections stay unapplied, and
  `VotingStore.ts:74` / `:347` must not be "corrected" — they are right. **Owner:**
  whoever next edits the guide's task-148 section.
- **F-30, F-31** — as dispositioned in their own entries above.
- **Design-doc drift, recorded and deliberately not fixed.**
  `designs/current-vote-display-design.md:97` still *offers* the superseded
  case-insensitive `cip129` comparison ahead of the clause retiring it — that is
  the append-only rule working as intended, not a live drift, but a reader who
  greps for the phrase hits the retired offer first. And the §6.1 note at `:101`
  still says `CurrentVoteSummary` "reads them reactively from the store" through a
  `DRepIndexEntry` type that does not exist in code (F-2), superseded by the prop
  chain cv-2 shipped instead (D-6). **Owner:** whoever next edits that design.

### Environment deviation record

- **`nix` is absent from this devcontainer, so the mandated pre-commit `nix fmt`
  never ran — on none of the 15 commits.** `node_modules/.bin/prettier` on
  explicitly listed paths was the substitute for the whole slice (F-12). A
  `nix fmt` pass **before merge** is an outstanding obligation the user owns; it is
  the one gate cv-2 records as unsatisfied rather than passed.
- **prettier scope.** `--write` was run only on the four files cv-2 created
  (`isSameVoteTarget.ts`, `isSameVoteTarget.spec.ts`,
  `storybook/stories/governance/_utils/fixtures.ts`,
  `_utils/GovernanceWrapper.tsx`). `yarn prettier` was never used — its script
  carries a repo-wide `**/*.*` glob that rewrites ~238 unrelated files (F-10). No
  tool-managed JSON was ever formatted (the tasks tracker, `en-US.json`,
  `ja-JP.json`, `translations/messages.json`).
  `source/renderer/app/containers/voting/Governance.tsx` was excluded from every
  invocation, because prettier 2.1.2 cannot parse its line-4 inline type import and
  exits 2 (F-10).
- **The prettier-red set among cv-2-relevant files is four, not the three usually
  quoted.** Measured at `fb025e44e` (`prettier --check`, exit codes): red —
  `VotingPowerDelegation.tsx`, `VotingPowerDelegationConfirmationDialog.tsx`,
  `storybook/stories/voting/Governance.stories.tsx`, and
  `tests/jest/security/governance-sanitization.spec.ts`; green and left alone —
  `VotingGovernancePage.spec.tsx`, `VotingStore.spec.ts`,
  `CurrentVoteSummary.tsx`, `tests/jest/i18n/preliminaryCopyMarkers.spec.ts`. All
  four reds are pre-existing, none was `--write`-repaired, and net new prettier
  violations for cv-2 are **zero**. This is what F-10 predicts — the commonly
  quoted three-file list is a sample of a 238-file dirty set — so a fourth red file
  is not a new condition.
- **No browser.** Every visual criterion is owed to manual release verification;
  Storybook's visual pass was never a runnable gate here, and the compile-level
  floor (`tsc --noEmit`, `yarn lint`, the dev-server preview bundle) is what cv-2
  could gate. `yarn check:all` and `yarn storybook:build` are red at HEAD for the
  unrelated storybook manager-webpack JSX-loader reason
  (`research/cv-1-findings.md` F-20) and were correctly never treated as cv-2
  gates.
- **`cardano-cli` is off PATH**, so
  `tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` self-skips —
  `const describeWithCli = isCliOnPath ? describe : describe.skip;` at `:28`, with
  the reason stated at `:25-27`. It is the single skipped suite in every cv-2
  measurement, by design; a future reader must not read it as a broken suite.
- **`gh` and push credentials are absent.** Nothing was pushed; the 15 task commits
  and the 3 planning commits are local to `feat/drep-discovery`.

---

## References

- Tasks tracker: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1162-1457` (phase `cv-2`)
- Slice PRD: `task-plans/cv-2-PRD.md` — D-1 … D-15 resolve O-1 … O-8; the
  "Corpus-vs-Repo Corrections cv-2 Inherits" table is the compact index of F-1 … F-10
- Orchestration prompt: `.agent/plans/governance/drep-discovery/prompt.md` (invariants `:93-138`, live-repo-wins rule `:39-41`)
- Working conventions: `.agent/plans/governance/drep-discovery/README.md` (`:14` append-only rule, `:18` preliminary `!!!`, `:67` cohort fixture floor)
- Designs: `designs/shared-design-tokens.md`, `designs/current-vote-display-design.md`, `designs/current-vote-display-ux.md`
- Preceding slice: `research/cv-1-findings.md` (F-9 comparator, F-15 sanitization, F-18 combined card, F-20 storybook build, F-24/F-25 i18n), `task-plans/cv-1-PRD.md`, `task-plans/cv-1-implementation-guide.md`, `task-plans/cv-1-code-review.md`
