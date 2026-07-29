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

## References

- Tasks tracker: `.agent/plans/governance/drep-discovery/governance-drep-discovery-plan-tasks.json:1162-1457` (phase `cv-2`)
- Slice PRD: `task-plans/cv-2-PRD.md` — D-1 … D-15 resolve O-1 … O-8; the
  "Corpus-vs-Repo Corrections cv-2 Inherits" table is the compact index of F-1 … F-10
- Orchestration prompt: `.agent/plans/governance/drep-discovery/prompt.md` (invariants `:93-138`, live-repo-wins rule `:39-41`)
- Working conventions: `.agent/plans/governance/drep-discovery/README.md` (`:14` append-only rule, `:18` preliminary `!!!`, `:67` cohort fixture floor)
- Designs: `designs/shared-design-tokens.md`, `designs/current-vote-display-design.md`, `designs/current-vote-display-ux.md`
- Preceding slice: `research/cv-1-findings.md` (F-9 comparator, F-15 sanitization, F-18 combined card, F-20 storybook build, F-24/F-25 i18n), `task-plans/cv-1-PRD.md`, `task-plans/cv-1-implementation-guide.md`, `task-plans/cv-1-code-review.md`
