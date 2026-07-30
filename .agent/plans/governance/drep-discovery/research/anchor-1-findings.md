# anchor-1 — Research findings

> Durable findings from the anchor-1 slice. Facts only; every `path:line` below was
> opened in the anchor-1 worktree at base `33c02840a` (branch `feat/drep-discovery`)
> and verified there. Each finding carries **Resolution.** (what is true and what the
> slice does about it), **Disposition.** (fixed now / rides with task-NNN / raised to
> the user / record-only) and **Owner.** (who discharges it).

---

## F-1 (task-152) — The scheme audit that justified the HTTPS-only allow-list reaches only source-literal URL producers, so two runtime-sourced classes of URL sit outside it and now fail silently

The guide's Step 1 audit is three greps over `source/`
(`.agent/plans/governance/drep-discovery/task-plans/anchor-1-implementation-guide.md:536-539`),
so its reach is URLs written as literals in the tree — not URLs that arrive as data
at runtime. Within that reach the conclusion holds and was independently re-checked:
the remaining `http://` literals under `source/` are not `openExternal` producers
(local token-metadata servers, the dev-server `loadURL`, a synthetic analytics URL,
story fixture data, and one cosmetic `label` in `About.tsx` whose `onClick` passes
the `https://` form). Two classes are invisible to those greps, and after this
change an `http:` value in either is rejected with no user-visible error:

1. **Stake-pool homepage.** `source/renderer/app/components/staking/widgets/TooltipPool.tsx:512`
   is `onClick={() => onOpenExternalLink(homepage)}`, and `homepage` is
   operator-registered pool metadata with no scheme constraint anywhere on the path.
2. **Newsfeed action URLs.** `source/renderer/app/stores/NewsFeedStore.ts:220-224`
   destructures `newsItem.action` and calls `this.stores.app.openExternalLink(url, e)`
   on the remote feed's `url` verbatim.

How often either carries `http://` in production is **unmeasured** — there is no
network in this environment to sample registered pool metadata or the live feed.

**Resolution.** Widening the allow-list is not the remedy: the guide fixes it at
exactly `https:` (`anchor-1-implementation-guide.md:506`) and locked invariant #3
(`:75-81`) forbids thinning the floor, so the constant stays a single value
(`source/main/ipc/open-external-url.ts:10`). The honest statement of the audit is
"one real non-https producer **among source-literal producers**", and the task-152
`statusReason` is worded that way rather than as an unqualified claim.
**Disposition.** Raised to the user as a product decision — accept the silent
failure, surface an error in the UI, or normalise the value at the producer. Out of
scope for task-152, which is a main-process hardening task.
**Owner.** User / product, before the pool-homepage or newsfeed surfaces are next
touched.

## F-2 (task-152) — `getNetworkExplorerUrl` was the one source-literal non-https producer, and forcing https there is safe as a code property but unproven as a reachable endpoint

At HEAD the helper chose its scheme per network, emitting `http://` for every
network outside `MAINNET | TESTNET | DEVELOPMENT` — which is `STAGING` and any
unrecognised value. It now emits `https://${uri}` unconditionally
(`source/renderer/app/utils/network.ts:36-39`). All four explorer URIs the sibling
`getNetworkExplorerUri` returns are public hosts and none is `localhost`, so no
local-development path is broken by the change. `tests/common/unit/networks.spec.ts`
grew from 4 tests to 12, pinning an https prefix for `MAINNET`, `TESTNET`,
`DEVELOPMENT`, `STAGING`, `'preprod'` and `'selfnode'`, plus the staging host in
full and the `getNetworkExplorerUrlByType` path for a network outside the localised
set. Re-measured here: 1 suite / 12 tests green.

**Resolution.** Fixed in the same change as the guard, so no shipped source-literal
caller produces a URL the guard would now reject.
**Disposition.** OWED — that `explorer.staging.cardano.org` actually serves https is
verified only as a code property (the scheme string emitted), never as a reachable
endpoint, because there is no network here. The same is owed for
`explorer.cardano.org` and `explorer.cardano-testnet.iohkdev.io`.
**Owner.** Whoever runs a staging build before release.

## F-3 (task-152) — `filterLogData` is renderer-only, so a new main-process logger sink is sanitized by hand and must be pinned by its own spec

The cv-2 sanitization floor (`cv-2-findings.md:2069-2080`) proves key-name redaction
through `filterLogData`, but that helper does not run on main-process logger calls:
a grep for `filterLogData` across `source/main` returns a single hit, and it is a
comment (`source/main/utils/setupLogging.ts:178-182`) explaining that one payload
deliberately bypasses it, not a call site. task-152 adds a new main-process sink, so
the floor binds it with no machinery to enforce it.

**Resolution.** The sink ships a bare protocol token and nothing else —
`logger.warn('Open external URL: rejected non-https scheme', { scheme })` at
`source/main/ipc/open-external-url.ts:28-30` — and the spec pins the absence
directly: `source/main/ipc/open-external-url.spec.ts:100-108` feeds
`http://user:pw@internal.example/secret` and asserts the serialized `logger.warn`
calls contain neither `internal.example` nor `secret`. Both floor anchors were
re-run and are unchanged at 26 and 27 tests.
**Disposition.** Record-only. Any future main-process logger sink in this slice needs
the same hand-enforcement plus its own containment assertion; do not assume
`filterLogData` covers it.
**Owner.** Every subsequent anchor-1 task that logs from `source/main`.

## F-4 (task-152, rides with task-151) — `setWindowOpenHandler` is a second, unguarded path into the OS shell that also logs the full URL

`source/main/index.ts:276-286` calls `shell.openExternal(url)` at `:283` with no
scheme check, and logs the whole URL first at `:279-281`
(`logger.info('Prevented creation of new browser window', { url })`). It is correctly
outside task-152's file scope, but task-151 Step 9 renders the anchor as
`<a href target="_blank" rel="noopener noreferrer">`, so a modifier click, a middle
click or any `window.open` bypasses the hardened IPC path entirely and writes the
anchor URL into a main-process log — defeating both invariant #3 and the
sanitization floor at once.

**Resolution.** Not fixed here; task-152 changed no file under `source/main` other
than the channel module and its spec.
**Disposition.** A **task-151 blocker**: that task must either route the anchor click
through `openExternalLink` or harden `setWindowOpenHandler` the same way.
**Owner.** task-151.

## F-5 (task-152) — The rejection is fire-and-forget, so a blocked URL surfaces as an unhandled promise rejection in the renderer console rather than a visible error

`source/renderer/app/stores/AppStore.ts:80-83` is
`openExternalLink(url, event) { if (event) event.preventDefault(); openExternalUrlChannel.send(url); }`
— the promise `send()` returns is discarded. `handleOpenExternalUrl` now returns
`Promise.reject(new Error('Rejected non-https external URL'))`
(`source/main/ipc/open-external-url.ts:31`), so a blocked URL produces console noise
in the renderer instead of user-facing feedback.

**Resolution.** Implemented as specified (`anchor-1-implementation-guide.md:929-933`
pre-declares this consequence). Console noise, not a crash, and after F-2's fix no
source-literal caller produces a non-https URL.
**Disposition.** Record-only, but it is the observable cost of the silent-rejection
decision and it compounds F-1: the runtime-sourced producers there are exactly the
callers a user would experience as "the link does nothing".
**Owner.** Recorded for the user alongside F-1's product decision.

## F-6 (task-152) — AC-3 is discharged negatively: this change renders no anchor link at all, and the anchor `<dd>` is still deliberately inert

"Anchor URL rendering remains gated on this hardening landing" is a criterion the
task passes by *not* rendering. `git status --short` lists no file under
`source/renderer/app/components/governance/`, and
`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorSection.tsx:55-57`
still emits `<dd className={styles.anchorValue}>{anchor.url}</dd>` beneath its
"Deliberately inert text" comment. task-152 has `dependencies: []` and is first in
the phase build order, so the guard is on disk before any anchor-render task starts.

**Resolution.** Green as a negative criterion; the https-gated link itself is
task-151 Step 9's deliverable, three commits later.
**Disposition.** Rides with task-151 — the ownership move (the gate is task-151's,
not task-152's) is recorded in the planning review at
`.agent/plans/governance/drep-discovery/task-plans/anchor-1-code-review.md:204-215`.
**Owner.** task-151.

## F-7 (task-152) — The close-out bookkeeping was not discharged by the implementation or fix passes; the tracker half is closed by this record, the commit is not

The implementation pass and the round-2 fix pass were both instructed not to commit
and not to edit the tracker JSON, and both complied — so at verification time the
row still read `"status": "pending"` with no `statusReason`, `evidence` or
`updatedAt`, and `git log -1` was still `33c02840a docs(gov): add anchor-1 slice
planning docs` with all four work files uncommitted. This is missing bookkeeping,
not broken code: every Verify gate, including "nothing outside the intended files
changed", passed on its literal wording.

**Resolution.** This scribe pass flips the tracker row to `complete` with
`statusReason`, `evidence` and `updatedAt: 2026-07-29`, in the sibling key order
prescribed at `anchor-1-implementation-guide.md:845-861`.
**Disposition.** OWED — the single close-out commit
`fix(gov): task-152 restrict open-external-url to the https scheme`
(`anchor-1-implementation-guide.md:869`) was still unmade when this file was written,
and `nix fmt` cannot run in this devcontainer at all (`node_modules/.bin/prettier
--write` over the four explicit paths is the substitute and is clean). Neither a
browser click-through nor a ja-JP visual pass was possible either; neither is
required by task-152, which changes no UI and no copy, but both are recorded as
not-run rather than claimed.
**Owner.** Whoever closes the task; the `nix fmt` obligation stays user-owned.

## F-8 (task-152) — The close-out commit landed and touches seven paths, not the five the guide's final Verify run names; the extra two are this slice's own plan docs

F-7's open half is discharged: the work is committed as `3a9b36daa`
`fix(gov): task-152 restrict open-external-url to the https scheme` — one Conventional
Commits subject line, no body, no trailer, exactly the subject prescribed at
`.agent/plans/governance/drep-discovery/task-plans/anchor-1-implementation-guide.md:869` —
and `git status --porcelain` is empty against it.

The guide's last check is `# 8. Nothing outside the five files changed.`
(`anchor-1-implementation-guide.md:907`), where the five are the four source/test paths
plus the tracker JSON. The commit carries **seven**: those five, plus
`research/anchor-1-findings.md` and `task-plans/anchor-1-code-review.md`. Both are plan
documentation under `.agent/`; no source file and no governance component outside the
four named paths is in the diff, which is the property AC-3 and Step 6 actually depend
on. The discrepancy is in the guide's arithmetic, not in the work: Step 8
(`:845-861`) asks in prose for a `statusReason`, an `evidence` array citing plan docs
and a close-out record, all of which necessarily add files, but the count in run 8 was
never updated to match.

No i18n catalog appears in the diff, so `yarn i18n:manage` was correctly not required
and was not run — this task changes no copy.

**Resolution.** Read run 8 as "no source file outside the four named paths", which is
what it is protecting; the literal count of five is stale for any task whose Step 8
also writes plan docs. Future anchor-slice guides should state the check as a
source-tree property rather than a file count.
**Disposition.** Record-only for task-152 — the commit is correct as landed and the
tracker `evidence` array cites both new docs.
**Owner.** Whoever authors the next anchor-slice implementation guide.

## F-9 (task-149) — Two of Step 12's five prettier paths are dirty at HEAD, so the mandated format-then-commit step forces a churn decision that no agent in this environment may take alone

Every `path:line` in F-9 through F-11 was opened at HEAD `6d38d2bfb` with the task-149
work uncommitted in the tree. Step 12 (`anchor-1-implementation-guide.md:2007-2026`)
prescribes `prettier --write` over five explicit paths (`:2013-2018`) before the
commit, but `prettier --check` over those five flags exactly two —
`source/common/utils/logging.ts` and `tests/jest/security/governance-sanitization.spec.ts` —
and both are proven pre-existing HEAD drift, not this task's lines: piping
`git show HEAD:<file>` through `prettier --stdin-filepath <file>` reproduces the
identical hunks, the `Object.keys(value).reduce(` reflow (HEAD `:57-67`, working tree
`:72-82` after the fifteen inserted lines) and the
`(MatomoTracker as unknown) as jest.Mock` double-paren cast (HEAD `:546`, working tree
`:629`). The second is the known prettier 2.1.2 oscillation on that construct. The
other three paths — `source/main/governance/AnchorFetchService.ts`,
`source/common/types/governance.types.ts`, `tests/jest/governance/AnchorFetchService.spec.ts` —
check clean, so no line task-149 added is prettier-dirty.

**Resolution.** `--write` was NOT run on the two dirty files: doing so would fold
unrelated reformat churn into a `feat(gov)` commit, and F-10 in `cv-2-findings.md`
already records that the pre-existing dirty set at HEAD is repo-wide. The literal
Step 12 command is therefore unexecutable as written without a scope decision.
**Disposition.** Raised to the user with the commit itself (F-11): either absorb the
two-hunk churn in the task commit, split it into its own formatting commit, or leave
both files to the user-owned `nix fmt` pass at merge.
**Owner.** Whoever makes the task-149 commit; the `nix fmt` obligation stays user-owned.

## F-10 (task-149) — Every transport guard is proven only against mocked `https.request` and `dns.promises.lookup`; no live fetch, no real TLS chain and no real anchor vector has ever been exercised

The service spec's own header states the method: deterministic `jest.mock` over
`https` and `dns` (`tests/jest/governance/AnchorFetchService.spec.ts:24-33`, mocks
bound at `:40-41`), and the main-process floor case in the security suite likewise
spies the same two modules (`tests/jest/security/governance-sanitization.spec.ts:661-662`).
Under that method the SSRF tables, the pinned-lookup rebinding defence
(`source/main/governance/AnchorFetchService.ts:217-229`), the shared 10-second budget
(`:9`, `:297-308`), the dual size guards (`:258`, `:267`), the content-type allow-list
(`:245-249`) and the 3xx rejection (`:233-238`) are proven as code properties only.
TLS default verification in particular is proven solely as the *absence* of any
`rejectUnauthorized` token (guide check `:2064-2066` returns no output) — Node's
actual certificate validation has never run against a real chain here, and no SIPO
or Cardano Academy anchor has been fetched end to end, because this devcontainer has
no network.

**Resolution.** Not resolvable in this environment; recorded so the mocked greens are
never re-told as live ones. Same class as F-2's unreachable-endpoint caveat.
**Disposition.** OWED — a networked run (dev build or release verification) must fetch
at least one real anchor vector over https and observe a TLS failure path against a
bad certificate before the transport floor is called live-proven.
**Owner.** Whoever runs a networked build before release; task-150/151 inherit the
caveat for their own offline proofs.

## F-11 (task-149) — The close-out commit was again not discharged by the implementation or review passes; the tracker half is closed by this record, the commit is not

Same shape as F-7: the implementation, verification and review passes were each
instructed not to commit, and complied. At the time of this record
`git status --porcelain` shows three modified files
(`source/common/types/governance.types.ts`, `source/common/utils/logging.ts`,
`tests/jest/security/governance-sanitization.spec.ts`) and two untracked
(`source/main/governance/AnchorFetchService.ts`,
`tests/jest/governance/AnchorFetchService.spec.ts`) against HEAD `6d38d2bfb`. This
scribe pass flips the task-149 tracker row to `complete` with `statusReason`,
`evidence` and `updatedAt: 2026-07-29` in the sibling key order.

**Resolution.** Bookkeeping, not broken code: every Verify gate at
`anchor-1-implementation-guide.md:2027-2069` measured green (39 / 35 / 27 / 206).
**Disposition.** OWED — the single commit
`feat(gov): task-149 add the hardened anchor fetch service` (`:2024`), one subject
line, no body, no trailer, blocked only on the F-9 prettier scope decision.
**Owner.** Whoever closes the task; F-8's lesson applies — the commit will also carry
the tracker row and the two plan docs, which is correct and not a scope breach.

## F-12 (task-150) — The close-out commit is again undischarged, but unlike task-149 it is not blocked on any prettier scope decision

Same shape as F-7 and F-11: the implementation, verification and review passes were
each instructed not to commit, and complied. At the time of this record
`git status --porcelain` shows four modified files (`source/common/ipc/api.ts`,
`source/common/types/governance.types.ts`, `source/main/ipc/index.ts`,
`source/renderer/app/ipc/governanceChannel.ts`) and ten untracked — the two
services (`source/main/governance/AnchorVerificationService.ts`,
`source/main/governance/anchorCache.ts`), the IPC handler and its spec
(`source/main/ipc/governanceAnchorChannel.ts`, `:spec.ts`), the two Jest suites
(`tests/jest/governance/AnchorVerificationService.spec.ts`,
`tests/jest/governance/anchorCache.spec.ts`) and the four fixture files under
`tests/mocks/governance/` — against HEAD `71ad2b4a1`. The commit F-11 recorded as
OWED has since landed as that HEAD, with a reworded subject
(`feat(gov): task-149 add the ssrf-guarded https anchor fetch transport`) rather
than the guide's proposed line at `anchor-1-implementation-guide.md:2024`. The F-9
churn decision does not recur here: all eleven Verify step-7 paths (`:2965-2976`)
pass `prettier --check` as they stand — neither of the two HEAD-drifted files is in
the list — so `--write` would be a no-op, step 8's digest regeneration (`:2978`) is
moot, and the committed fixture digest still reproduces (step 1 at `:2925-2930`
prints nothing, re-run in this scribe pass).

**Resolution.** Bookkeeping, not broken code: every task-150 Verify gate measured
green (9 / 13 / 2 tests across the three new suites, 159 passed governance tests
against the >=120 floor, 35 / 27 on both sanitization anchors, lint 0 errors at
5615 warnings), and this scribe pass flips the tracker row to `complete`.
**Disposition.** OWED — the single commit
`feat(gov): task-150 verify, cache and parse DRep anchor bytes` (`:2986`), one
subject line, no body, no trailer; per F-8's lesson it will also carry the tracker
row and the two appended plan docs, which is correct and not a scope breach.
**Owner.** Whoever closes the task; the `nix fmt` pre-merge pass stays user-owned.

## F-13 (task-151) — AC-4 closes mechanism-only: the real SIPO body bytes have never been fetched and their digest never checked against the on-chain dataHash, and the carry now leaves the slice

The acceptance table splits AC-4 explicitly (`anchor-1-implementation-guide.md:3931`):
the mechanism half is green — the real preprod on-chain pair
(`dataHash: 9e8cb2b0f4c2...f38e1` at
`research/drep-state-preprod-epoch295-sample.json:2853`, `url` at `:2854`) drives the
store tests and the Storybook fixture, and task-150's committed mock vector drives the
verify path — but the content half is OWED: nothing in the repo contains the real
CIP-119 body bytes from `https://sipo.tokyo/drep/SIPO.jsonld`
(`tests/mocks/governance/README.md:12` names both real vectors), so no offline run can
prove their Blake2b-256 digest equals the on-chain hash. task-150's close-out recorded
this as "carried forward to task-151 AC-4"; task-151 is now closing and the carry has
no later anchor-1 task to land in.

**Resolution.** Not resolvable in this environment — there is no network. Same class
as F-2 and F-10: a mocked or committed-fixture green must never be re-told as a
live-vector one.
**Disposition.** OWED — a networked run must fetch at least one real CIP-119 body
(SIPO mainnet or Cardano Academy preprod), hash it, and compare against the on-chain
`dataHash` before AC-4 is called fully green.
**Owner.** Whoever runs a networked build before release; the carry escalates to the
Planner at slice close rather than to any remaining anchor-1 task.

## F-14 (task-151) — The anchor-state story is authored but unrendered: DRepDetail.stories stays unregistered until task-172, so no visual or ja-JP overflow pass has run

Step 17 adds the anchor-state knob to
`storybook/stories/governance/DRepDetail.stories.tsx` but forbids touching the
registry: "Do **not** edit `storybook/stories/index.ts`. Registering
`DRepDetail.stories` is task-172's" (`anchor-1-implementation-guide.md:3848`). Live
state confirms it: `storybook/stories/index.ts:16-18` imports only
`Governance.stories`, `DRepDirectory.stories` and `CurrentVoteSummary.stories`, and no
line imports `DRepDetail.stories`. There is also no browser in this devcontainer, so
even a registered story could not be visually checked here.

**Resolution.** By design, not an omission — the guide itself says "record it as an
in-slice carry, not as done" (`:3848`).
**Disposition.** OWED — the Storybook visual pass and the ja-JP overflow check for the
verified-name block and the three new source-label variants run only after task-172
registers the file, and in an environment with a browser.
**Owner.** task-172 registers; whoever runs the visual pass afterwards closes it.

## F-15 (task-151) — The close-out commit is again undischarged, and the Step-7 format pass re-opens F-9's churn decision on two files whose drift predates this task

Same shape as F-7, F-11 and F-12: the implementation, verification and review passes
were each instructed not to commit, and complied. At the time of this record
`git status --porcelain` shows 24 modified files and one untracked
(`source/renderer/app/components/governance/drep-detail/DRepDetailAnchorContent.tsx`)
against HEAD `aa77b475c`. The F-12 commit has since landed as that HEAD. Unlike
task-150 but like task-149, the guide's format step is not clean as it stands: two of
the touched files fail `prettier --check` — `GovernanceQueryService.ts` at `:65-66`
and `:396-397`, `DRepDirectory.stories.tsx` at `:358-363` — and every drifted region
is the 2.1.2 oscillation shape sitting entirely outside this task's hunks (the GQS
hunk is at `:518`, the stories hunks at `:46`, `:54`, `:72`), so it is pre-existing
HEAD drift, not this task's formatting debt. Running the guide's
`prettier --write` list (`anchor-1-implementation-guide.md:3877-3893` area, Step 7)
would fold that pre-existing churn into the task commit.

**Resolution.** Bookkeeping plus one scoped decision, not broken code: every Verify
gate measured green (20 / 43 / 5 / 5 on the changed suites, 101 / 38 unmoved, 35 + 27
on the floor anchors, 95 95 True [] [] on i18n parity, lint 0 errors).
**Disposition.** OWED — the single commit `feat(gov): task-151 render the verified
givenName and expose metadata completeness` (`:3921`), one subject line, no body, no
trailer; per F-8's lesson it also carries the tracker row and the two appended plan
docs. The two drifted files may be committed as they stand — the user-owned `nix fmt`
pre-merge pass settles them — or normalized first by an explicit user decision, per
F-9.
**Owner.** Whoever closes the task; the `nix fmt` pre-merge pass stays user-owned.

## F-16 (task-172, closes F-14's registration half) — The three orphaned story files are finally registered, but the visual and ja-JP overflow pass is still owed and now has a named risk: `!!!高価値` in the fixed-width card top row

F-14's blocker is discharged: `storybook/stories/index.ts:19-21` now imports
`DRepCategoryBadge.stories`, `DRepDetail.stories` and `DRepDirectoryBanner.stories`,
and the cohort knob makes all four categories reachable from one control. What cannot
be discharged here is the seeing: there is no browser in this devcontainer,
`jest.config.js` roots exclude `storybook/` (only `tsc --noEmit` covers it, exit 0),
and `yarn storybook:build` is red at HEAD for unrelated reasons and is not a
substitute. The specific thing to look at when the pass finally runs: the ja-JP High
value label is `!!!高価値` (`ja-JP.json:320`), and the badge renders inside the card
top row where the favorite toggle, status badge, category badge and DRep id share one
flex row (`DRepCard.tsx:109-125`); the detail call site is `DRepDetail.tsx:114`.

**Resolution.** Registration resolved by this task exactly as F-14 predicted; the
visual judgement is not resolvable in this environment.
**Disposition.** OWED — `yarn storybook` plus a human eye, en-US and ja-JP, all four
categories at both call sites (AC-6 second half, `anchor-1-implementation-guide.md:4907-4911`).
**Owner.** Whoever runs the visual pass before release closes it.

## F-17 (task-172) — The Primary badge and the "With metadata" filter now mean different things: verified anchor content versus on-chain anchor presence

After this task the classifier's metadata input is `cohort.verifiedMetadataIds`
(`DRepCategoryBadge.tsx:80`), fed exclusively from the store's verified-only computed
(`GovernanceStore.ts:285-291`, `state === 'verified'`), and `anchor` is structurally
absent from `DRepCategorySource` (`DRepCategoryBadge.tsx:60-63`). But `filterDReps`
still implements the user-facing "With / Without metadata" filter on on-chain anchor
presence (`helpers.ts:198` and `:201`). A DRep whose anchor exists on-chain but failed
Blake2b-256 verification therefore matches "With metadata" and renders *Non-metadata*.
The guide scopes this deliberately (AC-2 discharge and the design-owner note at
`anchor-1-implementation-guide.md:4920-4923`): re-pointing the filter is a copy and
semantics change that needs its own tracker row.

**Resolution.** A recorded product decision, not a defect — the badge is the
anti-misleading surface and had to move to verified content with the anchor pipeline;
the filter's semantics were out of scope.
**Disposition.** Escalate to the Planner: either the filter gains a verified-content
mode with its own copy, or the divergence is accepted and documented in the design doc.
**Owner.** Design owner decides; Planner rows the follow-up if wanted.

## F-18 (task-172) — The binding priority order makes High value suppress the expiry hint for an in-cohort, verified, above-median DRep 7–12 epochs from expiry

`shared-design-tokens.md:39` binds High Value > Threshold > Primary > Non-metadata,
and the classifier implements it verbatim (`DRepCategoryBadge.tsx:87-96`); the spec's
case 5 pins exactly this tie-break (`drepActivity: 10`, in cohort, verified, above
median → `highValue`). Consequence: such a DRep never shows "Approaching expiry —
review before delegating", because the Threshold branch is unreachable once the High
value condition holds. That is what the binding rule says; the guide flags it as
possibly not what UX wants (`anchor-1-implementation-guide.md:4917-4919`).

**Resolution.** Implemented as bound — deviating in code was expressly forbidden.
**Disposition.** Recorded for the design owner; any change is a §1a spec change
first, then a classifier row, never a silent code fix.
**Owner.** Design owner.

## F-19 (task-172) — §1a specifies no colour for High value, so the violet fallback in the new `--badge-highlight-*` tokens awaits theme confirmation

Step 3 introduces `--badge-highlight-fg` / `--badge-highlight-bg` with violet
fallbacks `#7a5af8` / `rgba(122, 90, 248, 0.12)`
(`DRepCategoryBadge.scss:26-27,30`), chosen to stay distinguishable from the green
Active status badge that sits beside it in the same card row. No theme file defines
either token yet, so every theme currently renders the fallback. The guide records
this as an outstanding design-owner confirmation
(`anchor-1-implementation-guide.md:4924-4926`).

**Resolution.** Not resolvable here — token palettes are a design-owner call and
theme wiring is its own change.
**Disposition.** OWED — the design owner confirms or replaces the violet, and the
tokens get real definitions in the theme files.
**Owner.** Design owner; theme wiring goes to whoever rows it.

## F-20 (task-172) — The close-out commit is again undischarged, and for the first time in the slice the format step is clean as it stands

Same shape as F-7, F-11, F-12 and F-15: the implementation, verification and review
passes were each instructed not to commit, and complied. At the time of this record
`git status --porcelain` shows 25 modified files and nothing untracked on top of
HEAD `351467833` (the F-15 commit has since landed as that HEAD). Unlike task-149 and
task-151, no churn decision rides on this commit: `prettier --check` passes on all 18
explicit paths in the guide's format step (`anchor-1-implementation-guide.md:4801-4820`),
so F-9's oscillation files are simply not in this task's path list.

**Resolution.** Bookkeeping only, not broken code: every Verify gate measured green
(22 / 48 / 21 / 8 / 49 on the changed suites, 409 passed + 12 skipped on the sweep,
35 + 27 on the floor anchors, `97 97 true` then `[]` on i18n parity, lint 0 errors).
**Disposition.** OWED — the single commit `feat(gov): task-172 ground the DRep
category badge in cohort membership` (`:4793`), one subject line, no body, no
trailer; per F-8's lesson it also carries the tracker row and the appended plan docs.
**Owner.** Whoever closes the task; the `nix fmt` pre-merge pass stays user-owned.
