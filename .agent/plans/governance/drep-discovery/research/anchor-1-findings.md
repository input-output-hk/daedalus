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
