# UX-Refinement Findings

> Recorded at phase close, 2026-07-23. Durable, reusable findings from the
> ux-refinement implementation (task-159 … task-169) and its code-review log.
> Companion docs: [ux-refinement-PRD.md](../task-plans/ux-refinement-PRD.md) |
> [ux-refinement-code-review.md](../task-plans/ux-refinement-code-review.md)

---

## F-1 — Two-phase IPC, as built

One new request/response channel (`GOVERNANCE_DREP_STAKE_CHANNEL`), not streaming
or push (PD-3). The renderer owns the sequencing: `GovernanceStore.fetchDRepList`
awaits Phase 1 on the existing list channel (paints at `Loaded`, `votingPower`
reset to `null` on **every** run — PD-5), then `_enrichVotingPower` requests the
stake channel and merges `new BigNumber(<decimal string>)` by CIP-129 `drepId`
**string equality** — sound only because both payloads derive ids through the same
`_credentialToDRepId` → `Cardano.DRepID.cip129FromCredential` path. Phase-2
failure sets `VotingPowerEnrichState.Failed` / `isRankingUnavailable` and never
touches the painted list. Dedup is two-layered: per-phase in-flight promise slots
in the service, and the store guard extended over the enrich window
(`votingPowerState === Loading` short-circuits re-entry). Error transport reuses
the `__governanceError` plain-object contract via a shared `toGovernanceIpcError`
(an `Error` instance would flatten to `{ name, message }` under structured clone
and lose `details`). Reusable seam: a future expensive enrich (e.g. anchor
metadata) should follow this exact shape — one channel per phase, renderer
sequences and merges, failure degrades without corrupting the cheap phase.

## F-2 — DRepEmptyState / DRepErrorBanner build-out scope

The design docs referred to these components as existing; they existed nowhere in
the tree (grep-verified — PD-1). They were built at the design's component seams
in `components/governance/_shared/` (live naming; the design doc's map says
`shared/` — drift recorded as PD-2, no doc edit needed) with variant-union props
but **only this phase's variants implemented**: `DRepEmptyStateVariant = 'noSync'`
and `DRepErrorBanner` `'rankingUnavailable'`. The unions carry comments naming the
future owners (`noResults`/`selfnode` empty states; `refresh failed` banner). A
later slice adds a variant by extending the union + the `messageByVariant` map —
no structural change. No `.scss.d.ts` files: the global `declare module '*.scss'`
in `source/renderer/declaration.d.ts` types all SCSS modules (PD-12); generated
`.scss.d.ts` from the compile pass are gitignored.

## F-3 — task-169 PART B: standing verification debt (Nix shell)

`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts` proves the exact per-phase
argv (2 eras × 3 queries × 2 network-flag forms = 12 cases) against the real
`cardano-cli` parser, with `CARDANO_NODE_SOCKET_PATH` stripped from the child env
so a parser-clean invocation can only die at the socket stage. It self-skips via
`describe.skip` when the binary is not on PATH; in this devcontainer it reports
**12 skipped, 0 failed** — the skip gate itself is proven, the positive parse run
is recorded debt. Whoever first works in the Nix shell should run this suite and
record the result (the FP-1 prepend-vs-append regression class is structurally
unclosable at the mocked-spawn unit level).

## F-4 — task-166 remainder (locked `manual_execution`)

Remaining manual items, named in the plan Risks mitigation: (1) mainnet
`drep-state` fixture capture; (2) p50/p95 latency for both phases against a real
synced node, re-deriving the provisional 30 s stake budget; (3) promotion of real
captures into a committed `tests/jest/governance/` fixture — which also confirms
the real `drep-stake-distribution` container/key shape (the parser is dual-shape
tolerant precisely because this is unverified — PD-6). A preprod fixture exists:
`research/drep-state-preprod-epoch295-sample.json` (epoch 295, 258 DReps).

## F-5 — era-retry gate: structural signal at the spawn boundary

The conway fallback is gated on error class, not message text: `_runCliQuery`
classifies a non-zero exit whose stderr matches the optparse-applicative usage
signature (`/(invalid (option|argument)|missing:|usage:)/i`) as
`GovernanceQueryErrorType.UsageError`, and `_shouldRetryWithConway` is a pure
`queryErrorType === UsageError` check. A node-side `QueryFailed` — even one whose
stderr contains "latest" and "era" — can never trigger a spurious retry, and no
production error message needs to avoid particular words (PD-8).

## F-6 — Jest gotcha: renderer logging module crashes on import

`source/renderer/app/utils/logging.ts` dereferences `global.environment` at
import time, which is undefined under Jest — a suite importing it (e.g. to
`jest.spyOn` the logger) crashes before any test runs. `jest.mock` of the module
is the smallest fix; `GovernanceStore.spec.ts` is the precedent, with a 2-line
why-comment. Assertions against the mock still capture exactly what callers pass.

## F-7 — `yarn i18n:manage` works under Node v24

First exercised in this feature (slices 1-3 added no copy): exit 0, and
idempotent — a re-run leaves the working tree byte-identical, so the committed
`defaultMessages.json` / `translations/messages.json` rewrites are exactly what
the manager produces. The anticipated environment failure never materialized.
`yarn compile` remains the unreliable script under Node v24 — keep running
`node_modules/.bin/tsc --noEmit` directly (slice-3 precedent).

## F-8 — Step-12 `filterLogData` grep now has one expected hit

The guide's close-out grep (`grep -n "filterLogData"
source/main/utils/setupLogging.ts`, "must print nothing") now prints exactly one
line: the `logDRepStateSnapshot` doc comment stating the deliberate bypass. That
is documentation of the boundary, not a functional call — the gate's intent (the
snapshot writer must not route through `filterLogData`) holds. Future closers
should treat the single comment hit as the expected result.

## F-9 — prettier 2.1.2 ergonomics and the repo-wide-drift trap

The package scripts (`prettier:check`, bare `yarn prettier`) bake in a repo-wide
`"**/*.*"` glob and trip on ~240 files of pre-existing HEAD drift — always scope
`node_modules/.bin/prettier` to the task's changed files. Scoped `--write` passes
can still emit collateral rewraps of already-drifted lines inside touched files
(seen in task-168's `config.ts`/`setupLogging.ts`; formatting-only). At phase
close the worktree carried 243 modified files that proved **byte-identical to
prettier(HEAD) output** on every file (a repo-wide prettier run had happened at
some point); the drift was verified file-by-file against `git show HEAD:<f> |
prettier --stdin-filepath <f>` before being stashed to restore the clean
baseline. Verify equality the same way before ever discarding such drift.

## F-10 — F-3 reopened: the argv smoke assertion rejects its own pass condition

F-3 above records the positive parse run as verification debt. On re-reading the
suite that framing is too kind: the assertion is inverted, so this is a live test
defect rather than an unexecuted one. `USAGE_SIGNATURE` at
`tests/jest/governance/GovernanceCliArgvSmoke.spec.ts:15` mirrors
`GovernanceQueryService.CLI_USAGE_SIGNATURE`
(`source/main/governance/GovernanceQueryService.ts:65-66`) and therefore carries
the `missing:` alternative — while the suite deliberately strips
`CARDANO_NODE_SOCKET_PATH` from the child env (`:50-56`) precisely so a
parser-clean invocation dies at the socket stage. The documented outcome of that
invocation against a real binary is `Missing: --socket-path`
(`research/slice-1-final-pass-findings.md:19-20`), which the regex matches
case-insensitively, so `expect(stderr).not.toMatch(USAGE_SIGNATURE)` at `:69`
fails on the exact stderr that proves the argv parsed. All twelve parameterized
cases are dead on arrival wherever `cardano-cli` is on PATH; the skip gate is the
only reason nobody has seen it. The suite's own comment at `:64-67` names the
rejections it means to catch correctly (`Invalid option` /
`Missing: (--mainnet | --testnet-magic NATURAL)` / unknown era) — the regex is
simply wider than the comment. Production classification is **unaffected**:
`_runCliQuery` always sets `CARDANO_NODE_SOCKET_PATH` (`:350`), so a node-side
failure never prints `Missing:` and the conway-fallback gate at `:397` holds on
the reasoning F-5 records; the mismatch is confined to the smoke lane, which
inverts that precondition by design. The first Nix-shell run should therefore
expect red, and the correction belongs in the test rather than the service
signature: narrow the smoke assertion to reject `Invalid option` and
`Missing: (--mainnet | --testnet-magic` while tolerating the expected
`Missing: --socket-path`.

## F-11 — task-169 PART B is `complete` on criteria only its skip gate exercised

Tracker consequence of F-10, recorded so the status is read for what it actually
proves. task-169 is `complete` with two PART B acceptance criteria that describe
a real run — AC-4 ("a parse-only smoke test runs the real bundled cardano-cli
with the exact argv the service builds … and no `CARDANO_NODE_SOCKET_PATH`") and
AC-5 ("the test asserts the invocation clears the argument parser - failing only
on a socket/connection error") — plus AC-6, which permits the skip. In the only
environment the suite has ever run, AC-6 is what executed: AC-4 and AC-5 are
discharged by a gate, and F-10 shows AC-5's assertion would have gone red had it
run. This is not a mislabelled status on the vocabulary's own terms
(`README.md:12` — `complete` is implementation + focused tests + code review
clean, and all three happened), but nothing downstream owns the positive run:
task-125 is the only pending item that brings both a Nix shell and a synced node,
and its single acceptance criterion is the browse → evaluate → select → delegate
journey. The gap matters because the regression class this suite exists to close
already shipped broken once — the network flag must follow the subcommand,
discovered in the slice-1 live-app smoke test and fixed as FP-1
(`governance-drep-discovery-plan.md:138`) — and F-3 establishes it is
structurally unclosable at the mocked-spawn level, so no autonomous task can
substitute. Whoever runs the suite first should record the twelve-case result
verbatim; a skipped run does not discharge AC-4 or AC-5.
