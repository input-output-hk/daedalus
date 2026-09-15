Implementation: Iteration 1
Timestamp: 2026-09-16T22:05:00Z

**No scenario in this task has been executed.** The deliverable is the
procedure, the expected evidence and the operator checklist, which is what the
task-plan readme requires of a `manual_execution` task.

Changes made:
- `.agent/plans/asset-metadata-cache/task-plans/task-038.md`: ten scenarios, the
  per-platform preparation, and the checklist.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`:
  `task-038.status` to `blocked`, with the reason, and its implementation notes
  extended.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: a status log
  entry for phase 7, recording what was built and the five corrections this
  phase made to the document.

Files touched:
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`

No source file changed.

What was verified while writing the procedure, because a procedure written from
the PRD rather than from the repository is how an operator ends up looking in the
wrong place:

- The three preset URLs and the four networks they cover were read from
  `nix/internal/launcher-config.nix`, and selfnode's absence from that list is
  what scenario 10 exists for.
- The settings route and the menu label were read from `routes-config.ts` and
  `SettingsMenu.messages.ts`, so the operator is told to look under "Tokens" and
  not under a name that is only in this plan.
- The two refusal messages were taken from `en-US.json` verbatim, so scenario 5's
  pass condition is a string comparison rather than a paraphrase.
- The chain database path and the fact that it is resolved once, at startup, were
  read from `source/main/ipc/index.ts`, which is why the procedure says moving
  the chain takes effect on the next start.
- The log line scenario 7 greps for was taken from the source:
  `Asset metadata: pointer refused by the local check`.
- The retry interval scenario 9 waits out is one hour, and the window it has to
  fall inside is twelve, both read from `assetMetadataResolver.ts`.

Two corrections to documents this phase already committed, carried here because
they are one line each and both are compliance rather than content:

- `task-031.md` and `task-032.md` each cited a numbered internal analysis that a
  reader of this repository cannot open. `.agent/` ships publicly, so a citation
  a reader cannot resolve is not usable. Both now state the finding on its own
  terms: the `Store` constructor signature in `.agent/system/state-management.md`
  does not exist here, and `.agent/workflows/frontend.md` shows a react-intl API
  that this repository's pinned version predates.
- `task-038.md`'s selfnode scenario claimed the selection would read "Koios". It
  does not: the launcher configures no instance for selfnode, so the preset has
  no URL, nothing matches it, and the page resolves to "Custom index" with an
  empty field. Checked by evaluating `getAssetMetadataSourceIdFromUrl('')`, which
  answers `custom`. A procedure whose pass condition is wrong is worse than no
  procedure, so the scenario now says what the build does and why it is correct.

An editing pass was run over this procedure, `task-034.md` and the PRD's new
status log entry before the commit. It found one contradiction and six spelling
defects, all applied: the scope line said nine scenarios where the checklist and
the acceptance criteria say eight on three platforms and two on one. It also
questioned scenario 9's wait condition as a loose bound, which was right: it
asserted only the twelve-hour window and not the one-hour retry interval, so a
subject that had fallen into the day-long failure backoff would have passed. It
now asserts both, and names that failure.

The spelling changes move six words to American forms. That is a divergence from
the phases 1 to 6 plans, which use British forms 55 times and American none, and
it is the right way round: the rest of `.agent/` outside this plan uses American
forms 592 times against 77, so the plan set was the anomaly rather than the
standard. Recorded rather than reconciled: rewriting twenty-nine committed
documents to match is not this task's to do.

Checks: none run, because nothing outside `.agent/` changed. `git diff` over
`source`, `storybook`, `tests`, `package.json` and `yarn.lock` is empty.

Deviations from the approved plan:
- None.

Outcome: Procedure complete; execution pending an operator

Review of Iteration 1
Timestamp: 2026-09-16T22:10:00Z

Acceptance criteria, against the evidence:

1-4. **None of them are met, and none of them can be met here.** They are the
   operator's to meet. What this iteration is reviewed against is the
   `manual_execution` requirement: the plan must produce the exact procedure and
   expected evidence for an operator to finish it. It does, for ten scenarios,
   each with a pass condition and a named artifact.

Three judgements worth naming.

**Scenario 7 is the one that matters and it is not on the PRD's list.** Every
other scenario would pass against a build that believed whatever the index said.
It is also the most work to set up, which is why the procedure writes out what
the stand-in must answer rather than describing the idea.

**Scenario 9 cannot fail quickly.** A missing name is the expected state for the
first hour after a mint, so an operator running the list in one sitting will see
it in its unresolved state and have to come back. The criterion says what turns
that into a failure, and the `asset_resolution` query is what distinguishes
"waiting" from "gave up".

**Two scenarios need a developer to build a stand-in.** That is a real cost and
it is the reason this task is four hours rather than two. The alternative,
dropping them, would leave the two refusal paths and the whole untrusted-index
argument unchecked outside the unit suite.

The status is `blocked` rather than `pending` or `completed`, for the reason
`task-027` records: `pending` reads as not started and understates a procedure
that is ready to run, and `completed` would be false.

Decision: approved as a procedure. The task itself remains open until a signed
checklist comes back.
