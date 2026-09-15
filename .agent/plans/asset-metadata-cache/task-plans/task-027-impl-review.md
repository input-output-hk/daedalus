Implementation: Iteration 1
Timestamp: 2026-09-15T19:25:00Z

**No scenario in this task has been executed.** The deliverable is the procedure,
the expected evidence and the operator checklist, which is what the task-plan
readme requires of a `manual_execution` task.

Changes made:
- `.agent/plans/asset-metadata-cache/task-plans/task-027.md`: nine scenarios, the
  per-platform path table, and the checklist.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`:
  `task-027.status` to `blocked`, with the reason, and its implementation notes
  extended.

Files touched:
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

No source file changed.

What was verified here, because a procedure written from the PRD rather than from
the repository is how an operator ends up looking in the wrong directory:

- The three database paths were derived from
  `nix/internal/launcher-config.nix:193-199` and
  `source/main/assets/assetMetadataDb.ts:192-196` rather than copied from the
  PRD's table, and they agree with it.
- The log path and level were checked:
  `nix/internal/launcher-config.nix:210-213` and `:244` give
  `<stateDir>/Logs/pub`, and `source/main/utils/setupLogging.ts:25,30` name the
  file and set the file transport to `debug`, so every line these modules emit
  reaches it. Without that, half the evidence in the procedure would not exist.
- The exact log strings an operator greps for were taken from the source:
  `Asset metadata: query failed`,
  `Asset metadata cache: unavailable, answering as empty`,
  `Asset metadata cache: recreating the database` and
  `Asset registry: request refused`.
- The selfnode branch was read at `assetRegistryClient.ts:97-107` and the
  configuration that makes it necessary at
  `nix/internal/launcher-config.nix:448-450`.
- The mock registry's single subject was read from
  `utils/cardano/native-tokens/registry.json`: no `policy` field, and a `logo`
  value that base64-decodes to the ASCII text "Almost a logo". Both produce
  behaviour an operator would otherwise report as a defect, and both are called
  out in the scenario.
- `MOCK_TOKEN_METADATA_SERVER_PORT` defaults to 0 (`source/main/config.ts:170-172`)
  and the procedure for setting it is `README.md:165-184`.

Checks: none run, because nothing outside `.agent/` changed. `git diff` over
`source`, `storybook`, `tests`, `package.json` and `yarn.lock` is empty.

Deviations from the approved plan:
- None.

Outcome: Procedure complete; execution pending an operator

Review of Iteration 1
Timestamp: 2026-09-15T19:30:00Z

Acceptance criteria, against the evidence:

1-4. **None of them are met, and none of them can be met here.** They are the
   operator's to meet. What this iteration is reviewed against instead is the
   `manual_execution` requirement from the task-plan readme: the plan must
   produce the exact procedure and expected evidence for an operator to finish
   it. It does, for nine scenarios, each with a pass condition and a named
   artifact.

The judgement worth naming is the eighth scenario. It is not on the PRD's Manual
QA list, and it is the only one that catches a failure which looks like a
success: with the selfnode branch removed, a selfnode run queries the public
registry and gets a well-formed answer for a subject that does not exist there,
so nothing appears wrong. It needs a discriminator rather than an observation,
and the discriminator is a ticker in the mock that no real issuer publishes,
checked a second time with the mock stopped.

The second judgement is the evidence. Four of the nine scenarios assert that
something does not happen: no spinner, no error dialog, no notice on the second
run, no ticker with the mock stopped. A screenshot is weak evidence for an
absence, so those steps ask for a log line, a directory listing or a second
screenshot of the same view under the opposite condition.

The status is `blocked` rather than `pending` or `completed`. `pending` reads as
not started, which understates a procedure that is ready to run; `completed`
would be false. Anything downstream that reads the graph now sees why it cannot
proceed and what would unblock it.

Decision: approved as a procedure. The task itself remains open until a signed
checklist comes back.
