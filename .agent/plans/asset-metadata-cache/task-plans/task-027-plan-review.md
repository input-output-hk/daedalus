Planner: Iteration 1
Timestamp: 2026-09-15T19:15:00Z

The first plan reproduced the seven bullets from the task graph, marked the
interaction mode `agent_execution`, and proposed running what could be run here
and describing the rest.

Critique:

- **The interaction mode is wrong and it is the whole shape of the task.** There
  is no display on this machine, no macOS, no Windows, no funded wallet and no
  selfnode cluster. `manual_execution` is what the task-plan readme calls for
  when truthful completion needs something this environment cannot do, and it
  requires the plan to produce the exact procedure and the expected evidence
  instead.
- **Running part of it and describing the rest is the failure mode to avoid.**
  Two of the nine scenarios could be partially approximated here and none of them
  honestly. A checklist with three boxes ticked by an agent that could not see a
  screen is worse than an empty one, because a reader cannot tell which is which.
- **"Rows render, nothing spins, nothing errors" is not a pass condition.** Each
  scenario needs an artifact: a screenshot, a log line, or a path. Several of
  these scenarios are about what does not happen, and a screenshot is weak
  evidence for an absence; the log and the directory listing are strong.
- **The selfnode case was missing and nothing else covers it.**
  `nix/internal/launcher-config.nix:448-450` adds `metadataUrl` only for networks
  that are not selfnode, so the endpoint for a selfnode run is chosen in code at
  `assetRegistryClient.ts:97-107` and nowhere else. If that branch were removed,
  a selfnode run would query the public registry and get a well-formed answer,
  which is why the failure is silent. The scenario needs a discriminator rather
  than an observation, and the discriminator is a ticker in the mock that no real
  issuer publishes, checked again with the mock stopped.
- **The mock's own entry has two properties an operator would report as bugs.**
  It carries no `policy` field, so its decimals must not be applied, and its
  `logo` decodes to ASCII text rather than raster bytes, so it is refused on
  media type and no logo appears. Both are correct and both are now stated in the
  scenario, so that a correct result is not filed as a defect.
- **The database path is the artifact, not a screenshot of it.** The whole point
  of the platform check is the exact string, and Windows is the platform it is
  most likely to differ on.

What changed in response: the mode set to `manual_execution` with an explicit
statement that no step has been executed; findings 1 to 7 with the paths and line
numbers an operator needs; nine scenarios each with an expected result and a
named artifact; the selfnode scenario added with its discriminator and its two
correct-looking oddities; a checklist an operator signs; the graph status set to
`blocked` with the reason rather than left `pending`, so that nothing downstream
reads it as done.

Scope guard: no source file changes, and a failing scenario opens a defect rather
than being fixed inside this task.

Outcome: approved
