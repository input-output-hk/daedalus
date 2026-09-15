Planner: Iteration 1
Timestamp: 2026-09-15T17:40:00Z

The first plan read the task's `targetPaths`, found that all five spec files
already exist and are substantial, and proposed adding cases to each until the
count looked respectable.

Critique:

- **The acceptance criterion is a checklist and the plan was not working from
  it.** "Every negative case named in the PRD Testing Strategy has a spec, listed
  by name" is satisfied by going through fourteen items in order and saying, for
  each, where it is or that it is not. Eight of them turn out to be already
  covered, which is the right answer and has to be shown rather than assumed.
- **Four items on that list belong to phase 7.** The URL validator, the source-id
  reduction, pointer resolution and the chain row all describe code nobody has
  written. Omitting them silently would make the closing note look complete when
  it is not; they go in Non-Goals by name.
- **"Add more cases" is not a criterion.** What makes a gap a gap is that nothing
  reaches a line. The plan now rests on a coverage run over the same file set,
  quoted before and after, and every addition is traced to an uncovered region.
- **The gaps the run finds are all of one kind, and the first plan would have
  missed every one of them.** The real HTTP transport has no coverage at all;
  seven database failure paths, both IPC handler catches and three verification
  failure paths are unreached. Those are the lines that run when something has
  gone wrong, which is when a wallet most needs the degradation the modules
  promise.
- **The second acceptance clause was going to be answered by asserting the
  renderer is fine.** It is fine, and saying so needs the numbers: five modules
  at or near 100 percent, one at 55.55, and the one at 55.55 is `sortAssets`,
  which this branch changed and which orders every list on every surface.
- **The database's own `close()` is the wrong driver for its catch blocks** and
  the first plan proposed it. It nulls the wrapper's reference, so the accessors
  return at the guard and never enter the `try`. The catch blocks are for a
  handle that is live to the wrapper and dead to the engine, which is a different
  state and needs a different driver.

What changed in response: findings 1 to 15, each with its file and line or its
measured percentage; the phase 7 items named in Non-Goals; the four helpers this
plan never touched named there too, so that "coverage went up" cannot be bought
by testing unrelated code; the loopback server's dependence on the Nix sandbox
named as a risk to drive rather than to assume.

Scope guard: no source file changes. If a case cannot be written without changing
the module under test, that is recorded as a finding, not fixed here.

Outcome: approved
