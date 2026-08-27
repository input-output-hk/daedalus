# Findings

> **Things discovered while doing something else, written down rather than acted on.**

A finding is a defect, fragility, or open question that was uncovered during work
whose scope did not include fixing it. Recording it here keeps two things from
happening: the discovery being lost when the branch that made it merges, and the
branch that made it quietly growing to cover it.

## How this differs from the neighbours

| Folder | Holds |
|---|---|
| `plans/{feature}/research/` | Investigation supporting a specific plan's decisions |
| `SOPs/` | A repeatable procedure for something already solved |
| `findings/` | Something wrong or unclear that nobody has picked up yet |

A finding graduates out of this folder when it becomes a plan, an SOP, or a
commit. It is deleted when it is fixed, not marked resolved in place.

## What a finding needs

- The measurement that establishes it, so the next reader does not repeat the work
- What it does and does not affect today
- Which arc or area would own it
- Explicitly, what was **not** done and why

Filenames use a zero-padded numeric prefix, matching the convention in
`plans/*/research/`.
