# task-plans-ux-refinement

Per-task working docs for the **Mithril partial sync UX refinement sprint** (issue #10, Phase 2).
This tree is the Phase-2 equivalent of the original `../task-plans/` + flat `../research/` structure,
consolidated and labeled for the refinement sprint.

## Driving docs (sources of truth)
- Orchestration prompt: `../prompt-ux-refinement.md`
- Tasks (high-level): `../mithril-partial-sync-ux-refinement-tasks.json`
- PRD (decisions D1–D10): `../mithril-partial-sync-ux-refinement-prd.md`
- Gap dossier: `../research/19-ux-refinement-state-and-gaps.md`

## Layout
Tasks are split into one subdirectory per phase. A task ID's hundreds digit selects its phase folder
(`task-ux-1xx` → `phase-1/`, `task-ux-2xx` → `phase-2/`, …).

```
phase-1/  Cross-process availability contract + behind-ness signal   (task-ux-101, task-ux-102)
phase-2/  Backend UX-enabling correctness                            (task-ux-201..204)
phase-3/  Renderer discovery, gating, and confirmation               (task-ux-301..303)
phase-4/  Renderer progress and error overlay honesty                (task-ux-401..404)
phase-5/  Hygiene, Storybook, and automated coverage                 (task-ux-501..503)
phase-6/  Holistic EN + JA copy pass                                 (task-ux-601)
phase-7/  Backend-correctness re-validation + manual QA / rollout    (task-ux-701, task-ux-702)
```

## Files per task (four)
Inside the matching phase folder, each task `task-ux-NNN` owns:

| File | Role |
|------|------|
| `task-ux-NNN.md` | Canonical task plan + final outcome — single source of truth. Must be detailed enough for a smaller model to implement from it alone (see the prompt's "lower tasking detail" rule). |
| `task-ux-NNN-plan-review.md` | Append-only Planner/Critiquer transcript. |
| `task-ux-NNN-impl-review.md` | Append-only Implementation/Code-Review transcript. |
| `task-ux-NNN-research.md` | Durable findings: decisions, gotchas, evidence, residual gaps (this sprint's per-task research note). |

Do not write this sprint's docs into the original `../task-plans/` or flat `../research/`, and do not
backfill docs for the original completed `task-001…task-401` sprint.
