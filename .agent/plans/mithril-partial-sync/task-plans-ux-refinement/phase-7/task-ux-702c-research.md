# task-ux-702c — Research notes (durable findings from planning and implementation)

> Durable, reusable findings surfaced while orchestrating and implementing task-ux-702c.
> Plan: `task-ux-702c.md`. Decisions: `task-ux-702c-decisions.md`.

## 1. Phase-7 task surfaces must exist before build
The sprint prompt's four-file task surface is not optional for phase-7 work. If the canonical plan defers
`task-ux-NNN-plan-review.md`, `task-ux-NNN-impl-review.md`, or `task-ux-NNN-research.md` to implementation time, the
required Critiquer approval can block on process compliance even when the implementation plan itself is technically
sound. Create those files before marking planning approved.

## 2. Visual-proof tasks need a named Storybook owner, not just "Storybook coverage"
For presentation-heavy CATs, the canonical task plan needs the exact Storybook file that owns proof for the changed
surface. Generic "add a Storybook story" wording was not enough for the small-model handoff bar; pinning the exact
owner file (`MithrilPartialSyncOverlay.stories.tsx`, `MithrilPartialSyncDialogue.stories.tsx`,
`Diagnostics.stories.tsx`) removed that ambiguity.

## 3. Portaled dialog stories can need a theme-key remount to make visual proof trustworthy
The direct `MithrilPartialSyncConfirmation` Storybook stories render through a portal, so simply switching the
decorator theme can leave the mounted dialog carrying stale theme state. Re-keying the direct confirmation story on the
active theme (`key={props.currentTheme}`) makes the light/dark visual proof reliable for dialog-only CAT-F checks.