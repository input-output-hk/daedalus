# task-ux-704 — CAT-G per-section plan: Repo hygiene (G-1)

> Per-CAT implementation doc for task-ux-704 (finding F1, CONFIRMED). **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent:
> `task-ux-704.md`. If this doc ever disagrees with the live repo, prefer the live repo,
> re-run the verification commands quoted below, and reconcile here. All facts below were
> verified against HEAD on 2026-07-04.

## Sequencing position

CAT-G is **position-independent** — it touches no file any other CAT touches (repo root +
`.gitignore` only). Letter order (after CAT-F, before CAT-H) is fine; running it earlier is
also safe.

## Conventions (wave-wide, restated as binding here)

- **One commit for all of CAT-G.** Conventional Commits, subject line only — no body, no
  trailer. Suggested subject:
  `chore(repo): remove agent tooling debris from repo root`
- **Node v24 prep** (typed-scss-modules regen + jest `identity-obj-proxy` sidecar) applies only
  if you choose to run the tsc/jest gates — CAT-G deletes no code that any source, test, CI,
  or build file references (proven in Step 1), so those gates cannot regress from this CAT.
- **Do not reformat unrelated hunks** (prettier 2.1.2 drift — classify against
  `git show HEAD:<f> | prettier --stdin-filepath <f>` before blaming an edit). CAT-G touches no
  prettier-covered file.
- **Rollback:** revert the CAT-G commit (`git revert <sha>`) — it restores all three files and
  removes the ignore entries in one step.

## i18n

None.

---

# 1. G-1 — Remove agent-tooling debris from the repo root

**Defect:** three agent-tooling files sit tracked at the repo root. All three are ABSENT at the
diff base `48b557a02` — this branch is what would introduce them to `develop` via the PR:

| File | Size | What it is |
| --- | --- | --- |
| `ralph.sh` | 262 lines, executable | agent-loop driver script for the `opencode` CLI |
| `opencode.jsonc` | 60 lines | local config for that tool |
| `review_output.json` | 35 lines | one-off machine-written review artifact (`"taskId": "review-gemini-sync-bypass"`) |

`.gitignore` has no entries for any of them, so regenerated local copies would silently
re-track.

### Provenance (recorded so the deletion is traceable)

On this branch (ancestors of HEAD, verified with `git merge-base --is-ancestor`):

- `02b6528f3` `docs(mithril): add partial sync planning workspace` — introduced `ralph.sh` and
  `opencode.jsonc`.
- `1c4196b40` `fix(mithril): resolve partial sync CI failures` — modified `ralph.sh`.
- `4a486e5a2` `fix(mithril): add recovery fallback and data found notices` — introduced
  `review_output.json`.

Pre-rebase duplicates exist on non-HEAD refs only (visible with `git log --all`):
`a6f67e45f` / `c10119e31` (same subjects as the pair above), `181a0b59e`
`fix(agentic): stop ralph when opencode needs feedback`, and `9e1cf215a`
`chore(ralph): add iterative task runner script`. They are NOT ancestors of HEAD; listed here
for completeness only.

**Version-worthy content check (done, nothing to migrate):** anything worth keeping from these
files belongs under `.agent/` — the process history they represent is already captured there
(`.agent/plans/mithril-partial-sync/…`, `.agent/SOPs/…` narrate the ralph/opencode workflow,
and the review findings in `review_output.json` were superseded by the task-plan review docs).
No content is copied anywhere as part of this CAT.

### Step 1.1 — Re-verify nothing references the files

Run (from the repo root):

```bash
git grep -nE "ralph\.sh|opencode|review_output" -- . ':!ralph.sh' ':!opencode.jsonc' ':!review_output.json'
```

Expected result: hits (if any) ONLY under `.agent/` documentation — historical narrative,
not live path references — keep them. Do not pin an exact hit set: `git grep` searches
tracked files only, so the set varies with which of the phase-7 plan docs (e.g.
`task-ux-704.md`) are committed at the time of the run.
Nothing in `source/`, `tests/`, `storybook/`, `nix/`, `installers/`, `package.json`, or CI
references any of the three files. **If the re-run shows any hit outside `.agent/`, stop and
escalate (E1).** Record the re-run result in the impl review.

### Step 1.2 — Delete the files

```bash
git rm ralph.sh opencode.jsonc review_output.json
```

(`git rm` stages the deletions and handles `ralph.sh`'s executable bit.)

### Step 1.3 — Add root-anchored `.gitignore` entries

Append to the END of `.gitignore` (after the existing `*.scss.d.ts` line under `# Typescript`):

```gitignore

# Agent tooling artifacts (local only — never version; see .agent/ for anything worth keeping)
/ralph.sh
/opencode.jsonc
/review_output.json
```

Literal names, root-anchored (leading `/`) so only repo-root copies are ignored.

**Local-drift guard (important):** as of 2026-07-04 the working tree carries an UNCOMMITTED
`.gitignore` edit (`+.devcontainer`, part of the local dev-env sidecar). That drift must NOT
ride the CAT-G commit. Deterministic, non-interactive procedure:

1. `git diff .gitignore` — note every pre-existing unstaged line (expected: only
   `.devcontainer`; adapt if the drift differs, but never commit it).
2. Temporarily delete those drift lines from the working-tree file.
3. Append the three-entry block above; `git add .gitignore`.
4. Commit (together with the Step 1.2 deletions).
5. Re-insert the drift lines into the working-tree file and leave them unstaged.

If `git diff .gitignore` is already clean at implementation time, skip 2 and 5.

### Step 1.4 — Commit

One commit containing exactly: the three deletions + the `.gitignore` addition. Subject as in
Conventions. No body, no trailer.

---

## Verification

```bash
git status --short            # only the intended 4 paths staged pre-commit; drift line unstaged
git check-ignore -v ralph.sh opencode.jsonc review_output.json
```

`git check-ignore -v` must attribute all three to the new root-anchored `.gitignore` rules
(it evaluates patterns whether or not the files exist). Optionally `touch ralph.sh` and confirm
`git status` stays clean, then delete the touched file.

Wave-standard gates (`yarn compile`, `yarn lint`, scoped jest) cannot be affected by this CAT —
run them only if bundling CAT-G's verification with a neighboring CAT's; apply the Node v24
prep first if so.

## Files touched

- `ralph.sh` — deleted
- `opencode.jsonc` — deleted
- `review_output.json` — deleted
- `.gitignore` — three root-anchored entries appended (pre-existing local drift excluded from
  the commit)

Nothing else. No source, spec, i18n, or docs changes.

## Out of scope

- The `.agent/` documentation mentions of ralph/opencode (historical narrative — keep).
- The uncommitted `.devcontainer` `.gitignore` line and any other local-env sidecar state
  (stays local and unstaged).
- Any other repo-root hygiene (e.g. upstream ride-alongs) — the master doc classifies those as
  PR-author process items, not code tasks.

## Acceptance checks

- **G-1:** the three files are gone from the index and working tree; `git check-ignore -v`
  matches all three names against root-anchored rules; the Step 1.1 grep still shows no
  non-`.agent/` references; `git show --stat HEAD` for the CAT-G commit lists exactly 4 paths;
  the diff against base `48b557a02` no longer introduces any of the three files.

## Escalations

- **E1 (reference gate):** any Step 1.1 hit outside `.agent/` (source, tests, CI, package
  scripts) means something started depending on these files after 2026-07-04 — stop, do not
  delete, report the referencing path.
- **E2 (drift shape):** if the unstaged `.gitignore` drift at implementation time is more than
  the `.devcontainer` line, still follow the Step 1.3 procedure (exclude ALL drift from the
  commit); if drift and the new entries ever conflict textually, escalate rather than resolve
  ad hoc.
