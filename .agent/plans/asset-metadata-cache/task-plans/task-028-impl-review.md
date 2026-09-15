Implementation: Iteration 1
Timestamp: 2026-09-15T20:10:00Z

Changes made, documentation only:
- `.agent/workflows/ipc.md`: Overview, architecture diagram, Quick Reference, all
  five steps, the channel types, the Cardano table, the debugging section and the
  testing section.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`:
  `task-028.status` to `completed`, and its implementation notes rewritten to
  name what the correction found.

Files touched:
- `.agent/workflows/ipc.md`
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

`git diff --name-only` names nothing outside `.agent/`. No code changed.

What the document now says that it did not:

- The three wire names, and that nothing is ever sent on the bare constant.
- That `send` pairs with `onReceive` and `request` with `onRequest`, and that
  mixing them sends to a name with no listener and hangs silently. This is the
  failure a reader is most likely to cause and the document was silent on it.
- That the generic parameters are listed in the opposite order on the two sides.
- That the preload assigns `ipcRenderer` onto the renderer's `global` rather than
  forwarding anything.
- That a channel name is a singleton and a second instance throws at startup,
  including what that means under Jest.
- That responses are not correlated, with a pointer to the finding rather than a
  restatement of it.
- Which two bare names bypass the mechanism, and that one of them has two
  handlers.

What it no longer says: that `ipcRenderer.invoke` or `ipcMain.handle` are
involved; that the preload is a hop; that seven Cardano channels exist; that a
broadcast is `webContents.send` on the bare constant; that `ipcMain.on('*')` sees
every message.

Verification run:

- Criterion 2 by script: every `` `NAME_CHANNEL` `` in the document matched
  against the constants declared in `source/common/ipc/api.ts`. Thirty-eight
  names mentioned, and the only one not declared is `MY_NEW_CHANNEL`, which is
  the tutorial's own example.
- Criterion 3 against `governanceAnchorChannel.ts:11-14` and
  `governanceChannel.ts:8-10`, which is also the pair the document now tells a
  reader to copy.
- Criterion 4 by `git diff --name-only`: nothing outside `.agent/`.
- The prose-marker sweep: no em dash, no emoji, and no arrow outside the box
  diagram and the fat arrows in the TypeScript samples. The `---` separators
  between top-level sections were the document's style before this rewrite and
  are kept.
- `nix fmt` reported no change, which is expected: `perSystem/formatter.nix:48`
  excludes `.agent` from treefmt.
- The seven checks were run anyway as a regression guard, all exit 0. Nothing
  outside `.agent/` changed, so this says only that the tree is still green.

Two corrections from the editing pass, both to claims this plan had recorded as
verified:

**Who calls the handler registration.** The plan and the first draft of the
document both said `source/main/index.ts` calls the default export of
`source/main/ipc/index.ts`. It does not. `source/main/windows/main.ts:4` imports
it and `:72` calls it, after the window is constructed, because several handlers
need the window. The claim had been checked by reading `source/main/ipc/index.ts`
and not by following who calls it.

**How many call sites bypass the mechanism.** The plan said two, counting the two
`ipcRenderer.send` sites. There are two bare names but three handlers:
`source/main/windows/main.ts:74` and `:79` register `resize-window` and
`close-window` inline, and `source/main/ipc/resize-window.ts:5` registers a
second, identical `resize-window` handler, wired in through
`source/main/ipc/index.ts:37`. Both guard on the sender and both call
`window.setSize`, so a resize is applied twice. The document now records it,
because it is exactly the class of mistake the channel mechanism prevents:
`IpcChannel` throws on a second instance of the same name.

Deviations from the approved plan:
- The two corrections above, which widen the plan's finding 7 and add a
  fourteenth finding.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T20:16:00Z

Acceptance criteria, each against the evidence:

1. *The diagram names the mechanism the repository actually uses.* Met. Two
   boxes rather than three, the three derived names on the arrows, and the
   preload described as what it is.

2. *Every tabulated channel exists.* Met, by script rather than by reading.

3. *Every code sample matches the shape the repository uses.* Met, including the
   generic parameter order, which the old document had the same on both sides.

4. *No code change.* Met.

5. *An editing pass before the commit.* Met, and it is the part of this record
   worth keeping. It applied two small language fixes and found two technical
   errors that the planning sweep had written down as verified. Both were of the
   same kind: a claim checked by reading the file it is about rather than by
   following the call chain into it. A document whose whole purpose is to be
   trustworthy about the code cannot be checked that way, and the two are
   corrected in the plan as well as in the document.

The judgement worth naming is the duplicate `resize-window` handler. It could
have been left out, since it is about two bare-name handlers and not about
channels. It is in, because the document's own argument for the channel
mechanism is that it makes this impossible, and a live example of the mistake
the mechanism prevents is the strongest form that argument can take. It is
recorded rather than fixed: this task changes no code.

Summary: The document described a mechanism the repository does not use, in a
diagram, in five worked steps, in a broadcast recipe, in a debugging snippet, in
a test mock, and in a table of seven channels that do not exist. It now describes
the one that is there, with the file and line behind each claim, so the next
person to doubt it can check it in one command.

Decision: approved
