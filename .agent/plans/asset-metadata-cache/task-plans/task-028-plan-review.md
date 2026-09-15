Planner: Iteration 1
Timestamp: 2026-09-15T19:55:00Z

The first plan did what the task asked: fix the two mislabelled hops in the
diagram, mention that the real mechanism derives three names from one base, and
leave everything else.

Critique:

- **The two named claims are not the only wrong ones, and a plan that fixes only
  what it was told about leaves a document that looks corrected.** Reading it
  against the code turned up eleven more, and two of them matter more than the
  diagram does, because a reader copies from them.
- **The generic parameter order is reversed between the two sides and the
  document has it the same on both.** `IpcChannel<Incoming, Outgoing>` is written
  from the declaring side's point of view, so the main handler declares
  `<RendererRequest, MainResponse>` and the renderer client declares
  `<MainResponse, RendererRequest>`. Copying Step 4 as written produces a client
  whose types are inside out.
- **Nothing in the document says `send` and `request` must be paired with
  `onReceive` and `onRequest` respectively.** They post on different names, each
  has exactly one matching handler, and mixing them sends to a name with no
  listener. Nothing reports it: the message goes out and the promise never
  settles. That is the failure a reader is most likely to hit and the document
  was silent on it.
- **Seven of the channels the document tabulates do not exist**, the whole
  Cardano table. Twenty-four other names in those tables do. A table with a
  fictional third in it is worse than no table.
- **The broadcast section describes a mechanism nobody uses.**
  `webContents.send(` appears nowhere in `source/`, and the bare channel constant
  is never a wire name, so the snippet sends on a name nothing listens to.
- **The preload is drawn as a hop and is not one.** It assigns `ipcRenderer` onto
  the renderer's `global` so the channel can default to it, and forwards nothing.
- **Two snippets cannot work at all**: `ipcMain.on('*', ...)`, because Electron
  has no wildcard channel, and a test mock of `ipcRenderer.invoke`, which nothing
  calls.
- **Registration is in `source/main/ipc/index.ts`**, not in `source/main/index.ts`
  as Step 3 says.

What changed in response: findings 1 to 13, each with the file and line it was
read from; the scope widened from the diagram and the Quick Reference to every
claim the document makes; acceptance criterion 2 added, that every tabulated
channel exists, checkable by script; acceptance criterion 3 added, that the
samples compile in the shape the repository uses; an editing pass added before
the commit, because this is a page an outside reader will read.

Scope guard: no code change. Where the document and the code disagree, the
document is wrong, with one exception that is a real deficiency in the code:
response correlation. That is already written up as a finding, is still open, and
this document links it rather than restating it or proposing a fix.

Outcome: approved
