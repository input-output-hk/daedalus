Planner: Iteration 1
Timestamp: 2026-09-15T15:20:00Z

The first plan had the header call `requestAssetImage` directly, hold the bytes
in component state, and build the `data:` URL in the render.

Critique:

- **`hasImage` does not reach a component, and the plan assumed it did.** The
  task's implementation note says a row without it never asks, which cannot be
  written at all until the field is on the merged row. `_assetFor` builds the
  domain object field by field and does not include it. That is four edits the
  task's `targetPaths` do not name, and they are the difference between the note
  being implementable and not.
- **Component state does not survive a scroll.** A memo per component satisfies
  "at most one request per subject" only while the component stays mounted, and
  the acceptance criterion is written about scrolling, which is exactly when it
  unmounts. The memo has to outlive the row, so it belongs in the module that
  owns the channel.
- **The bytes should not reach the component.** Encoding in the render re-encodes
  on every paint, and the component has no reason to know the transport carries
  bytes. The client hands back a string or null.
- **"No placeholder" needs to be checkable.** The first plan asserted the image
  was present in one case and said nothing about the others. Two negative cases
  asserting no image element at all are what make the claim testable.
- **The flip from false to true was not covered.** A row rendered before its
  subject resolves has `hasImage` false; an effect keyed on the subject alone
  never asks again once the row resolves. The case is named so the dependency
  list cannot quietly lose it.

What changed in response: findings 2 to 6 added with their evidence; the memo
moved into `source/renderer/app/ipc/assetMetadataChannel.ts`, which is one of the
task's own target paths; `requestAssetImageUrl` added as the one place bytes
become a URL; the verification plan given the two negative header cases, the
flip case and the wire-level assertion for criterion 2; the memo's lack of
expiry named in Risks rather than left unsaid.

One thing deliberately not done: no test-only reset for the memo. The suite uses
a distinct subject per case instead, which is the honest way to test module state
and does not add an export that exists only for tests.

Scope guard: one surface. No logo in the send form, the transaction list or the
settings dialog, and no change to what the main process stores or refuses.

Outcome: approved
