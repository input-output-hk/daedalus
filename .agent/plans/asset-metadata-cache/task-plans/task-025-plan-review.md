Planner: Iteration 1
Timestamp: 2026-09-15T16:40:00Z

The first plan was to run `yarn i18n:manage`, confirm the artifacts did not move,
confirm each new message has a description, and stop.

Critique:

- **That is not a pass, it is a check that already runs.** `perSystem/checks.nix:59-75`
  snapshots the artifacts, regenerates and fails on any difference, and it has
  been green on every commit of this branch. A task whose whole output is
  re-running a green check has found nothing and asserted nothing.
- **"Every new message has a description" needs a denominator.** Checking eleven
  by eye says nothing about whether eleven is the exception. Run over all 1,640
  extracted messages it says something: one message lacks a description and
  eleven lack the `!!!` marker, and none of the twelve is this branch's.
- **Nothing checked whether the messages are read.** A `defineMessages` entry
  with no reader extracts cleanly, sits in all four artifacts, and keeps the
  check green forever. The scan is worth writing, and its result — 67 candidates,
  all pre-existing, most of them dynamic lookups, none of them this branch's — is
  worth recording.
- **Nothing checked the messages against the values they interpolate.** Four of
  the eleven put a count in front of "decimal places" with no plural form, so
  they render "1 decimal places". The value is reachable in all four: the
  registry publishes a count of one, and the settings dialog offers one in its
  own select. This is the defect a pass exists to find, and ICU plural is already
  used eleven times in this file, so the fix has precedent rather than being an
  invention.
- **The plan was about to fix the two pre-existing siblings as well, and must
  not.** `assets.warning.available` and `assets.warning.notUsing` have Japanese
  translations. Changing an English default leaves the translation for that id in
  place and stale, and the manager cannot detect it, so the safe rule is that a
  translated message is not edited by a pass like this one. The resulting
  inconsistency goes in Risks.

What changed in response: findings 2, 3, 4, 6, 7, 8 and 9 added, each with the
measurement behind it; the four plural corrections added to Scope; a Non-Goal
added forbidding edits to translated messages, with the reason; the risk of the
resulting inconsistency named; the verification plan given the singular cases and
the existing plural cases as their complement.

One thing checked and deliberately not acted on: the straight apostrophe. ICU
treats `'` as the start of a quoted section only when what follows is `{`, `}`,
`#` or another `'`. Four messages in `en-US.json` carry both an apostrophe and a
placeholder, and in all four the apostrophe is followed by a letter, so none of
them is misrendering. Checked rather than assumed, and left alone.

Scope guard: eleven messages, four of them corrected, no new message, no new
surface, no Japanese, and no edit to anything already translated.

Outcome: approved
