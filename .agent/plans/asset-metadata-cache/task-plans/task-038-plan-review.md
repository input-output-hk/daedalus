Planner: Iteration 1
Timestamp: 2026-09-16T21:55:00Z

Drafted from the task's entry in the graph against `task-027`'s procedure, which
is the format an operator here already has.

Critique of Iteration 1:

1. *The first draft had six scenarios, the six the task graph names.* Two are
   missing from that list and both matter. A freshly minted NFT is the only check
   of the volatile window's user-visible consequence, which the PRD's own open
   question 1 accepts as the usual path for NFTs rather than the exception. And
   selfnode has no instance at all, so the settings page has to render, issue
   nothing and say nothing about it. Nine scenarios and one more.

2. *"Point the setting at an instance that returns a bad pointer" had no way to
   do it.* No public instance will lie on request. The scenario names a local
   stand-in and says exactly what it must answer, which turns the hardest
   scenario from a hope into a scripted step. The same for the stale-tip case,
   because no public instance is twelve hours behind.

3. *Several pass conditions were things only the author could check.* "Names
   resolve correctly" is not one. Each now names an artifact: a log line, a
   screenshot pair before and after, or a `sqlite3` query with the columns that
   have to hold.

4. *The chain-row query was going to be described.* It is written out, because
   the three columns it checks are the whole of locked decision 10 and an
   operator should not have to compose SQL to check them.

5. *The one thing that was exercised here was going to go unmentioned.* The
   confirmation was run once against the real preprod database on this machine.
   Leaving that out would have someone repeat it; claiming it as a scenario would
   be false. It is recorded as what it is, in the interaction-mode section.

6. *Scenario 9's pass condition had no failure condition.* A missing name is the
   expected state for the first hour, so the criterion says what would make it a
   failure: the name never arriving after the block settles.

Changes made in response: scenarios 9 and 10, the two stand-ins, per-step
artifacts, the written-out query, the recorded live run, and scenario 9's
failure condition.

Scope guard: no source change, no new automation, no phases 1 to 6.

Outcome: approved
