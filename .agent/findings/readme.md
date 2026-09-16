# Findings

Problems noticed during other work, written down rather than fixed on the spot.

A finding belongs here when it is real, out of scope for whatever turned it up,
and would otherwise be lost when that thread ends. Each one states what is true
today, why it matters, and what a fix would have to decide. None of them is a
plan: a finding that gets picked up should grow a plan in `../plans/`.

Findings describing an exploit surface in shipped code do not belong here. This
repository is public, so a write-up of an unfixed weakness would be published
more durably than an issue and could not be withdrawn. Those are raised in the
private management repository instead, and are deliberately absent from this
index: a pointer saying where to look is still a signal that something is worth
looking for.

| Finding | Scope | Status |
|---------|-------|--------|
| [retired-drep-visibility.md](./retired-drep-visibility.md) | Governance, with an upstream fix: a retired DRep leaves the wallet loading forever | Open, not scheduled |
