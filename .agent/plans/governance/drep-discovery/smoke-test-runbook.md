# DRep discovery: live smoke test

What automated checks cannot tell you, and how to find out in an hour.

Everything here needs a running node and wallet. The Jest suite covers logic and
rendering against fixtures; it cannot tell you whether cardano-wallet returns
what we think it returns, whether an anchor on the live internet verifies, or
whether the page looks right.

---

## Setup

```bash
cd /home/adam/daedalus
yarn nix:preprod        # or yarn nix:mainnet
yarn dev
```

The preprod chain under `~/.local/share/Daedalus/preprod/chain` needs to catch
up before governance data is meaningful. Wait for the sync indicator to reach
100% rather than starting while it climbs: several of the states below are
deliberately different during sync and would be misread as faults.

A wallet with a delegated voting power is needed for the voting centre checks.
If none exists, delegate one first using the flow in section 4.

---

## 1. Voting Center

- [ ] The page has **no title of its own**. The tab says "Voting Center" and
      nothing repeats it below.
- [ ] Tabs sit flush against the dark top bar, with no seam or gap.
- [ ] Each wallet row shows its delegation target: a DRep name and id, or
      Abstain, or No Confidence, or "Not delegated".
- [ ] The status column shows **one** badge, never two. Green active, yellow
      expiring soon, grey inactive. An expiring DRep must not also read active;
      an inactive one must not read expiring.
- [ ] A wallet delegated to a DRep that has since **retired** shows "Loading
      DRep…" indefinitely. This is the known gap recorded in
      `.agent/findings/retired-drep-visibility.md`. Confirm it still behaves
      this way rather than having become something worse.

## 2. DRep Directory

- [ ] Twenty suggestions by default, in cards, not a table.
- [ ] Every suggestion is **active** and carries a name. None shows an
      inactive badge.
- [ ] "Show different suggestions" returns a **different** set, not the same
      twenty. Press it several times.
- [ ] The criteria panel changes the pool: turning off "verified metadata"
      should visibly admit DReps without names.
- [ ] Ask for a cohort of 50. On preprod the pool may be too small, in which
      case the relaxation notice must name which criteria were dropped.
- [ ] **Show all DReps** widens the list and it **scrolls**. Confirm more than
      one screen of cards loads as you scroll: the windowing reads its scroll
      container from the layout, and a regression there stops the list at its
      first screen.
- [ ] In show-all with the default sort, the order is banded: healthy DReps
      first, then expiring soon, then concentrated, then those without
      metadata, and **inactive last of all**. Scroll to the bottom to confirm
      the inactive ones are there.
- [ ] The filter strip is one bordered strip at the same height as the search
      field, not loose controls. The pool toggle is a switch.
- [ ] Table view shows the same DReps with the columns aligned, and scrolls
      horizontally rather than past the right edge when the window is narrow.
- [ ] Search by name and by id prefix. Results come from the **whole** list,
      not the twenty on screen: search for a DRep you know is not in the
      cohort. A skeleton, not "no results", while the full list loads.
- [ ] Abstain and No Confidence remain available while searching and while
      showing all.

## 3. DRep detail

Open several DReps, preferring ones with rich metadata.

- [ ] The DRep's own name is the page heading. No generic "DRep detail".
- [ ] Delegate is the only filled button; View details and the predefined
      options are flat.
- [ ] Voting power shows a figure and a share badge with an info control.
      Hovering or focusing it explains the share against the **total**
      delegated to DReps, not the DRep's own amount again.
- [ ] Two blocks, in order: canonical metadata fields, then additional. The
      references belong to the **canonical** block and must appear above the
      additional heading.
- [ ] Within each block, single values come before lists.
- [ ] Every link carries the external-link icon. Nothing outside references is
      clickable, even when its value is plainly a URL.
- [ ] "View voting history on Cardano Explorer" opens the right network's
      explorer at the right DRep.
- [ ] Copy buttons for the DRep id, the anchor hash and the payment address
      each copy the full value.
- [ ] If a DRep publishes a payment address for another network, a warning says
      so. Hard to find in the wild: worth constructing on preprod.

### Anchor states

Find one of each if you can; the storybook covers all four if you cannot.

- [ ] Verified with content: fields render under their labels.
- [ ] Verified but empty: "Nothing published", neutral, not a warning.
- [ ] Not verified: "Unverified" warning explaining the DRep registered an
      anchor and hash, that no matching document came back, and that the link
      is the DRep's own.
- [ ] An `ipfs://` anchor: our fetcher refuses the scheme, so this depends
      entirely on what the wallet made of it.

### The cache

- [ ] Open a DRep with metadata, go back, open it again. The second view should
      be immediate. Confirm a file appeared under
      `~/.local/share/Daedalus/preprod/DRep-anchor-cache/`.
- [ ] Restart Daedalus and open the same DRep. Still immediate: the cache is on
      disk and keyed by the anchor hash.

## 4. Delegation, end to end

The original PR items. Both need a wallet with funds.

- [ ] Delegate voting power to a DRep from the directory. The confirmation
      names the DRep, the transaction submits, and the voting centre shows the
      new target once it settles.
- [ ] Delegate to Abstain, then to No Confidence. Each records and displays.
- [ ] **Item 9**: a wallet already delegated to a stake pool must keep that
      delegation after a DRep vote delegation. Check the stake pool in the
      Delegation Center before and after. This was display-only when last
      checked; confirm it stayed that way.
- [ ] **Item 8**: change an existing DRep delegation to a different DRep. The
      pending state shows, then resolves to the new target.

## 5. Console

- [ ] With the developer tools open, walk the directory, the detail view and a
      delegation. No governance errors or unhandled rejections.
- [ ] No DRep id, anchor URL or payment address appears in the log output. The
      sanitisation floor forbids all three.

---

## What automation already covers

Do not spend live time re-checking these; they have tests.

- Cohort criteria, the seeded draw, reroll distinctness, relaxation order
- The banded ordering and its randomisation within bands
- Every anchor state and the three-way distinction between them
- Hostile metadata: markup in prose and labels, `javascript:`, `data:`,
  `file:` and scheme-relative URIs, bidi overrides
- Address network mismatch in both directions
- The additional-fields policy: nesting, depth bound, data URIs, withheld keys
