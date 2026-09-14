## Task ID and Title

`task-021` — One-time notice, leading with the send input rather than the
balance.

## Why Chosen Now

It is the last piece of the phase, and it depends on the two it sits beside:
`task-039`'s label is the durable half of this mitigation and `task-040`'s
snapshot is what makes a mid-edit resolution safe. The banner is the part that
speaks to habit, once, at the moment the habit stops being right.

## Interaction Mode

`agent_execution`.

## Scope

A dismissible banner on the token list, once per user, on the first run after the
update, for a profile that holds tokens.

## Non-Goals

- No modal. This is information, not a decision.
- Not per asset, not per wallet. Once per user.
- No change to the amount field, the label or the snapshot.

## Dependencies

`task-003`, `task-019`.

## Research Consulted

- `asset-metadata-cache-prd.md:1178-1199` for the risk being mitigated and for
  what the copy must lead with: the habit, not the number. Before the migration a
  user sends one and a half of a six-decimal token by typing `1500000`. After it,
  the same user types `1.5`. Typing `1500000` afterward sends a million and a
  half tokens, gated only by their balance.
- `asset-metadata-cache-prd.md:1143-1156` for how many tokens this affects: 850
  of 7,977 registry subjects, 10.7 percent, publish nonzero decimals that verify.
- The task's own notes for the three things the copy says and nothing else.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`.

## Live Repo Findings Verified For Planning

1. **The per-profile flag convention is one key and three methods.**
   `api/utils/localStorage.ts:334-339` is the closest match:
   `getStakingInfoWasOpen`, `setStakingInfoWasOpen` and its unset, over one
   `STORAGE_KEYS` entry and one member of the `StorageKey` union.
2. **`WalletTokens` is the token list and already takes the wallet.**
   `WalletTokens.tsx:31-43` has `wallet: Wallet` among its props, and
   `WalletTokensPage.tsx:45` derives its rows from `activeWallet.assets.total`.
   So the holdings check reads the wallet object and not the cache, which is what
   the task asks for.
3. **`ProfileStore` holds the other per-profile flags** and already reads the
   terms-of-use acceptance at startup: `_getTermsOfUseAcceptance` at `:429-431`,
   with `areTermsOfUseAccepted` at `:316-320`.
4. **A profile created after the update can be told apart from one that existed
   before it, by the terms of use.** An existing profile accepted them at some
   earlier version; a profile being created now has not accepted them at the
   moment the application starts. There is no other per-profile marker of age:
   `APP_UPDATE_COMPLETED` is written only by the in-application updater, so it is
   absent for anyone who installed the new version directly.
5. **Nothing collides.** `grep -rn "DECIMAL_PLACES" source` returns nothing.

## Files Expected To Change

- `source/common/config/electron-store.config.ts` and
  `source/common/types/electron-store.types.ts` — one key.
- `source/renderer/app/api/utils/localStorage.ts` — three methods.
- `source/renderer/app/stores/ProfileStore.ts` — the flag, its read, the
  fresh-profile rule and the dismissal.
- `source/renderer/app/actions/profile-actions.ts` — one action.
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.tsx`
  and its stylesheet — the banner.
- `source/renderer/app/components/wallet/tokens/wallet-tokens/WalletTokens.spec.tsx`
  — new.
- `source/renderer/app/containers/wallet/WalletTokensPage.tsx` — two props.
- the four translation artifacts, regenerated.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

1. **One flag, read once, in one await chain.** At startup the store reads the
   flag and the terms-of-use acceptance together. If the flag is set, the banner
   never shows. If it is not set and the terms have not been accepted, the
   profile is being created now, so the flag is written and the banner never
   shows. Otherwise the banner shows until it is dismissed.

   A single chain rather than a reaction over two requests, because a reaction
   would have to decide what to do while one of the two was still loading, and
   the answer would depend on which resolved first.

2. **It starts hidden.** The observable is `true`, meaning acknowledged, until
   the read says otherwise. A banner that flashes on every start while a storage
   read is in flight is worse than one that appears a moment late.

3. **The holdings check is on the wallet.** `assets.length` on the list the page
   derives from `wallet.assets.total`, so a profile with no tokens never sees it
   whatever the cache holds.

4. **Three sentences and nothing else**: what changed about entering an amount,
   what changed about displayed balances, and that the per-token setting still
   overrides both.

## Acceptance Criteria

1. Shown once, then never again after dismissal.
2. Not shown to a profile holding no tokens.
3. Survives a restart without reappearing.
4. The amount field states the unit it is accepting whether the banner is present
   or dismissed.
5. The banner does not appear on a fresh profile created after the update.
6. `compile`, `lint`, `i18n`, `stylelint` and `jest` green from `nix build`.
7. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The component spec drives the banner: present for a profile that holds tokens
  and has not acknowledged it, absent after the callback fires, absent for a
  profile holding no tokens, and absent for one that has acknowledged it.
- The dismissal is asserted as the callback the container wires to the store
  action, not as local component state, because criterion 3 is about what
  survives a restart and local state does not.
- The store cases drive criterion 3 and criterion 5 as the three combinations
  that matter: flag set, flag unset with terms accepted, flag unset with terms
  not accepted. The third writes the flag without showing anything, which is what
  criterion 5 means.
- Criterion 4 needs no new case and is asserted by construction: `AssetInput` has
  no access to the banner's state, takes no prop related to it, and its label
  cases in `AssetInput.spec.tsx` render with no banner in the tree at all. Stated
  rather than tested twice.
- All five Nix checks.

## Risks and Open Questions

- **The fresh-profile rule leans on the terms of use.** If a profile somehow
  reaches the token list without ever having accepted them, it would be marked as
  new and never shown the banner. That is the safe direction: the cost of not
  showing it to someone who has no habit is nothing.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-021.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-021-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-021-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A profile that held tokens before the update is told once, on the token list,
what changed about entering an amount.

## Final Outcome

Complete.

## Self-Review

The banner is the weakest of the three mitigations and the plan should say so:
it is dismissible, it is shown once, and a user who clicks it away without
reading it is left with the label and the snapshot. That ordering is deliberate
and it is why this task is last rather than first.
