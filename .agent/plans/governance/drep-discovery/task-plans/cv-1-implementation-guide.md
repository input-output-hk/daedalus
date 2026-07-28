# CV-1 Implementation Guide: Current-Vote Plumbing + CurrentVoteSummary Core

> **Phase:** `cv-1` — "Current-vote 1 - Plumbing + CurrentVoteSummary core"
> (riskLevel medium) | **Date:** 2026-07-27 |
> **PRD:** [cv-1-PRD.md](cv-1-PRD.md) |
> **Findings:** [research/cv-1-findings.md](../research/cv-1-findings.md)
>
> All `file:line` anchors below were verified against branch `wt/cv-1` at
> `b900b99b3` (pre-implementation). Line numbers shift as tasks land —
> re-anchor by the quoted content, not the number. Run every command from the
> worktree root.
>
> This document carries the full sections for `task-126` … `task-130`;
> sections for `task-131` … `task-135` are appended below by the second
> authoring pass, and the `task-170` / `task-171` sections by the
> reconciliation pass that added those two rows to cv-1.

## Implementation Order

Execute strictly in this order (in-phase chain per the tasks JSON; cross-phase
deps task-109/task-110 are already verified):

1. **task-126 — Commit cardano-wallet voting/delegating fixtures.** No
   dependencies; the authored fixtures are the ground truth every later spec
   in this slice consumes.
2. **task-127 — Fix the `delegating_and_voting` wire literal.** After 126: the
   corrected literal must exist before any code path or spec consumes the
   fixtures' `"status": "delegating_and_voting"` value.
3. **task-128 — Widen `WalletDelegation`/`WalletNextDelegation` with
   `voting`.** After 127: it extends the sibling types of the just-corrected
   `DelegationStatus` union and defines `WalletVotingTarget`, consumed by
   129/130/131/132.
4. **task-129 — `normalizeDRepIdentity` helper.** After 128: it returns the
   `DRepIdentity` that `WalletVotingTarget`'s `drep` variant carries.
5. **task-130 — Mapper in `_createWalletFromServerData` + collision rules.**
   After 128 and 129: the mapper consumes both the widened types and the
   normalizer, and passes `votingTarget` into the `Wallet` constructor.
6. **task-131 — Wallet domain `votingTarget`/`currentVote`/`isVoting` incl.
   `update()` pick list.** After 130: it promotes the constructor pass-through
   added by 130 to a declared `@observable` plus computeds, and extends the
   `update()` pick list (`Wallet.ts:177-201`) so polls do not drop the value.
7. **task-132 — `CurrentVoteSummary` CORE states, no live badge.** After 131
   (plus verified task-109/task-110): it renders `wallet.currentVote` only;
   the live status badge is cv-2 (task-136).
8. **task-133 — Storybook entry, 4 core knobs.** After 132: the story mounts
   the finished component's four core states via the global EN/JA toggle.
9. **task-134 — Jest: mapper + Wallet computeds + core snapshots.** After 130
   and 131 (consumes the task-126 fixtures and the export added by 130).
10. **task-170 — Redact raw wallet payloads at the AdaApi wallet-list log
    sites.** After 130: it re-logs the same `getWallets` / `getWallet` seam
    task-128 widened and task-130 populates, so it lands once that seam is
    final.
11. **task-171 — Restore the ja-JP `!!!` markers and guard them.** No
    dependencies of its own, but task-135 and cv-2's task-146 both depend on
    it: the guard is the only thing that stops task-135 — and cv-2 /
    anchor-2 copy after it — from minting an unmarked ja-JP value.
12. **task-135 — i18n core keys `voting.governance.currentVote.*`.** After
    132 and 171: `yarn i18n:manage` extracts the component's message
    definitions into both catalogs, keeping the leading `!!!`, and the
    task-171 guard covers the twelve new keys.

## Cross-Cutting Renderer Note

### TypeScript conventions

- The renderer compiles with `"strict": false` and `"noImplicitAny": false`
  (`tsconfig.json:79-80`; `strictNullChecks` is commented out at :81). Do not
  rely on strict-mode narrowing; write explicit `| null` in signatures and
  explicit guards.
- `yarn compile` typechecks the whole tree — `tsconfig.json` has no `include`
  and excludes only `node_modules` (`tsconfig.json:103`) — so new spec files
  under `tests/` must also compile.
- Use `import type { … }` for type-only imports (precedent:
  `source/renderer/app/domains/Wallet.ts:4-12`). Literal-union constant
  objects follow the `WalletDelegationStatuses` pattern (`Wallet.ts:33-43`).
- Test files use the live `.spec.ts(x)` convention (jest `roots:
  ['<rootDir>/tests', '<rootDir>/source']` at `jest.config.js:129`, `testMatch:
  ['**/?(*.)+(spec|test).[tj]s?(x)']` at `jest.config.js:156`). Never use the
  stale `.test.ts` names from the design doc (finding F-3).
- `yarn lint` covers `source storybook utils` only (`package.json:43`) — it
  does not lint `tests/`; new specs are still gated by `tsc`.
- Prettier 2.1.2 formatting: run `node_modules/.bin/prettier --check` (or
  `--write`) ONLY on files created in cv-1. Never reformat pre-existing files
  (the repo carries pre-existing drift); match surrounding style by hand when
  editing them. Never use `yarn prettier` for targeted runs (its glob covers
  the whole tree).
- Code comments only where logic is not self-evident: 1-3 plain lines stating
  the invariant or the why; never task IDs, review labels, ALL-CAPS markers,
  or change history.

### Verification gates and the Node v24 tsc fallback

- `yarn compile` is `tsc --noEmit` (`package.json:45`). Under Node v24.16.0
  `yarn compile` has previously failed for environment reasons. If it fails:
  capture the exact error, then gate on `node_modules/.bin/tsc --noEmit`
  directly. An environment failure is never treated as a code failure without
  checking.
- New `.scss` modules (task-132) need generated type declarations:
  `node_modules/.bin/typed-scss-modules source/renderer/app` (the
  `typedef:sass` script, `package.json:73`).
- Focused jest runs use the precedent form:
  `yarn test:jest <path-to-spec> --runInBand`.
- Whole-tree jest runs use `node_modules/.bin/jest --runInBand` (bare
  `yarn test:jest` is the identical run — `package.json:21` is `"test:jest":
  "jest"` — and is the form used in the Cross-Cutting gate). NEVER append
  `tests/jest` to a whole-tree gate: because the script is bare `jest`, a
  trailing path is a `testPathPattern` regex, and jest's `roots` are BOTH
  `<rootDir>/tests` and `<rootDir>/source` (`jest.config.js:129`) with most
  specs colocated under `source/`. Measured with `jest --listTests`: at cv-1
  close the unfiltered tree is 86 suites and `tests/jest` selects 10 of them
  (82 / 7 was the figure when this guide was authored). Only the unfiltered
  run may be reported as "all suites green".
- `yarn i18n:manage` (`package.json:54`) runs only after task-135 copy
  changes.

### Sanitization floor (invariant 2) as it applies to cv-1

Inlined, binding: no DRep id, no `abstain`/`no_confidence` literal, no
CIP-129/CIP-105 bech32 string in any logger, analytics, or electron-store
payload. Test fixtures and docs MAY contain DRep ids — the floor binds runtime
logging/analytics/store paths only. For cv-1 specifically:

- The ONLY new runtime log in tasks 126-130 is task-130's unknown-HRP warning.
  It logs a bounded, allowlisted HRP token (`hrp` key), never the raw id (see
  task-130 Step 2; design rule: warning "may include HRP only, never the raw
  DRep id", current-vote-display-design.md:164).
- `normalizeDRepIdentity` (task-129) is pure and never logs.
- `WalletVotingTarget` is held in memory only on the MobX `Wallet` instance —
  "not persisted to electron-store, never appears in IPC payloads"
  (current-vote-display-design.md:114).
- `filterLogData` already redacts the `voting` key ("redacts CIP-129 drepId
  nested under delegation.active.voting",
  `tests/jest/security/governance-sanitization.spec.ts:64`), so existing
  wallet-payload debug logs stay safe unchanged.
- The task-111 floor suite must be re-asserted green after task-130 lands and
  again in task-134:
  `yarn test:jest tests/jest/security/governance-sanitization.spec.ts --runInBand`.

---

## task-126: Commit cardano-wallet voting/delegating fixtures

**Files created (all new):**

- `tests/mocks/wallets/wallet-voting-drep.json`
- `tests/mocks/wallets/wallet-delegating-and-voting.json`
- `tests/mocks/wallets/wallet-voting-abstain.json`
- `tests/mocks/wallets/wallet-voting-no-confidence.json`

**Context.** No running cardano-wallet and no network exist in this
devcontainer, so the fixtures are AUTHORED from the pinned cardano-wallet
v2026-05-11 swagger shape (commit
`c642e0779676d2567e3d5fa1e2db9f029b6398e1`;
governance-drep-discovery-plan.md:174/:196) plus the live `ApiWallet`
consumption in `_createWalletFromServerData` (`api.ts:3010-3112`) — decision
D-5. Shape conformance is the binding check, not live capture.

**Locked invariants (inline).** Fixtures MAY contain DRep ids (the
sanitization floor binds runtime logging/analytics/store only). No real wallet
ids — synthetic 40-char repeated-digit hex only. Every bech32 string is
checksum-verified before commit (D-8); never hand-invent one.
`delegation.next` is an ARRAY (D-9). `abstain`/`no_confidence` are form-only
wire sentinels, never DRep ids (invariant 13).

### Canonical bech32 vectors (checksum-verified)

All strings below were decoded with `bech32@2.0.0` on 2026-07-27; payload
lengths and CIP-129 header bytes confirmed. Key-vector provenance:
`research/slice-3-findings.md:116-119`; `drep_vkh` and `pool` synthesized with
the `bech32` library from those credentials (D-8). Both encodings of each pair
share credential bytes, so cross-form assertions in later specs are exact.

| Role | String | Payload |
| --- | --- | --- |
| CIP-129 key DRep | `drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy` | 29 bytes, header `0x22` |
| CIP-105 key DRep (same credential) | `drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l` | 28 bytes |
| key credential hex | `a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c` | 28 bytes |
| CIP-129 script DRep | `drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt` | 29 bytes, header `0x23` |
| CIP-105 script DRep (same credential) | `drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc` | 28 bytes |
| script credential hex | `0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4` | 28 bytes |
| Deprecated 28-byte `drep` HRP form (rejection vector, task-129 spec only) | `drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9` | 28 bytes, no header |
| Synthetic stake pool | `pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2` | 28 bytes (`0x03` repeated) |

### Fixture field set — derived from what the mapper reads

`_createWalletFromServerData` consumes exactly these `AdaWallet` fields
(`api.ts` anchors): `id` (:3014), `address_pool_gap` (:3015), `balance`
`available`/`total` and — for non-legacy — `reward`, each `{ quantity, unit }`
(:3028-3049), `name` (:3017), `assets.available` / `assets.total` arrays
(:3065-3088; empty arrays still exercise the `.map` calls), `passphrase`
(optional, `get(passphrase, 'last_updated_at', null)` at :3027 — omitted),
`delegation.active` `{ status, target, voting }` (:3051-3056; `voting` is
consumed once task-130 lands), `delegation.next` consumed as an ARRAY via
`last(next)` (:3058-3059) even though `AdaWallet` types it singular
(`types.ts:45` — pre-existing mismatch, out of cv-1 scope, D-9), `state`
(:3021), `discovery` (:3023).

Deliberately omitted: `isLegacy` / `isHardwareWallet` (Daedalus-injected at
legacy call sites only, e.g. `api.ts:918`; the wire JSON has neither and the
destructure defaults both to `false` at `api.ts:3022/:3024`), `passphrase`,
`tip`. Nothing decorative.

### Step-by-Step

#### Step 1: Create the directory

```bash
mkdir -p tests/mocks/wallets
```

#### Step 2: `tests/mocks/wallets/wallet-voting-drep.json`

`status=voting` with `delegation.active.voting` = CIP-129 key DRep. Exact file
content (transcribe verbatim):

```json
{
  "id": "1111111111111111111111111111111111111111",
  "address_pool_gap": 20,
  "name": "cv1 fixture voting drep",
  "balance": {
    "available": {
      "quantity": 10000000000,
      "unit": "lovelace"
    },
    "total": {
      "quantity": 10000000000,
      "unit": "lovelace"
    },
    "reward": {
      "quantity": 0,
      "unit": "lovelace"
    }
  },
  "assets": {
    "available": [],
    "total": []
  },
  "delegation": {
    "active": {
      "status": "voting",
      "voting": "drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy"
    },
    "next": []
  },
  "state": {
    "status": "ready"
  },
  "discovery": "sequential"
}
```

Note: no `target` key — a voting-only status never carries a stake-pool
target; the mapper AC "never parses active.target" is asserted against this
fixture in task-134.

#### Step 3: `tests/mocks/wallets/wallet-delegating-and-voting.json`

`status=delegating_and_voting` with BOTH a pool `target` and a CIP-105
`drep_vkh` voting target, plus a non-empty pending `next` array (the "pending"
case of task-131/134's ACs is covered by this fixture, decision D-10). Exact
file content:

```json
{
  "id": "2222222222222222222222222222222222222222",
  "address_pool_gap": 20,
  "name": "cv1 fixture delegating and voting",
  "balance": {
    "available": {
      "quantity": 25000000000,
      "unit": "lovelace"
    },
    "total": {
      "quantity": 25000000000,
      "unit": "lovelace"
    },
    "reward": {
      "quantity": 1500000,
      "unit": "lovelace"
    }
  },
  "assets": {
    "available": [],
    "total": []
  },
  "delegation": {
    "active": {
      "status": "delegating_and_voting",
      "target": "pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2",
      "voting": "drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l"
    },
    "next": [
      {
        "status": "delegating",
        "target": "pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2",
        "changes_at": {
          "epoch_number": 412,
          "epoch_start_time": "2026-08-10T21:44:51Z"
        }
      }
    ]
  },
  "state": {
    "status": "ready"
  },
  "discovery": "sequential"
}
```

#### Step 4: `tests/mocks/wallets/wallet-voting-abstain.json`

`status=voting` with the `abstain` sentinel. Exact file content:

```json
{
  "id": "3333333333333333333333333333333333333333",
  "address_pool_gap": 20,
  "name": "cv1 fixture voting abstain",
  "balance": {
    "available": {
      "quantity": 5000000000,
      "unit": "lovelace"
    },
    "total": {
      "quantity": 5000000000,
      "unit": "lovelace"
    },
    "reward": {
      "quantity": 0,
      "unit": "lovelace"
    }
  },
  "assets": {
    "available": [],
    "total": []
  },
  "delegation": {
    "active": {
      "status": "voting",
      "voting": "abstain"
    },
    "next": []
  },
  "state": {
    "status": "ready"
  },
  "discovery": "sequential"
}
```

#### Step 5: `tests/mocks/wallets/wallet-voting-no-confidence.json`

`status=voting` with the `no_confidence` sentinel. Exact file content:

```json
{
  "id": "4444444444444444444444444444444444444444",
  "address_pool_gap": 20,
  "name": "cv1 fixture voting no confidence",
  "balance": {
    "available": {
      "quantity": 5000000000,
      "unit": "lovelace"
    },
    "total": {
      "quantity": 5000000000,
      "unit": "lovelace"
    },
    "reward": {
      "quantity": 0,
      "unit": "lovelace"
    }
  },
  "assets": {
    "available": [],
    "total": []
  },
  "delegation": {
    "active": {
      "status": "voting",
      "voting": "no_confidence"
    },
    "next": []
  },
  "state": {
    "status": "ready"
  },
  "discovery": "sequential"
}
```

#### Step 6: Verify

All four files parse, ids are synthetic repeated-digit 40-char hex, `next` is
an array, and the mapper-consumed fields are present:

```bash
for f in tests/mocks/wallets/wallet-*.json; do
  node -e "
    const w = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
    if (!/^([0-9a-f])\1{39}\$/.test(w.id)) throw new Error('id must be 40 repeated hex chars: ' + w.id);
    if (!Array.isArray(w.delegation.next)) throw new Error('delegation.next must be an array');
    for (const k of ['id','address_pool_gap','name','balance','assets','delegation','state','discovery'])
      if (!(k in w)) throw new Error('missing field ' + k);
    for (const b of ['available','total','reward'])
      if (typeof w.balance[b].quantity !== 'number' || w.balance[b].unit !== 'lovelace')
        throw new Error('bad balance.' + b);
    console.log('ok', process.argv[1]);
  " "$f"
done
```

Checksum-verify every bech32 string used by the fixtures (D-8 / gotcha 12 —
expected outputs in the comments):

```bash
node -e "const {bech32}=require('bech32');console.log(bech32.decode(process.argv[1],1000).prefix)" "drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy"        # prints: drep
node -e "const {bech32}=require('bech32');console.log(bech32.decode(process.argv[1],1000).prefix)" "drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l"     # prints: drep_vkh
node -e "const {bech32}=require('bech32');console.log(bech32.decode(process.argv[1],1000).prefix)" "pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2"         # prints: pool
node -e "
  const { bech32 } = require('bech32');
  const d = bech32.decode('drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy', 1000);
  const bytes = bech32.fromWords(d.words);
  if (d.prefix !== 'drep' || bytes.length !== 29 || bytes[0] !== 0x22) throw new Error('bad CIP-129 key vector');
  console.log('ok: CIP-129 key vector, 29-byte payload, header 0x22');
"
```

Format check on the four new files only:

```bash
node_modules/.bin/prettier --check "tests/mocks/wallets/*.json"
```

If the check fails, run the same command with `--write` (allowed — these four
files are created by this task) and re-check.

### Acceptance

- [ ] Four fixtures exist under `tests/mocks/wallets/` covering `voting` (DRep)
      / `delegating_and_voting` / `abstain` / `no_confidence` (AC-1).
- [ ] Each conforms to the ApiWallet shape of the pinned v2026-05-11 swagger:
      snake_case wire keys only, no Daedalus-injected `isLegacy` /
      `isHardwareWallet`, `delegation.next` an array (AC-2, D-5, D-9).
- [ ] Each fixture is minimal (only mapper-consumed fields) and contains no
      real wallet ids — ids are `1…1`/`2…2`/`3…3`/`4…4` × 40 (AC-3).
- [ ] All bech32 strings decode cleanly with the one-liners above (D-8).
- [ ] `node_modules/.bin/prettier --check "tests/mocks/wallets/*.json"` passes.

---

## task-127: Fix the `delegating_and_voting` wire literal (constant export name preserved)

**Files touched:**

- `source/renderer/app/api/wallets/types.ts` (edit)
- `source/renderer/app/domains/Wallet.ts` (edit)
- `tests/jest/api/walletDelegationStatuses.spec.ts` (new — creates
  `tests/jest/api/`)

**Context.** The cardano-wallet wire value for dual delegation is
`delegating_and_voting`; the renderer compares against
`'voting_and_delegating'` — a latent bug that misclassifies real
pool-and-DRep wallets. The ONLY two occurrences of the old literal in the
repo are the two lines edited below (verified:
`grep -rn "voting_and_delegating" source/ tests/ storybook/` returns exactly
`types.ts:84` and `Wallet.ts:42`). The constant export name
`VOTING_AND_DELEGATING` is preserved (plan Key Decisions row, reconciled to
task-127 — F-1).

**Locked invariants (inline).** Constant NAME never changes — only its string
value. `Wallet.ts:244-245` (`isDelegating` computed) consumes the constant,
not the literal, so it needs no edit — the behavioral fix flows through
automatically. No other behavioral change.

### Step-by-Step

#### Step 1: Correct the union member in `types.ts`

At `source/renderer/app/api/wallets/types.ts:80-84`, the current code is:

```ts
export type DelegationStatus =
  | 'delegating'
  | 'not_delegating'
  | 'voting'
  | 'voting_and_delegating';
```

Change ONLY the last member so the block reads:

```ts
export type DelegationStatus =
  | 'delegating'
  | 'not_delegating'
  | 'voting'
  | 'delegating_and_voting';
```

#### Step 2: Correct the constant value in `Wallet.ts`

At `source/renderer/app/domains/Wallet.ts:33-43`, the current code is:

```ts
export const WalletDelegationStatuses: {
  DELEGATING: DelegationStatus;
  NOT_DELEGATING: DelegationStatus;
  VOTING: DelegationStatus;
  VOTING_AND_DELEGATING: DelegationStatus;
} = {
  DELEGATING: 'delegating',
  NOT_DELEGATING: 'not_delegating',
  VOTING: 'voting',
  VOTING_AND_DELEGATING: 'voting_and_delegating',
};
```

Change ONLY line 42 so it reads:

```ts
  VOTING_AND_DELEGATING: 'delegating_and_voting',
```

#### Step 3: Prove no stragglers

```bash
grep -rn "voting_and_delegating" source tests storybook || echo "OK: no stale literal"
```

Expected: the `OK` line and no file hits (grep exits non-zero on no match).

#### Step 4: Add the pin spec

Create `tests/jest/api/walletDelegationStatuses.spec.ts` (new directory
`tests/jest/api/` — jest picks it up via `roots` automatically,
`jest.config.js:129`). Exact file content:

```ts
import { WalletDelegationStatuses } from '../../../source/renderer/app/domains/Wallet';

// The wire value for dual delegation is 'delegating_and_voting'; the constant
// export name is intentionally kept unchanged.
describe('WalletDelegationStatuses wire literals', () => {
  it('pins VOTING_AND_DELEGATING to the delegating_and_voting wire literal', () => {
    expect(WalletDelegationStatuses.VOTING_AND_DELEGATING).toBe(
      'delegating_and_voting'
    );
  });

  it('pins the remaining statuses to their wire literals', () => {
    expect(WalletDelegationStatuses.DELEGATING).toBe('delegating');
    expect(WalletDelegationStatuses.NOT_DELEGATING).toBe('not_delegating');
    expect(WalletDelegationStatuses.VOTING).toBe('voting');
  });
});
```

(`domains/Wallet.ts` imports only `lodash`, `mobx`, `bignumber.js`, and types
— safe to import in jest without mocks.)

#### Step 5: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn test:jest tests/jest/api/walletDelegationStatuses.spec.ts --runInBand
yarn test:jest tests/jest --runInBand   # all existing suites stay green
yarn lint
node_modules/.bin/prettier --check tests/jest/api/walletDelegationStatuses.spec.ts
```

### Acceptance

- [ ] `DelegationStatus` union contains `'delegating_and_voting'`, not
      `'voting_and_delegating'` (AC-1).
- [ ] `WalletDelegationStatuses.VOTING_AND_DELEGATING ===
      'delegating_and_voting'` with the constant name preserved (AC-2).
- [ ] The pin spec above passes (AC-3).
- [ ] Step 3 grep finds zero remaining `voting_and_delegating` literals in the
      renderer codebase (AC-4).
- [ ] `tsc` clean; full existing jest run green.

---

## task-128: Widen `WalletDelegation`/`WalletNextDelegation` with `voting` field

**Files touched:**

- `source/renderer/app/api/wallets/types.ts` (edit — types only, no runtime
  code)

**Context.** Defines `WalletVotingTarget` (does not exist anywhere yet) and
adds the optional `voting` field to both delegation types. `DRepIdentity`
ALREADY exists at `source/common/types/governance.types.ts:20-31` with exactly
the required shape (`raw` required, `cip129?`, `cip105?`, `credentialHex?`,
`credentialType: 'key' | 'script'`) — decision D-6: import it, NEVER redefine
it.

**Locked invariants (inline).** `abstain` / `no_confidence` are form-only
sentinels, never DRep directory entries (invariant 13) — they are
discriminated variants without a `drep` payload. `givenName` / `anchorUrl` are
NOT on `DRepIdentity` (anchor-derived display is owned by the drepIndex,
current-vote-display-design.md:99). Type-vs-wire note, locked: on the wire,
`delegation.active.voting` is a raw string (`abstain` / `no_confidence` /
bech32 DRep id); the widened type expresses the parsed contract demanded
verbatim by the task's ACs, and the only consumer (task-130's `parseVoting`)
reads the raw value through untyped `lodash.get` and treats its input as
`unknown`. This mirrors the pre-existing `next` singular-vs-array looseness
(D-9) and is not reconciled in cv-1.

### Step-by-Step

#### Step 1: Import `DRepIdentity`

At the top of `source/renderer/app/api/wallets/types.ts`, directly after line
5 (`import type { ApiTokens } from '../assets/types';`), add:

```ts
import type { DRepIdentity } from '../../../../common/types/governance.types';
```

(Path precedent: `containers/voting/VotingGovernancePage.tsx:12` uses the same
four-level relative import from an equally deep directory. `types.ts` already
participates in a benign import cycle with `domains/Wallet.ts`; a type-only
import adds no runtime edge.)

#### Step 2: Define `WalletVotingTarget`

Immediately after the `DelegationStatus` union (after `types.ts:84`, as
corrected by task-127), add:

```ts
export type WalletVotingTarget =
  | {
      kind: 'drep';
      drep: DRepIdentity;
      source: 'verified' | 'unverified' | 'onchain';
    }
  | { kind: 'abstain' }
  | { kind: 'no_confidence' };
```

(Shape verbatim from current-vote-display-design.md:88-91; discriminator
`kind` with values `'drep' | 'abstain' | 'no_confidence'`.)

#### Step 3: Widen the two delegation types

At `types.ts:105-108` the current code is:

```ts
export type WalletDelegation = {
  status: DelegationStatus;
  target?: string;
};
```

Replace with:

```ts
export type WalletDelegation = {
  status: DelegationStatus;
  target?: string;
  voting?: WalletVotingTarget;
};
```

At `types.ts:110-114` the current code is:

```ts
export type WalletNextDelegation = {
  status: DelegationStatus;
  target?: string;
  changes_at: WalletNextDelegationEpoch;
};
```

Replace with:

```ts
export type WalletNextDelegation = {
  status: DelegationStatus;
  target?: string;
  voting?: WalletVotingTarget;
  changes_at: WalletNextDelegationEpoch;
};
```

#### Step 4: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
grep -rn "interface DRepIdentity" source | cat   # exactly ONE hit: source/common/types/governance.types.ts (never redefined)
grep -n "WalletVotingTarget" source/renderer/app/api/wallets/types.ts
yarn test:jest tests/jest --runInBand   # types-only change; all suites stay green
yarn lint
```

Do NOT run prettier on `types.ts` (pre-existing file) — match the surrounding
style shown in the blocks above.

### Acceptance

- [ ] `WalletDelegation` and `WalletNextDelegation` expose
      `voting?: WalletVotingTarget` (AC-1).
- [ ] `WalletVotingTarget` discriminator is `kind` with values
      `'drep' | 'abstain' | 'no_confidence'` (AC-2).
- [ ] `DRepIdentity` contains `raw` (required) plus optional
      `cip129`/`cip105`/`credentialHex` — satisfied by importing
      `source/common/types/governance.types.ts:20-31` (AC-3, D-6).
- [ ] `DRepIdentity` carries `credentialType` — same import (AC-4).
- [ ] Exactly one `DRepIdentity` definition exists repo-wide.
- [ ] `tsc` clean; no runtime code added.

---

## task-129: `normalizeDRepIdentity` helper

**Files touched:**

- `source/renderer/app/utils/governance/normalizeDRepIdentity.ts` (new —
  creates the `utils/governance/` directory)
- `tests/jest/governance/normalizeDRepIdentity.spec.ts` (new)

**Context.** Pure decoder per current-vote-display-design.md:151-166:
`bech32.decode` preserving the HRP; CIP-129 `drep1…` = 29-byte payload with
header `0x22` (key) / `0x23` (script); CIP-105 `drep_vkh1…` / `drep_script1…`
= bare 28-byte credential; anything else → `null`. Reuses the existing
`bech32@2.0.0` renderer dependency (`package.json:204`; import precedent
`source/renderer/app/utils/crypto.ts:4`) — no new dependency. The main-process
CIP-129 precedent (`GovernanceQueryService.ts:620-640`,
`Cardano.DRepID.cip129FromCredential`) is main-only; reuse the PATTERN (header
byte + credential), not the file.

**Locked invariants (inline).** Byte-equality (invariant 10): `raw` is
returned untouched, and CIP-129 ↔ CIP-105 round-trips are lossless. Purity:
no side effects, never throws, never logs — the sanitized unknown-HRP warning
belongs to the caller (task-130). Key and script DReps with identical 28-byte
credentials are NEVER conflated: `credentialType` always set, and the derived
CIP-129 forms differ in the header byte.

**Resolved judgment calls (do not revisit):**

- Decode limit: use the `bech32` default (90 chars). Every valid DRep encoding
  is ≤ 64 chars; overlong input is correctly rejected as invalid.
- HRP `drep` with a 28-byte payload (the deprecated pre-Conway CIP-105 `drep`
  form, e.g. the rejection vector in the task-126 table) → `null`. The design
  mandates 29 bytes + header for HRP `drep` and "Reject any other HRP, length
  mismatch, or bech32-decode failure by returning null"
  (current-vote-display-design.md:162-164).
- Byte handling uses plain `number[]` from `bech32.fromWords` (typed
  `ArrayLike<number> → number[]` in `bech32/dist/index.d.ts`) — no `Buffer`,
  no extra imports.

### Step-by-Step

#### Step 1: Create the module

```bash
mkdir -p source/renderer/app/utils/governance
```

Create `source/renderer/app/utils/governance/normalizeDRepIdentity.ts` with
exactly:

```ts
import { bech32 } from 'bech32';
import type { DRepIdentity } from '../../../../common/types/governance.types';

const CIP129_KEY_HEADER = 0x22;
const CIP129_SCRIPT_HEADER = 0x23;
const CREDENTIAL_BYTE_LENGTH = 28;

const toHex = (bytes: number[]): string =>
  bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('');

/**
 * Pure decoder for DRep identifiers: CIP-129 `drep1…` (29-byte payload with a
 * 0x22 key / 0x23 script header) and CIP-105 `drep_vkh1…` / `drep_script1…`
 * (bare 28-byte credential). Unknown HRP, length mismatch, bad checksum, or
 * bad header returns null; never throws, never logs.
 */
export function normalizeDRepIdentity(raw: string): DRepIdentity | null {
  let prefix: string;
  let bytes: number[];
  try {
    const decoded = bech32.decode(raw);
    prefix = decoded.prefix;
    bytes = bech32.fromWords(decoded.words);
  } catch {
    return null;
  }
  if (prefix === 'drep') {
    if (bytes.length !== CREDENTIAL_BYTE_LENGTH + 1) {
      return null;
    }
    const header = bytes[0];
    if (header !== CIP129_KEY_HEADER && header !== CIP129_SCRIPT_HEADER) {
      return null;
    }
    const credentialType = header === CIP129_KEY_HEADER ? 'key' : 'script';
    const credential = bytes.slice(1);
    const cip105Hrp = credentialType === 'key' ? 'drep_vkh' : 'drep_script';
    return {
      raw,
      cip129: raw,
      cip105: bech32.encode(cip105Hrp, bech32.toWords(credential)),
      credentialHex: toHex(credential),
      credentialType,
    };
  }
  if (prefix === 'drep_vkh' || prefix === 'drep_script') {
    if (bytes.length !== CREDENTIAL_BYTE_LENGTH) {
      return null;
    }
    const credentialType = prefix === 'drep_vkh' ? 'key' : 'script';
    const header =
      credentialType === 'key' ? CIP129_KEY_HEADER : CIP129_SCRIPT_HEADER;
    return {
      raw,
      cip129: bech32.encode('drep', bech32.toWords([header, ...bytes])),
      cip105: raw,
      credentialHex: toHex(bytes),
      credentialType,
    };
  }
  return null;
}
```

(`catch` without a binding is the repo's precedent for discard-and-return —
e.g. `source/renderer/app/stores/VotingStore.ts:408`; tsconfig `target:
es2019` supports it.)

#### Step 2: Create the spec

Create `tests/jest/governance/normalizeDRepIdentity.spec.ts` with exactly:

```ts
import { bech32 } from 'bech32';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';

// Checksum-verified vector set: each CIP-129 id decodes to a 29-byte payload
// (0x22 key / 0x23 script header) sharing its credential bytes with the
// matching CIP-105 form, so cross-encoding assertions are exact.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_CIP129 =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const SCRIPT_CIP105 =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';
const SCRIPT_CREDENTIAL_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
// Deprecated pre-Conway form: HRP `drep` over a bare 28-byte credential.
const DEPRECATED_DREP_28_BYTE =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const POOL_ID = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2';

describe('normalizeDRepIdentity', () => {
  it('normalizes a CIP-129 key DRep id', () => {
    expect(normalizeDRepIdentity(KEY_CIP129)).toEqual({
      raw: KEY_CIP129,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    });
  });

  it('normalizes a CIP-129 script DRep id', () => {
    expect(normalizeDRepIdentity(SCRIPT_CIP129)).toEqual({
      raw: SCRIPT_CIP129,
      cip129: SCRIPT_CIP129,
      cip105: SCRIPT_CIP105,
      credentialHex: SCRIPT_CREDENTIAL_HEX,
      credentialType: 'script',
    });
  });

  it('normalizes a CIP-105 key-hash DRep id (drep_vkh)', () => {
    expect(normalizeDRepIdentity(KEY_CIP105)).toEqual({
      raw: KEY_CIP105,
      cip129: KEY_CIP129,
      cip105: KEY_CIP105,
      credentialHex: KEY_CREDENTIAL_HEX,
      credentialType: 'key',
    });
  });

  it('normalizes a CIP-105 script-hash DRep id (drep_script)', () => {
    expect(normalizeDRepIdentity(SCRIPT_CIP105)).toEqual({
      raw: SCRIPT_CIP105,
      cip129: SCRIPT_CIP129,
      cip105: SCRIPT_CIP105,
      credentialHex: SCRIPT_CREDENTIAL_HEX,
      credentialType: 'script',
    });
  });

  it('round-trips drep1 -> cip105 -> drep1 losslessly for key and script', () => {
    const keyIdentity = normalizeDRepIdentity(KEY_CIP129);
    expect(normalizeDRepIdentity(keyIdentity.cip105).cip129).toBe(KEY_CIP129);
    const scriptIdentity = normalizeDRepIdentity(SCRIPT_CIP129);
    expect(normalizeDRepIdentity(scriptIdentity.cip105).cip129).toBe(
      SCRIPT_CIP129
    );
  });

  it('never equates a key DRep and a script DRep sharing credential bytes', () => {
    const words = bech32.toWords(new Array(28).fill(7));
    const key = normalizeDRepIdentity(bech32.encode('drep_vkh', words));
    const script = normalizeDRepIdentity(bech32.encode('drep_script', words));
    expect(key.credentialHex).toBe(script.credentialHex);
    expect(key.credentialType).toBe('key');
    expect(script.credentialType).toBe('script');
    expect(key.cip129).not.toBe(script.cip129);
  });

  it('returns null, without throwing, for invalid or foreign input', () => {
    const invalidInputs = [
      '',
      'abstain',
      'no_confidence',
      'not-a-bech32-string',
      POOL_ID,
      DEPRECATED_DREP_28_BYTE,
      `${KEY_CIP129.slice(0, -1)}x`,
    ];
    invalidInputs.forEach((value) => {
      expect(normalizeDRepIdentity(value)).toBeNull();
    });
  });

  it('returns null for a drep payload with an unknown CIP-129 header byte', () => {
    const badHeader = bech32.encode(
      'drep',
      bech32.toWords([0x99, ...new Array(28).fill(7)])
    );
    expect(normalizeDRepIdentity(badHeader)).toBeNull();
  });
});
```

Notes pinned for the implementer: the `abstain` / `no_confidence` sentinels
are NOT DRep ids — the normalizer rejects them; the mapper (task-130) handles
them BEFORE calling the normalizer (invariant 13).
`` `${KEY_CIP129.slice(0, -1)}x` `` corrupts the checksum (single-character
substitution is always detected by bech32). The in-test synthesized vectors
use the `bech32` library itself, honoring D-8.

#### Step 3: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn test:jest tests/jest/governance/normalizeDRepIdentity.spec.ts --runInBand
yarn lint      # source/ is linted; the new module must pass
node_modules/.bin/prettier --check source/renderer/app/utils/governance/normalizeDRepIdentity.ts tests/jest/governance/normalizeDRepIdentity.spec.ts
```

If the prettier check fails, run the same command with `--write` (both files
are created by this task) and re-run the spec.

### Acceptance

- [ ] Pure function, no side effects — no logging, no mutation, `raw` returned
      byte-equal (AC-1, invariant 10).
- [ ] Round-trips `drep1 → cip105 → drep1` losslessly (AC-2 — covered by the
      round-trip spec).
- [ ] Invalid bech32 input returns `null` consistently — never throws (AC-3).
- [ ] Unit tests cover all three prefixes plus invalid input (AC-4).
- [ ] Uses HRP-preserving `bech32.decode`; distinguishes `drep`, `drep_vkh`,
      `drep_script` even with identical 28-byte payloads (AC-5).
- [ ] Returns `credentialType: 'key' | 'script'` (AC-6).
- [ ] Reuses the existing `bech32` dependency; `package.json` diff is empty
      (AC-7 — verify with `git diff package.json`).

---

## task-130: Mapper in `_createWalletFromServerData` + collision rules

**Files touched:**

- `source/renderer/app/api/api.ts` (edit)
- `source/renderer/app/domains/Wallet.ts` (edit — `WalletProps` pass-through
  entry and one type import ONLY; everything else in `Wallet.ts` is task-131)

**Context.** Implements the design's `parseVoting` + 4-way status switch
(current-vote-display-design.md:122-146) inside
`_createWalletFromServerData` (`api.ts:3010-3112`). `delegation.active.voting`
is the authoritative current state — the newest on-chain delegation IS
current, no waiting period (plan :154).

**Locked invariants (inline).**

- Sanitization floor (invariant 2): the unknown-HRP warning logs a bounded,
  allowlisted HRP token only — never the raw DRep id, never the full input
  (design :110/:164). Re-assert the floor suite green after this task.
- `voting`-only status ⇒ `delegatedStakePoolId = null`; `active.target` is
  never parsed as a vote target and never leaks a DRep-era value into the pool
  field.
- `delegating` / `not_delegating` (and legacy) mappings stay byte-identical to
  today's behavior.
- No anchor hydration: `DRepIdentity` has no `givenName`/`anchorUrl`; the
  mapper never populates display metadata.
- Renderer-only: no IPC, no lovelace parsing changes, no WalletsStore polling
  change (design :14).

**Resolved judgment calls (do not revisit):**

- `_createWalletFromServerData` becomes an EXPORTED const so task-134 can
  import it directly for mapper specs. No call-site changes; no behavior
  change.
- `parseVoting` stays module-private in `api.ts`; task-134 tests it through
  the exported mapper with the task-126 fixtures.
- `parseVoting` takes `unknown`: the wire value is read through untyped
  `lodash.get`, and at runtime it is a raw string (`abstain` /
  `no_confidence` / bech32 id) despite the task-128 type (see the task-128
  type-vs-wire note).
- Task boundary vs task-131 (D-7 refinement): this task adds ONLY the optional
  `votingTarget` entry to `WalletProps` so the constructor object literal
  compiles — the `Wallet` constructor is `Object.assign(this, data)`
  (`Wallet.ts:172-174`), so the value already lands on the instance. Task-131
  owns the declared `@observable` class field, the `currentVote`/`isVoting`
  computeds, and the `update()` pick-list entry (`Wallet.ts:177-201`). Do NOT
  add those here.

### Step-by-Step

#### Step 1: Imports in `api.ts`

Add `WalletVotingTarget` to the existing wallets-types import block
(`api.ts:158-189`, the list opened by `// Wallets Types` / `import {` and
closed by `} from './wallets/types';`). Insert one line after `AdaWallets,`:

```ts
  WalletVotingTarget,
```

Directly after `import { filterLogData } from '../../../common/utils/logging';`
(`api.ts:99`), add:

```ts
import { normalizeDRepIdentity } from '../utils/governance/normalizeDRepIdentity';
```

(`logger` at `api.ts:91` and `WalletDelegationStatuses` at `api.ts:6-9` are
already imported; `get` / `last` from lodash are already used at
`api.ts:3052/:3059`.)

#### Step 2: Add `parseVoting` above the mapper

Immediately above `const _createWalletFromServerData = action(`
(`api.ts:3010`), insert:

```ts
const LOGGABLE_HRP_PATTERN = /^[a-z_]{1,16}$/;

// Wire values for delegation.active.voting: 'abstain', 'no_confidence', or a
// bech32 DRep id. Unknown shapes degrade to null; the warning may carry a
// bounded HRP token only — never the raw id (sanitization floor).
const parseVoting = (voting: unknown): WalletVotingTarget | null => {
  if (voting == null || typeof voting !== 'string') return null;
  if (voting === 'abstain') return { kind: 'abstain' };
  if (voting === 'no_confidence') return { kind: 'no_confidence' };
  const drep = normalizeDRepIdentity(voting);
  if (drep === null) {
    const separatorIndex = voting.lastIndexOf('1');
    const hrp = separatorIndex > 0 ? voting.slice(0, separatorIndex) : '';
    logger.warn('AdaApi::parseVoting unrecognized voting target', {
      hrp: LOGGABLE_HRP_PATTERN.test(hrp) ? hrp : 'invalid',
    });
    return null;
  }
  return { kind: 'drep', drep, source: 'onchain' };
};
```

Why the allowlist pattern: a bech32 HRP is lowercase-only and short; any
malformed value whose pre-`1` slice is longer than 16 chars or contains
digits (as every bech32 data part does) collapses to the fixed token
`'invalid'`, so no fragment of a DRep id can reach the log.

#### Step 3: Export the mapper

Change `api.ts:3010` from:

```ts
const _createWalletFromServerData = action(
```

to:

```ts
export const _createWalletFromServerData = action(
```

#### Step 4: Replace the delegation seam

At `api.ts:3051-3056` the current code is:

```ts
    // Current (Active)
    const active = get(delegation, 'active', null);
    const target = get(active, 'target', null);
    const status = get(active, 'status', null);
    const delegatedStakePoolId = isLegacy ? null : target;
    const delegationStakePoolStatus = isLegacy ? null : status;
```

Replace with:

```ts
    // Current (Active)
    const active = get(delegation, 'active', null);
    const target = get(active, 'target', null);
    const status = get(active, 'status', null);
    const delegationStakePoolStatus = isLegacy ? null : status;
    // A voting-only status never carries a stake-pool target; active.target
    // must never be surfaced as a pool id in that state.
    let delegatedStakePoolId: string | null = null;
    let votingTarget: WalletVotingTarget | null = null;
    if (!isLegacy) {
      switch (status) {
        case WalletDelegationStatuses.VOTING:
          delegatedStakePoolId = null;
          votingTarget = parseVoting(get(active, 'voting', null));
          break;
        case WalletDelegationStatuses.VOTING_AND_DELEGATING:
          delegatedStakePoolId = target;
          votingTarget = parseVoting(get(active, 'voting', null));
          break;
        case WalletDelegationStatuses.DELEGATING:
        case WalletDelegationStatuses.NOT_DELEGATING:
        default:
          delegatedStakePoolId = target;
          votingTarget = null;
          break;
      }
    }
```

Byte-identical proof for the untouched paths: today non-legacy
`delegatedStakePoolId = target` where `target` defaults to `null` via
`get(active, 'target', null)`; the `delegating`/`not_delegating`/`default`
branch assigns exactly that same `target`, and the legacy path stays `null`.
`delegationStakePoolStatus` is unchanged. Do not touch the `// Last` block
(`api.ts:3057-3063`).

#### Step 5: Pass `votingTarget` into the constructor

In the `return new Wallet({ … })` call (`api.ts:3089-3110`), after the line
`pendingDelegations: next,` (`api.ts:3108`) and before `discovery,`, add:

```ts
      votingTarget,
```

#### Step 6: `WalletProps` pass-through in `Wallet.ts`

Add `WalletVotingTarget` to the existing type-only import from
`'../api/wallets/types'` (`Wallet.ts:4-11`) so the list reads:

```ts
import type {
  WalletSyncState,
  SyncStateStatus,
  DelegationStatus,
  WalletUnit,
  WalletPendingDelegations,
  Discovery,
  WalletVotingTarget,
} from '../api/wallets/types';
```

In `WalletProps` (`Wallet.ts:112-132`), after
`pendingDelegations?: WalletPendingDelegations;` (`Wallet.ts:128`), add:

```ts
  votingTarget?: WalletVotingTarget | null;
```

Nothing else in `Wallet.ts` changes in this task (no class field, no computed,
no pick-list entry — task-131).

#### Step 7: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn lint
yarn test:jest tests/jest --runInBand   # every existing suite stays green
yarn test:jest tests/jest/security/governance-sanitization.spec.ts --runInBand   # floor re-asserted (invariant 2)
grep -n "votingTarget" source/renderer/app/api/api.ts source/renderer/app/domains/Wallet.ts
grep -n "export const _createWalletFromServerData" source/renderer/app/api/api.ts
grep -rn "givenName\|anchorUrl" source/renderer/app/api/api.ts || echo "OK: no anchor hydration in mapper"
```

Behavioral assertions (voting fixture ⇒ `delegatedStakePoolId === null` and
populated `votingTarget`; `delegating_and_voting` fixture ⇒ both populated;
absent `active.voting` ⇒ `votingTarget === null`; unknown HRP ⇒ sanitized
warning + `null`) are formally pinned by the task-134 specs against the
task-126 fixtures — this task's gate is compile + lint + all existing suites
+ the floor suite + the greps above.

### Acceptance

- [ ] `status === 'voting'` ⇒ `delegatedStakePoolId === null` and
      `votingTarget` populated from `active.voting` (AC-1 — branch in Step 4;
      pinned by task-134).
- [ ] `status === 'delegating_and_voting'` ⇒ both `delegatedStakePoolId` and
      `votingTarget` populated (AC-2 — branch in Step 4; pinned by task-134).
- [ ] `delegating` / `not_delegating` mappings byte-identical to today
      (AC-3 — proof in Step 4; full existing jest run green).
- [ ] `status === 'voting'` with absent `active.voting` ⇒ `votingTarget` is
      `null`; `active.target` is never parsed as a vote (AC-4 — `parseVoting`
      only ever receives `active.voting`).
- [ ] `DRepIdentity` carries no `givenName`/`anchorUrl`; the mapper hydrates no
      anchor-derived display values (AC-5 — grep in Step 7).
- [ ] Unknown HRP: parser returns `null`, warning carries the allowlisted
      `hrp` token only, wallet treated as if `voting === undefined`.
- [ ] Floor suite green; `tsc` clean.

---

## task-131: Wallet domain `votingTarget`/`currentVote`/`isVoting` incl. `update()` pick list

**Files touched:**

- `source/renderer/app/domains/Wallet.ts` (edit — the substantive change; the
  whole of Steps 1-3)
- `source/common/types/governance.types.ts` (edit — one comment line, Step 4;
  no type change)

**Context.** Task-130 already added the `votingTarget?: WalletVotingTarget |
null` entry to `WalletProps` and `WalletVotingTarget` to the type-only import
list in `Wallet.ts` (task-130 Step 6) — do NOT re-add either. The constructor
is `Object.assign(this, data)` (`Wallet.ts:172-174`), so mapped values already
land on instances; what is missing is the declared `@observable` class field
(so MobX tracks it), the `currentVote`/`isVoting` computeds, and the
`update()` pick-list entry. All line numbers below are the `b900b99b3`
pre-implementation state — task-130 inserts one `WalletProps` line and one
import line above these regions, so re-anchor by the quoted content.

**Locked invariants (inline).**

- Pick-list trap (D-7, R-2): the `update()` method assigns
  `pick(other, [...])` over an EXPLICIT field list (`Wallet.ts:177-201`).
  Omitting `'votingTarget'` there compiles clean and fails invisibly — every
  wallet poll refresh would silently keep the stale vote target. BOTH
  `WalletProps` (done by task-130) and the pick list (this task) must carry
  the field.
- No historical vote-target fields and no `pendingVote` computed (D-10 —
  the task-134 description's `pendingVote` mention is description drift; the
  "pending" coverage comes from the `wallet-delegating-and-voting.json`
  fixture's non-empty `next` array).
- Renderer-only: no WalletsStore polling change, no new IPC.

**Resolved judgment calls (do not revisit):**

- The class field is typed `WalletVotingTarget | null | undefined`, matching
  the sibling optional-observable style
  (`delegatedStakePoolId: string | null | undefined`, `Wallet.ts:153-154`).
  `currentVote` normalizes `undefined` → `null` so every consumer sees only
  `WalletVotingTarget | null` (the tsconfig has `strictNullChecks` off —
  first-half Cross-Cutting Renderer Note — so the explicit normalization is
  the contract, not the compiler).
- Placement mirrors `WalletProps` ordering: field between
  `pendingDelegations` and `discovery`; pick-list entry directly after
  `'pendingDelegations',`; computeds directly after `isDelegating`.
- `??` is repo-idiomatic (`stores/GovernanceStore.ts:192`
  `return this.defaultCohort ?? this.drepList;`) and downlevels fine under
  `target: es2019` (`tsconfig.json:14`).
- Task-131's AC "Unit tests cover all four delegation statuses plus pending"
  is formally pinned by the task-134 specs in this same slice
  (`tests/jest/api/createWalletFromServerData.spec.ts` for the statuses and
  the pending fixture; `tests/jest/api/walletVotingComputeds.spec.ts` for the
  computeds and `update()` propagation) — mirroring how task-130's behavioral
  ACs are pinned. This task's own gate is compile + lint + all existing
  suites + the greps below.

### Step-by-Step

#### Step 1: Declare the observable class field

At `Wallet.ts:161-164` the current code is:

```ts
  @observable
  pendingDelegations: WalletPendingDelegations;
  @observable
  discovery: Discovery;
```

Insert the new field between the two so the block reads:

```ts
  @observable
  pendingDelegations: WalletPendingDelegations;
  @observable
  votingTarget: WalletVotingTarget | null | undefined;
  @observable
  discovery: Discovery;
```

#### Step 2: Extend the `update()` pick list

At `Wallet.ts:193-196` (inside the `pick(other, [` array opened at
`Wallet.ts:180`) the current code is:

```ts
        'lastDelegatedStakePoolId',
        'lastDelegationStakePoolStatus',
        'pendingDelegations',
        'discovery',
```

Insert `'votingTarget',` directly after `'pendingDelegations',` so it reads:

```ts
        'lastDelegatedStakePoolId',
        'lastDelegationStakePoolStatus',
        'pendingDelegations',
        'votingTarget',
        'discovery',
```

#### Step 3: Add the computeds

Directly after the closing brace of the `isDelegating` computed
(`Wallet.ts:239-247`, ends with `].includes(statusToCheck);` + `}`) and before
`@computed get isSequential()`, insert:

```ts
  @computed
  get currentVote(): WalletVotingTarget | null {
    return this.votingTarget ?? null;
  }

  @computed
  get isVoting(): boolean {
    return this.currentVote !== null;
  }
```

#### Step 4: Drop the task id from the `DRepIdentity` comment

`source/common/types/governance.types.ts:16-19` currently reads:

```ts
/**
 * Discriminated DRep identity with all known encodings.
 * Populated by normalizeDRepIdentity (cv-1, task-129).
 */
```

Remove the parenthetical only, so line 18 reads:

```ts
 * Populated by normalizeDRepIdentity.
```

Nothing else in the file changes — the interface, its members, and every
member doc comment stay byte-identical. This is the repo's only remaining
task id in a code comment; the Cross-Cutting grep floor below pins it at
zero.

#### Step 5: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
grep -n "votingTarget" source/renderer/app/domains/Wallet.ts
# Expected exactly 4 hits: the WalletProps entry (task-130), the @observable
# field, the 'votingTarget' pick-list entry, and `this.votingTarget` in
# currentVote.
grep -rn "task-1[0-9][0-9]" source tests storybook || echo "OK: no task ids in code"
node_modules/.bin/jest --runInBand   # whole tree: all 86 suites stay green
yarn lint
```

`Wallet.ts` is a shared domain object: five specs import it, and only
`tests/jest/api/walletDelegationStatuses.spec.ts` is inside the `tests/jest`
path filter. Confirm the other four are green in the unfiltered run above —
`source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx`,
`source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx`,
`source/renderer/app/components/wallet/WalletSendForm.spec.tsx`, and
`tests/wallets/unit/wallet-utils.spec.ts` — and report the suite/test counts
of the whole-tree run in the task's statusReason. A filtered run may never be
reported as "all existing suites green".

Do NOT run prettier on `Wallet.ts` or `governance.types.ts` (pre-existing
files) — match the surrounding style shown in the blocks above.

### Acceptance

- [ ] `Wallet.currentVote` returns the parsed `WalletVotingTarget` or `null`
      (AC-1 — Step 3; formally pinned by
      `tests/jest/api/walletVotingComputeds.spec.ts`, task-134).
- [ ] `isVoting === true` iff `currentVote !== null` (AC-2 — Step 3; same
      pin).
- [ ] Unit tests cover all four delegation statuses plus pending (AC-3 —
      discharged in-slice by the task-134 specs; the pending case is the
      `wallet-delegating-and-voting.json` fixture's `next` entry, D-10).
- [ ] `Wallet.update()` pick list explicitly includes `'votingTarget'`
      (AC-4 — Step 2; propagation pinned by the task-134 `update()` cases).
- [ ] `WalletProps` and the type import were NOT re-modified (they already
      carry the task-130 entries; Step 5 grep shows exactly 4 hits).
- [ ] The `(cv-1, task-129)` parenthetical is gone from
      `governance.types.ts:18` while the "Populated by normalizeDRepIdentity"
      sentence remains; `grep -rn "task-1[0-9][0-9]" source tests storybook`
      returns zero hits (Step 4).
- [ ] `tsc` clean; the UNFILTERED whole-tree jest run is green (86 suites
      at cv-1 close) and its counts are recorded in the statusReason.

---

## task-132: `CurrentVoteSummary` CORE states, no live badge

**Files created (all new):**

- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx`

**Context.** Pure presentational component per
current-vote-display-design.md:170-187: props exactly
`{ currentVote: WalletVotingTarget | null }`; four render rules (design
:182-185). It is NOT mounted anywhere in cv-1 — mounting into
`VotingPowerDelegation` is cv-2 (task-139); in this slice only Storybook
(task-133) and the snapshot spec (task-134) consume it, so
`VotingPowerDelegation.tsx` stays byte-identical. Reuses `DRepIdDisplay`
(props `{ drepId: string; showCopiedConfirmation?; intl }`,
`DRepIdDisplay.tsx:28-32`; truncation consts :35-37; default export wrapped
in `injectIntl`, :98) for the id row, and `DRepSourceLabel` (prop `source:
DRepSourceLabelVariant`, `DRepSourceLabel.tsx:18-24`) for the on-chain source
label.

**Locked invariants (inline).**

- NO live status badge (invariant 14): canonical on-chain DRep status is
  `active | inactive`, `expiring` is renderer-derived, and the badge is cv-2
  (task-136). Nothing in this component reads `GovernanceStore`, `drepIndex`,
  or `DRepStatusBadge`.
- No cardano-cli spawn and no fallback IPC lookup to compensate for the
  missing badge (design :187). The component's only input is the prop.
- No auto-delegation (invariant 9): `currentVote == null` renders the
  CIP-1694 reward-withdrawal warning + "Choose a delegation" CTA — the panel
  is never hidden and never collapsible (ux :31).
- Sentinels (invariant 13): `abstain` / `no_confidence` render a badge + a
  caption and NO DRep id row.
- Byte-equality (invariant 10): the DRep state's id row renders
  `currentVote.drep.raw` — the untouched wire string. `DRepIdDisplay`
  truncates the visible text but exposes the full id via tooltip,
  `aria-label`, and copy (which copies the full `drepId`), so no byte is lost
  to display.
- No English-literal fallback (D-4): `DRepSourceLabelVariant` is only
  `'on-chain' | 'on-chain-anchor-reference'` (`DRepSourceLabel.tsx:18`) and
  cannot express `delegatedToDRep` / `abstain` / `noConfidence`; those labels
  render via the component-local react-intl messages below.
  `DRepSourceLabel`'s prop contract is NOT widened in cv-1.
- No anchor-derived display: no `givenName`, no anchor URL, no view-details
  link (anchor-1/2 and cv-2 respectively; design :99, plan :158).
- Sanitization floor (invariant 2): the component renders DRep ids in the
  DOM (allowed) but never logs, stores, or emits them.

**Resolved judgment calls (do not revisit):**

- The messages module is created HERE, not in task-135: the component cannot
  satisfy "Do NOT silently fall back to English literals" without message
  definitions, and task-133/134 render it before task-135 runs. It carries
  the FULL 12-key core inventory — including `drep.viewDetails` /
  `drep.anchorMetadata`, which are defined but NOT rendered in cv-1 (cv-2
  wires them; task-135's AC-1 demands their catalog presence). Task-135 owns
  catalog population and ja-JP copy only.
- The DRep state renders ONE id row: `currentVote.drep.raw`. The dual
  CIP-129/CIP-105 row in the UX mock (ux :77) belongs to the full cv-2 card;
  cv-1 is "DRep-ID-only" (plan :284).
- `DRepSourceLabel` renders with `source="on-chain"` unconditionally in the
  DRep state: cv-1's mapper only ever emits `source: 'onchain'` (task-130
  Step 2), and the on-chain variant is the only truthful label before the
  verified-anchor pipeline exists.
- The CTA `Button` renders with NO `onClick` in cv-1: the design pins props
  to exactly `{ currentVote }` (design :174-176), and cv-2 (task-139) mounts
  and wires the panel. `ButtonSkin` spreads only DOM-safe props onto a plain
  `<button>` (`react-polymorph/lib/skins/simple/ButtonSkin.js`), so an
  absent handler is inert, not an error.
- Function component + `injectIntl` + `intlShape` typing, following the
  `DRepIdDisplay.tsx` precedent (not class components, not `useIntl` — the
  repo's react-intl version predates hooks).
- Glyphs `●` / `⊘` / `✕` are `aria-hidden="true"` and always paired with
  visible localized text (ux accessibility table: glyph+text, never
  color-only); the warning paragraph carries `role="alert"`.

### Step-by-Step

#### Step 1: Create `CurrentVoteSummary.messages.ts`

Convention precedent: `VotingPowerDelegation.messages.ts:1-9`. Exact file
content:

```ts
import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  headerCurrent: {
    id: 'voting.governance.currentVote.headerCurrent',
    defaultMessage: '!!!Current delegation',
    description: 'Header of the current-vote summary panel',
  },
  statusDelegatedToDRep: {
    id: 'voting.governance.currentVote.statusDelegatedToDRep',
    defaultMessage: '!!!Delegated to DRep',
    description:
      'Status label when the wallet delegates its voting power to a DRep',
  },
  statusAbstain: {
    id: 'voting.governance.currentVote.statusAbstain',
    defaultMessage: '!!!Abstain',
    description: 'Status label when the wallet voting power is set to Abstain',
  },
  statusNoConfidence: {
    id: 'voting.governance.currentVote.statusNoConfidence',
    defaultMessage: '!!!No Confidence',
    description:
      'Status label when the wallet voting power is set to No Confidence',
  },
  noDelegationTitle: {
    id: 'voting.governance.currentVote.noDelegation.title',
    defaultMessage: '!!!No governance delegation',
    description: 'Panel title when the wallet has no governance delegation',
  },
  noDelegationWarning: {
    id: 'voting.governance.currentVote.noDelegation.warning',
    defaultMessage:
      "!!!Your staking rewards cannot be withdrawn until you delegate this wallet's voting power to a DRep, Abstain, or No Confidence.",
    description:
      'Reward-withdrawal warning shown when the wallet has no governance delegation',
  },
  noDelegationSubline: {
    id: 'voting.governance.currentVote.noDelegation.subline',
    defaultMessage:
      '!!!Daedalus will not pick a DRep for you — choose how you want your voting power to participate in Cardano governance.',
    description: 'Subline stating Daedalus never auto-delegates voting power',
  },
  noDelegationCta: {
    id: 'voting.governance.currentVote.noDelegation.cta',
    defaultMessage: '!!!Choose a delegation',
    description: 'Call-to-action to choose a governance delegation',
  },
  drepViewDetails: {
    id: 'voting.governance.currentVote.drep.viewDetails',
    defaultMessage: '!!!View details',
    description:
      'In-app link label to the delegated DRep detail view (rendered in a later slice)',
  },
  drepAnchorMetadata: {
    id: 'voting.governance.currentVote.drep.anchorMetadata',
    defaultMessage: '!!!Anchor metadata ↗',
    description:
      'External link label to the delegated DRep anchor metadata (rendered in a later slice)',
  },
  abstainCaption: {
    id: 'voting.governance.currentVote.abstain.caption',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.',
    description: 'Caption explaining the Abstain delegation state',
  },
  noConfidenceCaption: {
    id: 'voting.governance.currentVote.noConfidence.caption',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.',
    description: 'Caption explaining the No Confidence delegation state',
  },
});
```

Copy is verbatim from the ux inventory (current-vote-display-ux.md:154-186)
and the PRD's i18n Core-Key Inventory; every `defaultMessage` keeps the
leading `!!!` (invariant 11). The apostrophe in `wallet's` is a literal in
ICU message syntax (it does not precede `{` or `}`), and prettier keeps that
one string double-quoted (`.prettierrc` sets `singleQuote: true`, which
prettier overrides only for strings containing `'`).

#### Step 2: Create `CurrentVoteSummary.scss`

Theme-variable convention precedent: `DRepEmptyState.scss` (e.g.
`var(--theme-text-secondary, #6b7384)`), `DRepErrorBanner.scss:9`
(`var(--badge-warning-fg, #b76e00)`), and `var(--theme-separator, #e0e0e0)`
(drep-directory modules). Exact file content:

```scss
.component {
  display: flex;
  flex-direction: column;
  gap: 8px;
  margin-bottom: 20px;
  padding: 16px;
  border: 1px solid var(--theme-separator, #e0e0e0);
  border-radius: 4px;
}

.header {
  font-size: 14px;
  font-weight: 600;
  margin: 0;
  color: var(--theme-text-primary);
}

.statusRow {
  display: flex;
  align-items: center;
  gap: 12px;
}

.statusBadge {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  font-size: 14px;
  font-weight: 500;
  color: var(--theme-text-primary);
}

.glyph {
  line-height: 1;
}

.sourceLabel {
  font-size: 12px;
  color: var(--theme-text-secondary, #6b7384);
}

.idRow {
  display: flex;
  align-items: center;
}

.caption {
  font-size: 13px;
  line-height: 1.4;
  margin: 0;
  color: var(--theme-text-secondary, #6b7384);
}

.warning {
  display: flex;
  gap: 8px;
  font-size: 13px;
  line-height: 1.4;
  margin: 0;
  color: var(--badge-warning-fg, #b76e00);
}

.warningGlyph {
  font-weight: 700;
}

.subline {
  font-size: 13px;
  line-height: 1.4;
  margin: 0;
  color: var(--theme-text-secondary, #6b7384);
}

.cta {
  margin-top: 4px;
}
```

No fixed heights or `text-overflow: ellipsis` — long ja-JP strings must wrap
(task-135 overflow review depends on it).

#### Step 3: Create `CurrentVoteSummary.tsx`

Exact file content:

```tsx
import React from 'react';
import { injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepSourceLabel from '../../governance/_shared/DRepSourceLabel';
import type { WalletVotingTarget } from '../../../api/wallets/types';
import { messages } from './CurrentVoteSummary.messages';
import styles from './CurrentVoteSummary.scss';

type Props = {
  currentVote: WalletVotingTarget | null;
  intl: intlShape.isRequired;
};

// Status labels render through the local message set because
// DRepSourceLabel's variant union cannot express them; DRepSourceLabel is
// reused only for the on-chain source label on the DRep state.
function CurrentVoteSummary({ currentVote, intl }: Props) {
  if (currentVote == null) {
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.noDelegationTitle)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.noDelegationTitle)}
        </h3>
        <p className={styles.warning} role="alert">
          <span className={styles.warningGlyph} aria-hidden="true">
            !
          </span>
          {intl.formatMessage(messages.noDelegationWarning)}
        </p>
        <p className={styles.subline}>
          {intl.formatMessage(messages.noDelegationSubline)}
        </p>
        <div>
          <Button
            className={styles.cta}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.noDelegationCta)}
          />
        </div>
      </section>
    );
  }

  if (currentVote.kind === 'drep') {
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        <div className={styles.statusRow}>
          <span className={styles.statusBadge}>
            <span className={styles.glyph} aria-hidden="true">
              ●
            </span>
            {intl.formatMessage(messages.statusDelegatedToDRep)}
          </span>
          <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
        </div>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentVote.drep.raw} />
        </div>
      </section>
    );
  }

  const isAbstain = currentVote.kind === 'abstain';
  return (
    <section
      className={styles.component}
      aria-label={intl.formatMessage(messages.headerCurrent)}
    >
      <h3 className={styles.header}>
        {intl.formatMessage(messages.headerCurrent)}
      </h3>
      <div className={styles.statusRow}>
        <span className={styles.statusBadge}>
          <span className={styles.glyph} aria-hidden="true">
            {isAbstain ? '⊘' : '✕'}
          </span>
          {intl.formatMessage(
            isAbstain ? messages.statusAbstain : messages.statusNoConfidence
          )}
        </span>
      </div>
      <p className={styles.caption}>
        {intl.formatMessage(
          isAbstain ? messages.abstainCaption : messages.noConfidenceCaption
        )}
      </p>
    </section>
  );
}

export default injectIntl(CurrentVoteSummary);
```

Render-rule mapping (design :182-185): `null` → noDelegation warning +
subline + CTA; `kind === 'drep'` → header + "Delegated to DRep" badge +
on-chain source label + id row (id-only — no name, no badge, no links);
`kind === 'abstain'` / `'no_confidence'` → header + glyph badge + caption,
no id row.

#### Step 4: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
# `import styles from './CurrentVoteSummary.scss'` typechecks via the global
# `declare module '*.scss'` (declaration.d.ts:11); the generated
# CurrentVoteSummary.scss.d.ts is a gitignored editor convenience:
node_modules/.bin/typed-scss-modules source/renderer/app
yarn lint
node_modules/.bin/prettier --check \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx
node_modules/.bin/jest --runInBand   # whole tree: all 86 suites stay green
# Boundary greps — all must come back empty:
grep -n "GovernanceStore\|drepIndex\|DRepStatusBadge\|givenName\|anchorUrl" \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx \
  || echo "OK: no badge/store/anchor coupling"
grep -rn "CurrentVoteSummary" source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx \
  || echo "OK: VotingPowerDelegation untouched"
```

If the prettier check fails, run the same command with `--write` (all three
files are created by this task) and re-check.

### Acceptance

- [ ] Renders the DRep id via `DRepIdDisplay` and the source label via
      `DRepSourceLabel` (AC-1 — Step 3 `kind === 'drep'` branch).
- [ ] `currentVote == null` renders the reward-withdrawal warning and CTA
      instead of hiding the panel (AC-2, invariant 9).
- [ ] DRep state renders the DRep id ONLY — no `givenName`, no anchor URL
      link, no in-app view-details link (AC-3; keys for the links exist in
      the messages module but are not rendered).
- [ ] `abstain` and `no_confidence` render with no DRep id (AC-4,
      invariant 13).
- [ ] No live active/inactive/expiring badge anywhere (AC-5, invariant 14 —
      Step 4 boundary grep).
- [ ] Status labels come from the component-local react-intl messages; no
      English string literals in JSX (AC-6, D-4).
- [ ] Props are exactly `{ currentVote: WalletVotingTarget | null }` (+
      injected `intl`); no store, IPC, or cli access (design :174/:187).
- [ ] `tsc` clean, lint clean, prettier clean on the three new files.

---

## task-133: Storybook entry, 4 core knobs

**Files touched:**

- `storybook/stories/governance/CurrentVoteSummary.stories.tsx` (new)
- `storybook/stories/index.ts` (edit — one import line)

**Context.** Storybook 6.4 with the legacy `storiesOf()` API (never CSF).
Registration is explicit: `storybook/main.ts:8` loads ONLY
`storybook/stories/index.ts`, so an unimported story file is invisible.
Decorator + locale conventions follow the existing
`storybook/stories/governance/` files (`DRepCategoryBadge.stories.tsx`,
`DRepDirectory.stories.tsx`): `StoryProvider` wrapping `StoryDecorator`,
`withKnobs`, and NO local `IntlProvider` — the global `StoryWrapper`
decorator provides intl, and the English/Japanese toggle in the preview
window drives every label (binding rule; D-3/F-3).

**Record-only observation (do NOT fix in cv-1):**
`DRepCategoryBadge.stories.tsx`, `DRepDetail.stories.tsx`, and
`DRepDirectoryBanner.stories.tsx` exist under
`storybook/stories/governance/` but are NOT imported by
`storybook/stories/index.ts` (only `./governance/DRepDirectory.stories` is,
`index.ts:17`), so they never render. Pre-existing gap outside cv-1 scope —
carry it into the code-review log; do not silently register them here.

**Locked invariants (inline).**

- Global English/Japanese toggle ONLY: no local `IntlProvider`, no
  per-locale story variants (D-3).
- Exactly the 4 core knob values `noDelegation | drepUnverified | abstain |
  noConfidence`; NO `drepVerified` knob and no 5-value knob (cv-2; ux :199-211
  describes the 5-value cv-2 spec — cv-1 drops only `drepVerified`).
- No `GOVERNANCE_WALLETS` mutation — this story does not touch the
  `voting/Governance.stories.tsx` fixtures at all; `CurrentVoteSummary` takes
  `currentVote` directly, so no wallet factory is needed.
- Bech32 vectors are the checksum-verified task-126 canonical set (D-8) —
  never hand-invent a new one.

**Resolved judgment calls (do not revisit):**

- Knob labels come from the ux §13 table (minus `drepVerified`); knob VALUES
  are the ids the acceptance criterion names.
- The `drepUnverified` knob maps to `source: 'onchain'` — the only source
  value the cv-1 pipeline emits (task-130); the knob id is kept verbatim per
  the AC.
- `key={option}` remount on knob change is applied (cheap, matches the plan
  :156 isolation decision) even though the component is stateless.
- One story (`Core states`) with one select knob, following the
  `DRepDirectory.stories.tsx` knob-to-state mapping pattern (:212-292).

### Step-by-Step

#### Step 1: Create `storybook/stories/governance/CurrentVoteSummary.stories.tsx`

Exact file content:

```tsx
import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, select } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import CurrentVoteSummary from '../../../source/renderer/app/components/voting/voting-governance/CurrentVoteSummary';
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';

const PANEL_STYLE = {
  margin: '0 auto',
  maxWidth: 640,
  padding: 24,
};

// Checksum-verified vectors from the cv-1 fixture set.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';

const CURRENT_VOTE_OPTIONS = {
  'Not delegated (warning)': 'noDelegation',
  'DRep — unverified anchor': 'drepUnverified',
  Abstain: 'abstain',
  'No Confidence': 'noConfidence',
};

const resolveCurrentVote = (option: string): WalletVotingTarget | null => {
  switch (option) {
    case 'drepUnverified':
      return {
        kind: 'drep',
        drep: {
          raw: KEY_CIP129,
          cip129: KEY_CIP129,
          cip105: KEY_CIP105,
          credentialHex: KEY_CREDENTIAL_HEX,
          credentialType: 'key',
        },
        source: 'onchain',
      };
    case 'abstain':
      return { kind: 'abstain' };
    case 'noConfidence':
      return { kind: 'no_confidence' };
    case 'noDelegation':
    default:
      return null;
  }
};

// Locale is intentionally NOT wired here: the global StoryWrapper decorator
// provides the IntlProvider, so the English/Japanese toggle at the top of the
// preview window drives every label rendered below.
storiesOf('Governance / Current Vote Summary', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .addDecorator(withKnobs)
  .add('Core states', () => {
    const option = select(
      'Current vote (mock)',
      CURRENT_VOTE_OPTIONS,
      'noDelegation'
    );
    return (
      <div style={PANEL_STYLE}>
        <CurrentVoteSummary
          key={option}
          currentVote={resolveCurrentVote(option)}
        />
      </div>
    );
  });
```

(The `drep` object literal typechecks structurally against `DRepIdentity`
via `WalletVotingTarget` — no extra type import needed.)

#### Step 2: Register the story

In `storybook/stories/index.ts`, directly after line 17
(`import './governance/DRepDirectory.stories';`), add:

```ts
import './governance/CurrentVoteSummary.stories';
```

#### Step 3: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn lint      # package.json:43 lints storybook/ too
node_modules/.bin/prettier --check storybook/stories/governance/CurrentVoteSummary.stories.tsx
yarn storybook # start-storybook -p 6006 (package.json:55)
```

Manual pass in the running Storybook at `http://localhost:6006` (if the
devcontainer cannot open a browser, `yarn storybook:build` compiling without
errors is the automated floor, and the visual pass moves to the main
checkout):

- "Governance / Current Vote Summary → Core states" is listed.
- Cycle all four knob values: each renders without console errors.
- Toggle English → Japanese via the global DaedalusMenu control: all labels
  switch (before task-135 lands, react-intl logs missing-message warnings
  and falls back to the `!!!` defaultMessage — expected mid-slice; re-check
  after task-135 for real ja-JP copy and NO missing-message warnings).
- No text overflow in either locale at the default story width.

### Acceptance

- [ ] Four core knob values `noDelegation | drepUnverified | abstain |
      noConfidence` render without console errors (AC-1).
- [ ] Story renders in en-US and ja-JP via the GLOBAL locale toggle without
      overflow (AC-2 — final ja-JP check happens after task-135 lands).
- [ ] No local `IntlProvider`, no per-locale variants, no `drepVerified`
      knob, no 5th value.
- [ ] Story is registered in `storybook/stories/index.ts` (Step 2 — without
      it the story is invisible, see the record-only observation).
- [ ] `tsc` + lint + prettier clean on the new file.

---

## task-134: Jest — mapper, Wallet computeds, and `CurrentVoteSummary` core snapshots

**Files created (all new; plus one generated snapshot file):**

- `tests/jest/api/createWalletFromServerData.spec.ts`
- `tests/jest/api/walletVotingComputeds.spec.ts`
- `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`
- `source/renderer/app/components/voting/voting-governance/__snapshots__/CurrentVoteSummary.spec.tsx.snap`
  (generated on first run — commit it)

**Files modified:**

- `tests/jest/governance/normalizeDRepIdentity.spec.ts` (task-129's existing
  eight-case spec — EXTENDED in place by Step 5 with the wrong-length CIP-105
  vector; never re-created, never renamed)

**Context.** `tests/jest/api/` already exists (created by task-127's pin
spec). Jest picks everything up via `roots: ['<rootDir>/tests',
'<rootDir>/source']` (`jest.config.js:129`) and `testMatch:
['**/?(*.)+(spec|test).[tj]s?(x)']` (`jest.config.js:156`); the live
convention is `.spec.ts(x)` (F-3). Component specs are colocated with their
component (precedent: `VotingPowerDelegationConfirmationDialog.spec.tsx`,
`DRepCategoryBadge.spec.tsx`); wire/domain specs live under `tests/jest/`.
`_createWalletFromServerData` is exported by task-130 and takes a single
`AdaWallet` (`api.ts:3010-3012`).

**Import recipe for `api.ts` under jest — VERIFIED empirically in this
worktree (do not improvise):**

- `jest.mock` of `source/renderer/app/utils/logging` is required: the real
  logger destructures `global.electronLog` and calls through it; the global
  is absent in jest (precedent `tests/jest/governance/GovernanceStore.spec.ts:23-32`).
  The mock doubles as the spy for the sanitized warning.
- `jest.mock` of `source/renderer/app/api/utils/request` is ALSO required:
  `request.ts:20-21` reads `global.environment` AND constructs
  `new global.https.Agent(...)` at module scope. Jest provides
  `globals.environment` (`jest.config.js:63-66`) but NOT `global.https`
  (the Electron preload provides it at runtime), so importing `api.ts`
  without this mock crashes with
  `TypeError: Cannot read properties of undefined (reading 'Agent')`.
  With both mocks hoisted, a plain static import of `api.ts` works.
- Fixtures load via `require` + a cast through `unknown`, because
  `AdaWallet` demands `isLegacy` (`types.ts:53`) which the wire JSON
  correctly omits — a pre-existing type/wire mismatch; do not "fix" the
  fixtures or the type (D-9 sibling issue).

**Locked invariants (inline).**

- Sanitization floor (invariant 2): the warning spec asserts the logged
  payload is `{ hrp: <allowlisted-or-'invalid'> }` and that the raw string
  never appears anywhere in the mock's calls. The task-111 floor suite is
  re-asserted green in this task.
- Byte-equality (invariant 10): mapper assertions compare full bech32
  strings and the credential hex byte-for-byte against the checksum-verified
  vectors (D-8).
- Snapshots must show NO status badge, NO givenName, NO anchor/view-details
  links (invariants 14 and the anchor deferral) — the negative assertions
  below pin this beyond snapshot review.
- Prettier 2.1.2 never stabilizes on
  `toHaveBeenCalledWith('string', { object })`; the warning assertions
  destructure `mock.calls` instead (repo-known pitfall).

**Resolved judgment calls (do not revisit):**

- Deep-equality on the mapped `votingTarget` goes through mobx `toJS(...)`:
  after task-131 the field is a deep observable, and `toJS` strips the
  observable wrapper so `toEqual` sees a plain object.
- The Wallet-computeds spec lives beside the other cv-1 wire specs in
  `tests/jest/api/` (the task's `targetPath`), not in a new
  `tests/jest/domains/` — one new directory for the slice, not two.
- The five mapping cases (AC-1) are: drep (voting-only), abstain,
  no_confidence, delegating_and_voting, and pending (the
  `wallet-delegating-and-voting.json` fixture's `next` entry, D-10).
- The snapshot spec renders with `messages` from `en-US.json`; until
  task-135 lands, the 12 new keys are absent from the catalog and react-intl
  falls back to the `!!!` `defaultMessage` — which is byte-identical to what
  task-135 seeds, so the snapshots and text assertions are stable across the
  task boundary (react-intl logs missing-message console noise until then;
  it does not fail the run).

### Step-by-Step

#### Step 1: Create `tests/jest/api/createWalletFromServerData.spec.ts`

Exact file content:

```ts
import path from 'path';
import { toJS } from 'mobx';
import { _createWalletFromServerData } from '../../../source/renderer/app/api/api';
import { logger } from '../../../source/renderer/app/utils/logging';
import type { AdaWallet } from '../../../source/renderer/app/api/wallets/types';

// The real renderer logger writes through global.electronLog, which does not
// exist in the Jest environment; the mock also records the sanitized warning.
jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

// request.ts constructs a global.https.Agent at module scope; the Electron
// preload provides that global, the Jest environment does not. Mapper tests
// never issue requests, so the module is replaced wholesale.
jest.mock('../../../source/renderer/app/api/utils/request', () => ({
  request: jest.fn(),
}));

const mockedWarn = logger.warn as jest.Mock;

// Checksum-verified vector set shared with the cv-1 fixtures.
const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';
const KEY_CREDENTIAL_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const POOL_ID = 'pool1qvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsxqcrqvpsx6m90l2';

const loadFixture = (name: string): AdaWallet =>
  (require(path.join(
    __dirname,
    '../../mocks/wallets',
    name
  )) as unknown) as AdaWallet;

const withDelegation = (delegation: unknown): AdaWallet =>
  (({
    ...(loadFixture('wallet-voting-drep.json') as Record<string, unknown>),
    delegation,
  } as unknown) as AdaWallet);

describe('_createWalletFromServerData voting mapping', () => {
  beforeEach(() => {
    mockedWarn.mockClear();
  });

  it('maps a voting-only DRep wallet: votingTarget populated, pool id null', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-drep.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(wallet.delegationStakePoolStatus).toBe('voting');
    expect(toJS(wallet.votingTarget)).toEqual({
      kind: 'drep',
      drep: {
        raw: KEY_CIP129,
        cip129: KEY_CIP129,
        cip105: KEY_CIP105,
        credentialHex: KEY_CREDENTIAL_HEX,
        credentialType: 'key',
      },
      source: 'onchain',
    });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps delegating_and_voting: pool target AND votingTarget populated', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-delegating-and-voting.json')
    );
    expect(wallet.delegatedStakePoolId).toBe(POOL_ID);
    expect(wallet.delegationStakePoolStatus).toBe('delegating_and_voting');
    expect(toJS(wallet.votingTarget)).toEqual({
      kind: 'drep',
      drep: {
        raw: KEY_CIP105,
        cip129: KEY_CIP129,
        cip105: KEY_CIP105,
        credentialHex: KEY_CREDENTIAL_HEX,
        credentialType: 'key',
      },
      source: 'onchain',
    });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps the abstain sentinel with no pool id', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-abstain.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(toJS(wallet.votingTarget)).toEqual({ kind: 'abstain' });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('maps the no_confidence sentinel with no pool id', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-voting-no-confidence.json')
    );
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(toJS(wallet.votingTarget)).toEqual({ kind: 'no_confidence' });
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('keeps the pending next delegation intact alongside the vote target', () => {
    const wallet = _createWalletFromServerData(
      loadFixture('wallet-delegating-and-voting.json')
    );
    expect(wallet.pendingDelegations).toHaveLength(1);
    expect(wallet.lastDelegationStakePoolStatus).toBe('delegating');
    expect(wallet.lastDelegatedStakePoolId).toBe(POOL_ID);
  });

  it('yields votingTarget null for status voting without active.voting and never parses active.target', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', target: POOL_ID },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    expect(wallet.delegatedStakePoolId).toBeNull();
    expect(mockedWarn).not.toHaveBeenCalled();
  });

  it('degrades an unknown HRP to null with a sanitized HRP-only warning', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', voting: POOL_ID },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    expect(mockedWarn).toHaveBeenCalledTimes(1);
    const [message, data] = mockedWarn.mock.calls[0];
    expect(message).toBe('AdaApi::parseVoting unrecognized voting target');
    expect(data).toEqual({ hrp: 'pool' });
    expect(JSON.stringify(mockedWarn.mock.calls)).not.toContain(POOL_ID);
  });

  it('collapses a malformed voting value to the fixed invalid token', () => {
    const wallet = _createWalletFromServerData(
      withDelegation({
        active: { status: 'voting', voting: 'not-a-bech32-string' },
        next: [],
      })
    );
    expect(wallet.votingTarget).toBeNull();
    const [, data] = mockedWarn.mock.calls[0];
    expect(data).toEqual({ hrp: 'invalid' });
    expect(JSON.stringify(mockedWarn.mock.calls)).not.toContain(
      'not-a-bech32-string'
    );
  });

  it('maps delegating and not_delegating byte-identically to today', () => {
    const delegating = _createWalletFromServerData(
      withDelegation({
        active: { status: 'delegating', target: POOL_ID },
        next: [],
      })
    );
    expect(delegating.delegatedStakePoolId).toBe(POOL_ID);
    expect(delegating.votingTarget).toBeNull();
    const notDelegating = _createWalletFromServerData(
      withDelegation({ active: { status: 'not_delegating' }, next: [] })
    );
    expect(notDelegating.delegatedStakePoolId).toBeNull();
    expect(notDelegating.votingTarget).toBeNull();
  });
});
```

#### Step 2: Create `tests/jest/api/walletVotingComputeds.spec.ts`

Exact file content:

```ts
import BigNumber from 'bignumber.js';
import { toJS } from 'mobx';
import Wallet from '../../../source/renderer/app/domains/Wallet';
import type { WalletProps } from '../../../source/renderer/app/domains/Wallet';
import type { WalletVotingTarget } from '../../../source/renderer/app/api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';

const DREP_TARGET: WalletVotingTarget = {
  kind: 'drep',
  drep: { raw: KEY_CIP129, credentialType: 'key' },
  source: 'onchain',
};

const makeWallet = (votingTarget?: WalletVotingTarget | null): Wallet =>
  new Wallet({
    id: 'computeds-wallet',
    addressPoolGap: 20,
    name: 'computeds wallet',
    amount: new BigNumber(0),
    availableAmount: new BigNumber(0),
    reward: new BigNumber(0),
    assets: { available: [], total: [] },
    passwordUpdateDate: null,
    syncState: { status: 'ready' },
    isLegacy: false,
    delegatedStakePoolId: null,
    delegationStakePoolStatus: null,
    lastDelegatedStakePoolId: null,
    lastDelegationStakePoolStatus: null,
    pendingDelegations: [],
    discovery: 'sequential',
    hasPassword: false,
    votingTarget,
  } as WalletProps);

describe('Wallet.currentVote / Wallet.isVoting', () => {
  it('returns the drep target and isVoting true', () => {
    const wallet = makeWallet(DREP_TARGET);
    expect(toJS(wallet.currentVote)).toEqual(DREP_TARGET);
    expect(wallet.isVoting).toBe(true);
  });

  it('returns the abstain target and isVoting true', () => {
    const wallet = makeWallet({ kind: 'abstain' });
    expect(toJS(wallet.currentVote)).toEqual({ kind: 'abstain' });
    expect(wallet.isVoting).toBe(true);
  });

  it('returns the no_confidence target and isVoting true', () => {
    const wallet = makeWallet({ kind: 'no_confidence' });
    expect(toJS(wallet.currentVote)).toEqual({ kind: 'no_confidence' });
    expect(wallet.isVoting).toBe(true);
  });

  it('returns null and isVoting false for a null target', () => {
    const wallet = makeWallet(null);
    expect(wallet.currentVote).toBeNull();
    expect(wallet.isVoting).toBe(false);
  });

  it('returns null and isVoting false when votingTarget was never set', () => {
    const wallet = makeWallet();
    expect(wallet.currentVote).toBeNull();
    expect(wallet.isVoting).toBe(false);
  });

  it('update() propagates a fresh votingTarget onto a stale instance', () => {
    const stale = makeWallet(null);
    stale.update(makeWallet({ kind: 'abstain' }));
    expect(toJS(stale.currentVote)).toEqual({ kind: 'abstain' });
    expect(stale.isVoting).toBe(true);
  });

  it('update() clears a removed votingTarget instead of sticking stale', () => {
    const stale = makeWallet({ kind: 'no_confidence' });
    stale.update(makeWallet(null));
    expect(stale.currentVote).toBeNull();
    expect(stale.isVoting).toBe(false);
  });
});
```

(The two `update()` cases fail if the Step-2 pick-list entry from task-131
is missing — they are the executable form of the R-2 mitigation.)

#### Step 3: Create `source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx`

Render-wrapper precedent:
`VotingPowerDelegationConfirmationDialog.spec.tsx:30-55` (react-polymorph
`ThemeProvider` + `IntlProvider` with the en-US catalog). Exact file content:

```tsx
import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import CurrentVoteSummary from './CurrentVoteSummary';
import type { WalletVotingTarget } from '../../../api/wallets/types';

const KEY_CIP129 = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const KEY_CIP105 =
  'drep_vkh15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94czu4a4l';

const DREP_VOTE: WalletVotingTarget = {
  kind: 'drep',
  drep: {
    raw: KEY_CIP129,
    cip129: KEY_CIP129,
    cip105: KEY_CIP105,
    credentialHex: 'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c',
    credentialType: 'key',
  },
  source: 'onchain',
};

const renderSummary = (currentVote: WalletVotingTarget | null) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <CurrentVoteSummary currentVote={currentVote} />
      </IntlProvider>
    </ThemeProvider>
  );

describe('CurrentVoteSummary core states', () => {
  afterEach(cleanup);

  it('renders the noDelegation warning, subline, and CTA (snapshot)', () => {
    const { container } = renderSummary(null);
    expect(screen.getByText('!!!No governance delegation')).toBeInTheDocument();
    expect(screen.getByRole('alert')).toBeInTheDocument();
    expect(screen.getByText('!!!Choose a delegation')).toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders the DRep id row with the on-chain label and no badge (snapshot)', () => {
    const { container } = renderSummary(DREP_VOTE);
    expect(screen.getByText('!!!Delegated to DRep')).toBeInTheDocument();
    // DRepIdDisplay truncates the visible text but exposes the full raw id.
    expect(screen.getByLabelText(KEY_CIP129)).toBeInTheDocument();
    expect(screen.getByText('!!!On-chain')).toBeInTheDocument();
    expect(
      screen.queryByText(/Active|Inactive|Expiring/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders abstain with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'abstain' });
    expect(screen.getByText('!!!Abstain')).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });

  it('renders no confidence with a caption and no DRep id (snapshot)', () => {
    const { container } = renderSummary({ kind: 'no_confidence' });
    expect(screen.getByText('!!!No Confidence')).toBeInTheDocument();
    expect(
      screen.getByText(
        '!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/drep1|drep_vkh|drep_script/)
    ).not.toBeInTheDocument();
    expect(container.firstChild).toMatchSnapshot();
  });
});
```

#### Step 4: Run and commit the snapshots

```bash
yarn test:jest source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx --runInBand
```

First run writes
`source/renderer/app/components/voting/voting-governance/__snapshots__/CurrentVoteSummary.spec.tsx.snap`.
Review it before committing: it must contain NO badge markup, NO
`givenName`, NO anchor/view-details link, and the DRep snapshot's visible id
text is the truncated form while the `aria-label` carries the full raw id.
Stage the snapshot file explicitly alongside the spec.

#### Step 5: Extend the task-129 normalizer spec

`tests/jest/governance/normalizeDRepIdentity.spec.ts` already exists with
eight cases (task-129). Add ONE case to its existing `describe` block — do
NOT create a second spec file:

```ts
  it('returns null for a drep_vkh payload of the wrong length', () => {
    const wrongLength = bech32.encode(
      'drep_vkh',
      bech32.toWords(new Array(29).fill(7))
    );
    expect(normalizeDRepIdentity(wrongLength)).toBeNull();
  });
```

`bech32` is already imported at the top of that spec. The vector is
synthesized with the library rather than hand-written (D-8), so its checksum
is valid by construction and the input reaches the CIP-105 length guard
(`normalizeDRepIdentity.ts`, the `bytes.length !== CREDENTIAL_BYTE_LENGTH`
return in the `drep_vkh` / `drep_script` branch) instead of the decode
`catch`. That guard is the one branch task-129's eight cases leave uncovered.

Together with the four `expect(mockedWarn).not.toHaveBeenCalled()` assertions
in the Step-1 accepted-target cases, this pins the normalizer's "never logs"
contract on the ACCEPTED-id path, not only on the rejection paths the
sanitized-warning cases already cover.

#### Step 6: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn test:jest tests/jest/api --runInBand
yarn test:jest tests/jest/governance/normalizeDRepIdentity.spec.ts --runInBand
yarn test:jest source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx --runInBand
yarn test:jest tests/jest/security/governance-sanitization.spec.ts --runInBand   # floor re-asserted (invariant 2)
yarn test:jest   # whole tree: tests/ + colocated source/ specs
yarn lint
node_modules/.bin/prettier --check \
  tests/jest/api/createWalletFromServerData.spec.ts \
  tests/jest/api/walletVotingComputeds.spec.ts \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx
```

### Acceptance

- [ ] All five mapping cases pass: drep, abstain, no_confidence,
      delegating_and_voting, pending (AC-1 — Step 1).
- [ ] The voting-only fixture asserts `delegatedStakePoolId === null`
      (AC-2 — Step 1, first case).
- [ ] Wallet computeds covered for every `WalletVotingTarget` kind plus
      null (and never-set), including `update()` propagation both ways
      (AC-3 — Step 2).
- [ ] `CurrentVoteSummary` snapshots cover
      noDelegation / drepUnverified / abstain / noConfidence (AC-4 — Step 3;
      `DREP_VOTE` with `source: 'onchain'` IS the drepUnverified state).
- [ ] Sanitized-warning cases: HRP-only payload, fixed `'invalid'` token,
      raw string absent from every logged call (invariant 2).
- [ ] All four accepted-target mapper cases — voting-only DRep,
      delegating_and_voting, abstain, no_confidence — assert `mockedWarn` was
      never called, so the no-logging floor is pinned on the accepted-id path
      too (AC-7 — Step 1).
- [ ] `tests/jest/governance/normalizeDRepIdentity.spec.ts` is EXTENDED (nine
      cases) with the wrong-length CIP-105 vector, and a coverage run over
      `normalizeDRepIdentity.ts` reports no uncovered lines (Step 5).
- [ ] Floor suite green; whole-tree jest green; `tsc` clean.

---

## task-135: i18n core keys `voting.governance.currentVote.*`

**Files touched:**

- `source/renderer/app/i18n/locales/en-US.json` (12 new entries — seeded by
  the runner)
- `source/renderer/app/i18n/locales/ja-JP.json` (12 new entries — seeded,
  then hand-translated)
- `source/renderer/app/i18n/locales/defaultMessages.json` (regenerated)
- `translations/messages.json` (regenerated by `yarn i18n:extract`)

**Context.** The message DEFINITIONS already exist
(`CurrentVoteSummary.messages.ts`, task-132 Step 1) — this task runs the
catalog workflow and lands the ja-JP drafts. `yarn i18n:manage` =
`yarn i18n:extract && yarn i18n:check` (`package.json:52-54`); the check step
runs `react-intl-translations-manager`
(`translations/translation-runner.ts`), which regenerates
`defaultMessages.json` and ADDS missing keys to both locale files with the
`defaultMessage` text as the seeded value, preserving existing translations
and keeping the files key-sorted. Extraction covers `defineMessages()` in
`source/**/*.{ts,tsx}` — unrendered messages (the two `drep.*` link labels)
are still extracted, which is exactly how the reserved
`confirmationDialog.previousVote`/`.newVote` convention works (ux :168).

**Locked invariants (inline).**

- Invariant 11: every new en-US AND ja-JP string keeps the leading `!!!`;
  removing `!!!` is a release-end manual review, never a per-slice task.
  (Some pre-existing ja-JP strings lack `!!!` because they were already
  reviewed — e.g. `voting.governance.heading`, `ja-JP.json:958`. Do not
  touch any existing entry.)
- No cv-2 keys: no `sameVoteHint`, no `status.expiring/.inactive/
  .unavailable` (task-146); `confirmationDialog.previousVote`/`.newVote`
  stay reserved-not-wired — none of these appear in the extraction because
  no `defineMessages` defines them (verify in Step 4).
- Whitelist files stay untouched (`whitelist_en-US.json` /
  `whitelist_ja-JP.json` are empty arrays — the repo convention is to leave
  the untranslated report informational).

**Resolved judgment calls (do not revisit):**

- ja-JP terminology reuses the reviewed catalog terms: 棄権 (Abstain,
  `ja-JP.json:944`), 不信任 (No Confidence, :962), DRepに委任 (delegate to
  DRep, :954), オンチェーン (on-chain, :354).
- The 12 new keys sort alphabetically between
  `voting.governance.confirmationDialog.vote` and
  `voting.governance.delegateToDRep` (`ja-JP.json:953-954`; en-US
  neighborhood identical) — the runner maintains this ordering; do not
  reorder by hand.

### Step-by-Step

#### Step 1: Seed the catalogs

```bash
yarn i18n:manage
```

Expected: the run reports 12 added keys per locale
(`voting.governance.currentVote.*`) and no deleted keys. Both locale files
now carry the `!!!` English `defaultMessage` values; `defaultMessages.json`
and `translations/messages.json` are regenerated. The en-US entries are
final for this slice exactly as seeded:

```json
  "voting.governance.currentVote.abstain.caption": "!!!Your stake is recorded on chain as not participating in governance. Rewards can be withdrawn.",
  "voting.governance.currentVote.drep.anchorMetadata": "!!!Anchor metadata ↗",
  "voting.governance.currentVote.drep.viewDetails": "!!!View details",
  "voting.governance.currentVote.headerCurrent": "!!!Current delegation",
  "voting.governance.currentVote.noConfidence.caption": "!!!Your stake counts as Yes on every motion of no-confidence. Rewards can be withdrawn.",
  "voting.governance.currentVote.noDelegation.cta": "!!!Choose a delegation",
  "voting.governance.currentVote.noDelegation.subline": "!!!Daedalus will not pick a DRep for you — choose how you want your voting power to participate in Cardano governance.",
  "voting.governance.currentVote.noDelegation.title": "!!!No governance delegation",
  "voting.governance.currentVote.noDelegation.warning": "!!!Your staking rewards cannot be withdrawn until you delegate this wallet's voting power to a DRep, Abstain, or No Confidence.",
  "voting.governance.currentVote.statusAbstain": "!!!Abstain",
  "voting.governance.currentVote.statusDelegatedToDRep": "!!!Delegated to DRep",
  "voting.governance.currentVote.statusNoConfidence": "!!!No Confidence",
```

#### Step 2: Replace the ja-JP seeds with the draft translations

In `source/renderer/app/i18n/locales/ja-JP.json`, replace the VALUES of the
12 seeded keys (keys and ordering unchanged) with exactly:

```json
  "voting.governance.currentVote.abstain.caption": "!!!ステークはガバナンスに参加しないものとしてオンチェーンに記録されます。報酬は引き出し可能です。",
  "voting.governance.currentVote.drep.anchorMetadata": "!!!アンカーメタデータ ↗",
  "voting.governance.currentVote.drep.viewDetails": "!!!詳細を表示",
  "voting.governance.currentVote.headerCurrent": "!!!現在の委任",
  "voting.governance.currentVote.noConfidence.caption": "!!!ステークはすべての不信任動議において賛成票として集計されます。報酬は引き出し可能です。",
  "voting.governance.currentVote.noDelegation.cta": "!!!委任先を選択",
  "voting.governance.currentVote.noDelegation.subline": "!!!DaedalusがDRepを代わりに選ぶことはありません。Cardanoガバナンスへの投票権の参加方法をご自身で選択してください。",
  "voting.governance.currentVote.noDelegation.title": "!!!ガバナンス委任がありません",
  "voting.governance.currentVote.noDelegation.warning": "!!!このウォレットの投票権をDRep、棄権、または不信任に委任するまで、ステーキング報酬を引き出すことはできません。",
  "voting.governance.currentVote.statusAbstain": "!!!棄権",
  "voting.governance.currentVote.statusDelegatedToDRep": "!!!DRepに委任済み",
  "voting.governance.currentVote.statusNoConfidence": "!!!不信任",
```

All 12 keep the leading `!!!` (preliminary-copy marker; the release-end
review removes it, invariant 11).

#### Step 3: Re-run the manager — must be clean

```bash
yarn i18n:manage
```

Expected: zero added keys, zero deleted keys, and the hand-written ja-JP
values are preserved (the runner only seeds MISSING keys). This is the
"runs clean" acceptance check.

#### Step 4: Verify

```bash
grep -c "voting.governance.currentVote" source/renderer/app/i18n/locales/en-US.json   # 12
grep -c "voting.governance.currentVote" source/renderer/app/i18n/locales/ja-JP.json   # 12
grep "voting.governance.currentVote" source/renderer/app/i18n/locales/ja-JP.json | grep -v '"!!!' || echo "OK: every new ja-JP value keeps !!!"
grep -n "sameVoteHint\|currentVote.status\.\|previousVote\|newVote" source/renderer/app/i18n/locales/en-US.json || echo "OK: no cv-2/reserved keys leaked"
git diff --stat   # only the two locale files, defaultMessages.json, translations/messages.json
git diff source/renderer/app/i18n/locales/ja-JP.json | grep "^-" | grep -v "^---" || echo "OK: no existing ja-JP entry modified or removed"
yarn test:jest source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx --runInBand
# Snapshot stability check: catalog values are byte-identical to the
# defaultMessage fallbacks, so the task-134 snapshots must NOT change.
```

#### Step 5: ja-JP overflow review

```bash
yarn storybook
```

In "Governance / Current Vote Summary → Core states", switch the global
locale to Japanese and cycle all four knob values: every string renders
fully (the `.scss` wraps — no ellipsis, no clipped line), no
missing-message console warnings remain, and the panel height grows
naturally. If the devcontainer cannot open a browser, record this review as
a main-checkout follow-up in the code-review log rather than skipping it.

### Acceptance

- [ ] All 12 core keys present in `en-US.json` AND `ja-JP.json`:
      headerCurrent, statusDelegatedToDRep, statusAbstain,
      statusNoConfidence, noDelegation title/warning/subline/cta,
      drep.viewDetails, drep.anchorMetadata, abstain.caption,
      noConfidence.caption (AC-1).
- [ ] ja-JP copy is a faithful preliminary draft, reviewed for
      length/layout overflow, and every value keeps the leading `!!!`
      (AC-2, invariant 11).
- [ ] `yarn i18n:manage` runs clean on re-run: no missing, no deleted, no
      churn (AC-3 — Step 3).
- [ ] No cv-2 keys (`sameVoteHint`, `status.*`) and no reserved
      `confirmationDialog.previousVote`/`.newVote` in the extraction
      (Step 4 grep).
- [ ] No pre-existing catalog entry modified; whitelist files untouched.
- [ ] task-134 snapshots unchanged after catalog seeding (Step 4).

---

## task-170: Redact raw wallet payloads at the AdaApi wallet-list log sites

**Files touched:**

- `source/renderer/app/api/api.ts` (edit — logger call sites ONLY)
- `tests/jest/security/governance-sanitization.spec.ts` (edit — one new case
  in the existing `call boundaries` describe)

**Context.** `filterLogData` has redacted `drepId` / `dRepId` / `vote` /
`voting` at any depth since slice-1 (`source/common/utils/logging.ts:21`;
governance keys at `:45-48`), but the two wallet-shaped debug logs in
`api.ts` never apply it to the wallet payload: `api.ts:379-383` logs
`wallets` and `legacyWallets` whole with only `hwLocalData` filtered, and
`api.ts:458-460` logs the raw `wallet`. Task-128 widened `WalletDelegation`
with `voting?: WalletVotingTarget`, so `delegation.active.voting` — the
user's own CIP-129/CIP-105 DRep id, or the `abstain` / `no_confidence`
sentinel — now rides those payloads verbatim into the log file. `getWallets`
runs on every wallet-list poll, which makes it the highest-frequency logging
path in the app.

**Locked invariants (inline).**

- Sanitization floor (invariant 2): after this task no DRep id, no
  CIP-129/CIP-105 bech32 string and no `abstain` / `no_confidence` literal
  reaches a logger payload from any Shelley-wallet `api.ts` call site.
- No behavioral change: the requests, their return values and
  `_createWalletFromServerData` are untouched. Only the argument handed to
  `logger.debug` changes.
- Renderer-only: no IPC and no main-process edit. `filterLogData` is already
  imported (`api.ts:99`) — add no new import.
- This closes the highest-frequency key-redactable path. It does NOT close
  the message-substring class (a DRep id embedded in an error *message*),
  which `filterLogData` structurally cannot reach — that stays open by
  design, as slice-3 recorded.

**Resolved judgment calls (do not revisit):**

- The whole payload object is wrapped in the shared `filterLogData` rather
  than filtered field by field: `filterLogData` is typed
  `(data: Record<string, any>)` and an array is not assignable to that type,
  so a per-field call would need a cast — and a second bespoke redactor on a
  security seam is exactly what the floor exists to prevent.
- Wrapping the whole object also omits each wallet's `passphrase` metadata
  (`{ last_updated_at }` — `passphrase` is already on the shared sensitive
  list). That is the ONE non-governance shape change, and it is deliberate:
  strictly more redaction, on a field with no diagnostic value. No other
  logged field changes.
- Legacy/Byron wallet log sites are NOT wrapped. A `LegacyAdaWallet` carries
  no wire `delegation` — Daedalus injects `NOT_DELEGATING` at `api.ts:918` —
  so it cannot carry a vote target, and wrapping would churn unrelated log
  shapes for no floor gain.

### Step-by-Step

#### Step 1: Wrap the `getWallets` success payload

At `api.ts:379-383` the current code is:

```ts
      logger.debug('AdaApi::getWallets success', {
        wallets,
        legacyWallets,
        hwLocalData: filterLogData(hwLocalData),
      });
```

Replace with:

```ts
      logger.debug(
        'AdaApi::getWallets success',
        filterLogData({ wallets, legacyWallets, hwLocalData })
      );
```

`filterLogData` recurses by key name at any depth, so the single outer call
covers `delegation.active.voting` and `delegation.next[*].voting` on every
wallet in both arrays and still filters `hwLocalData` exactly as the nested
call did.

#### Step 2: Wrap the `getWallet` success payload

At `api.ts:458-460` the current code is:

```ts
      logger.debug('AdaApi::getWallet success', {
        wallet,
      });
```

Replace with:

```ts
      logger.debug('AdaApi::getWallet success', filterLogData({ wallet }));
```

#### Step 3: Audit the remaining whole-payload wallet logs

```bash
grep -n "logger.debug('AdaApi::.* success'," -A2 source/renderer/app/api/api.ts \
  | grep -B1 "^[0-9]*-        wallets\?,$"
```

Thirteen sites log a bare `wallet` / `wallets` payload. Classify each by the
type of the logged value and wrap every site whose payload is a Shelley
`AdaWallet` / `AdaWallets`. Measured at HEAD, six can carry
`delegation.active.voting`:

| Call site | Payload |
| --- | --- |
| `api.ts:379` `AdaApi::getWallets success` | `wallets` (Step 1) |
| `api.ts:458` `AdaApi::getWallet success` | `wallet` (Step 2) |
| `api.ts:870` `AdaApi::createWallet success` | `wallet: AdaWallet` (`api.ts:867`) |
| `api.ts:1588` `AdaApi::restoreWallet success` | `wallet: AdaWallet` (`api.ts:1585`) |
| `api.ts:1628` `AdaApi::createHardwareWallet success` | spread of `hardwareWallet: AdaWallet` (`api.ts:1621`) |
| `api.ts:2077` `AdaApi::updateWallet success` | `wallet` — `AdaWallet` on the non-legacy branch (`api.ts:2071`) |

The seven legacy/Byron sites (`:927`, `:1705`, `:1768`, `:1822`, `:1876`,
`:1930`, `:1976`) stay unwrapped per the judgment call above. Record the
audited list in the task evidence.

#### Step 4: Add the `getWallets` call-boundary case

In `tests/jest/security/governance-sanitization.spec.ts`, beside the existing
module-scope `jest.mock` of `delegateVotes` (the file's established pattern —
factories may `require`, but must not close over a module-scope const), add:

```ts
jest.mock(
  '../../../source/renderer/app/api/wallets/requests/getWallets',
  () => ({
    getWallets: jest.fn(async () => [
      // eslint-disable-next-line global-require
      require('../../mocks/wallets/wallet-voting-drep.json'),
    ]),
  })
);

jest.mock(
  '../../../source/renderer/app/api/wallets/requests/getLegacyWallets',
  () => ({ getLegacyWallets: jest.fn(async () => []) })
);
```

The fixture is the checksum-verified task-126 one, so its `voting` value is
the canonical CIP-129 key vector. Then add this case to the existing
`describe('Governance sanitization — call boundaries', …)` block, modelled on
the `AdaApi::delegateVotes` case above it:

```ts
  it('redacts the vote target from the AdaApi wallet-list poll log', async () => {
    const FIXTURE_DREP =
      'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
    (global as any).environment = {
      ...(global as any).environment,
      isSelfnode: false,
    };
    (global as any).https = require('https');
    (global as any).daedalus = {
      api: {
        localStorage: {
          getHardwareWalletsLocalData: jest.fn(async () => ({})),
        },
      },
    };

    const loggerSpy = jest
      .spyOn(rendererLogger, 'debug')
      .mockImplementation(() => undefined);
    // eslint-disable-next-line global-require
    const AdaApi = require('../../../source/renderer/app/api/api').default;
    const api = new AdaApi(false, {} as any);

    await api.getWallets();

    const getWalletsLog = loggerSpy.mock.calls.find(
      ([message]) => message === 'AdaApi::getWallets success'
    );
    expect(getWalletsLog).toBeDefined();
    const payload = JSON.stringify(getWalletsLog?.[1]);
    expect(payload).not.toContain(FIXTURE_DREP);
    expect(payload).not.toContain('abstain');
    expect(payload).not.toContain('no_confidence');
  });
```

`global.daedalus.api.localStorage.getHardwareWalletsLocalData` must be stubbed
because `getWallets` destructures it at `api.ts:369` and then indexes the
result by wallet id (`api.ts:401`), so an object — not `undefined` — is
required. If the run reports the fixture id rather than a redaction failure,
re-read the fixture: it must be the committed
`tests/mocks/wallets/wallet-voting-drep.json`, not a hand-written copy.

#### Step 5: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn lint
yarn test:jest tests/jest/security/governance-sanitization.spec.ts --runInBand
node_modules/.bin/jest --runInBand   # whole tree: all 86 suites stay green
grep -n "logger.debug('AdaApi::getWallets success'" -A3 source/renderer/app/api/api.ts
grep -n "logger.debug('AdaApi::getWallet success'" source/renderer/app/api/api.ts
```

Do NOT run prettier on `api.ts` or on the sanitization spec (both
pre-existing) — match the surrounding style shown in the blocks above.

### Acceptance

- [ ] `wallets` and `legacyWallets` at the `AdaApi::getWallets success` call
      site pass through `filterLogData`; `hwLocalData` is still filtered
      (AC-1 — Step 1).
- [ ] The `AdaApi::getWallet success` call site gets the same treatment on
      the single `wallet` object (AC-2 — Step 2).
- [ ] Every remaining whole-payload `logger.*` site in `api.ts` that can
      carry `delegation.*.voting` is audited, the six Shelley sites are
      wrapped, and the audit list is in the task evidence (AC-3 — Step 3).
- [ ] `tests/jest/security/governance-sanitization.spec.ts` gains a
      `getWallets` call-boundary case driving a voting-wallet fixture through
      `AdaApi.getWallets` and asserting no bech32 DRep id and no
      `abstain` / `no_confidence` literal reaches the emitted payload (AC-4 —
      Step 4).
- [ ] INHERITED sanitization floor: the full governance-sanitization suite is
      green with the new case, and no non-governance log shape for the
      wallet-list flow changes beyond the deliberate `passphrase` omission.
- [ ] `tsc` clean; lint clean; the UNFILTERED whole-tree jest run is green
      (86 suites at cv-1 close).

---

## task-171: Restore the ja-JP `!!!` markers and guard them

**Files touched:**

- `source/renderer/app/i18n/locales/ja-JP.json` (edit — nineteen VALUES gain
  a leading `!!!`; nothing else changes)
- `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` (new — creates
  `tests/jest/i18n/`)
- `source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx`
  (edit — three exact-text ja-JP `getByText` assertions gain the same `!!!`
  prefix; they cannot match once the marker is restored, so the whole-tree
  gate below is unreachable without this)

**Context.** Invariant 11 binds BOTH locales, and slice-1's ja-JP copy landed
without the marker. Measured at HEAD by diffing the two catalogs: exactly 20
keys are present in both files with an en-US value starting `!!!` and a ja-JP
value that does not. Nineteen are this feature's — the seventeen
`governance.drepDirectory.*` keys, `governance.tabs.directory`, and
`sidebar.categoryTooltip.governance`. The twentieth,
`wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement`,
predates the feature and its ja-JP copy has already been reviewed; it is the
guard's single allow-list entry.

This task is ordered before task-135 because the guard is what makes the rule
enforceable for every later mint — task-135's twelve keys here, task-146's in
cv-2, and anchor-2's copy after that. A guard landing after the mints
protects nothing.

**Locked invariants (inline).**

- This task RESTORES markers and never strips one. Removing `!!!` is the
  release-end manual copy review and is user-owned; no en-US value and no
  already-reviewed ja-JP value is touched here.
- Only the nineteen values listed below change. No key is added, removed,
  renamed or reordered — the catalog stays key-sorted exactly as the i18n
  runner left it.
- `defaultMessages.json` and `translations/messages.json` are NOT edited: the
  `!!!` in those files comes from each component's source `defaultMessage`,
  which is untouched, so `yarn i18n:manage` must leave them byte-identical.

**Resolved judgment calls (do not revisit):**

- The guard lives at `tests/jest/i18n/preliminaryCopyMarkers.spec.ts`, a new
  directory alongside the existing `tests/jest/{api,governance,security}`
  grouping; jest picks it up through `roots` with no config change
  (`jest.config.js:129`).
- The guard is asymmetric on purpose: it fires only when the en-US value
  starts with `!!!`. Once the release-end review clears an en-US marker the
  assertion goes silent for that key on its own, so the allow-list needs no
  maintenance and stays at its one pre-existing entry.
- Keys present in only one catalog are out of scope — there are none at HEAD,
  and the missing-key case is already the i18n runner's job.

### Step-by-Step

#### Step 1: Restore the nineteen ja-JP markers

In `source/renderer/app/i18n/locales/ja-JP.json`, prefix `!!!` to the VALUE of
exactly these keys, leaving the key, its position, and the rest of the string
untouched:

```
governance.drepDirectory.copyButton
governance.drepDirectory.copyId
governance.drepDirectory.empty
governance.drepDirectory.error
governance.drepDirectory.lastUpdated
governance.drepDirectory.loading
governance.drepDirectory.pagination.next
governance.drepDirectory.pagination.pageInfo
governance.drepDirectory.pagination.previous
governance.drepDirectory.refresh
governance.drepDirectory.refreshing
governance.drepDirectory.retry
governance.drepDirectory.source.onChain
governance.drepDirectory.status.active
governance.drepDirectory.status.inactive
governance.drepDirectory.title
governance.drepDirectory.votingPower
governance.tabs.directory
sidebar.categoryTooltip.governance
```

Do NOT run prettier on the locale files — they are tool-managed by the i18n
runner and a reformat produces a whole-file diff.

#### Step 2: Create the guard

```bash
mkdir -p tests/jest/i18n
```

Create `tests/jest/i18n/preliminaryCopyMarkers.spec.ts` with exactly:

```ts
import enUS from '../../../source/renderer/app/i18n/locales/en-US.json';
import jaJP from '../../../source/renderer/app/i18n/locales/ja-JP.json';

// Copy that is still preliminary carries a leading `!!!` in every locale
// until the release-end review clears it. This one key's ja-JP copy was
// reviewed before the rule existed, so its en-US marker outlives its ja-JP
// one; it is the only permitted asymmetry.
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const en: Record<string, string> = enUS;
const ja: Record<string, string> = jaJP;

describe('preliminary copy markers', () => {
  it('keeps the ja-JP !!! marker on every key whose en-US copy is still preliminary', () => {
    const unmarked = Object.keys(en).filter(
      (key) =>
        key in ja &&
        en[key].startsWith('!!!') &&
        !ja[key].startsWith('!!!') &&
        !REVIEWED_JA_JP_EXCEPTIONS.includes(key)
    );
    expect(unmarked).toEqual([]);
  });
});
```

(`resolveJsonModule` is on — `tsconfig.json:38` — and catalog JSON is already
imported from specs, e.g. `VotingPowerDelegationConfirmationDialog.spec.tsx:9`.
The failure message lists the offending keys, which is the point: a reviewer
should see WHICH key reopened the gap.)

#### Step 3: Verify

```bash
yarn compile   # Node v24 fallback: node_modules/.bin/tsc --noEmit
yarn test:jest tests/jest/i18n/preliminaryCopyMarkers.spec.ts --runInBand
yarn i18n:manage        # expected DIRTY here: the 12 voting.governance.currentVote.*
                        # keys are already missing at HEAD and are task-135's
                        # deliverable, so the clean run is gated there, not here
git diff --stat         # ja-JP.json, the new spec, and DRepDirectory.spec.tsx only
git diff --stat source/renderer/app/i18n/locales/defaultMessages.json translations/messages.json \
  || echo "OK: generated catalogs untouched"
node_modules/.bin/prettier --check tests/jest/i18n/preliminaryCopyMarkers.spec.ts
node_modules/.bin/jest --runInBand   # whole tree: all 86 suites stay green
```

Then prove the guard bites rather than merely passing: temporarily strip the
`!!!` from one restored ja-JP value, re-run the focused spec, confirm it fails
and names that key, and put the marker back.

### Acceptance

- [ ] All nineteen feature-introduced keys carry a leading `!!!` in
      `ja-JP.json` — the eighteen `governance.*` keys plus
      `sidebar.categoryTooltip.governance` (AC-1 — Step 1).
- [ ] The Jest guard asserts, for every key in both catalogs whose en-US
      value starts with `!!!`, that the ja-JP value does too, with an
      allow-list holding only
      `wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement`
      (AC-2 — Step 2).
- [ ] The guard demonstrably FAILS on a newly unmarked ja-JP counterpart, so
      task-135, task-146 and anchor-2 copy cannot silently reopen the gap
      (AC-3 — Step 3).
- [ ] `yarn i18n:manage` runs clean and `defaultMessages.json` /
      `translations/messages.json` are unchanged by the restoration (AC-4 —
      only the ja-JP translation file is edited).
- [ ] The task restores markers only and never strips one; removal stays the
      user-owned release-end copy review (AC-5).
- [ ] `tsc` clean; the UNFILTERED whole-tree jest run is green (86 suites
      at cv-1 close).

---

## Cross-Cutting Acceptance (All Tasks)

After all twelve tasks are complete, run from the worktree root:

```bash
yarn compile          # Zero TS errors (Node v24 fallback: node_modules/.bin/tsc --noEmit)
node_modules/.bin/typed-scss-modules source/renderer/app   # scss typings regenerate cleanly
yarn lint             # Zero ESLint errors (covers source, storybook, utils)
yarn test:jest        # Whole tree green: all 86 suites — tests/ plus colocated source specs
yarn test:jest tests/jest/security/governance-sanitization.spec.ts --runInBand   # floor re-asserted (invariant 2)
yarn i18n:manage      # Clean re-run: no missing/deleted keys
node_modules/.bin/prettier --check \
  "tests/mocks/wallets/*.json" \
  tests/jest/api/walletDelegationStatuses.spec.ts \
  tests/jest/api/createWalletFromServerData.spec.ts \
  tests/jest/api/walletVotingComputeds.spec.ts \
  source/renderer/app/utils/governance/normalizeDRepIdentity.ts \
  tests/jest/governance/normalizeDRepIdentity.spec.ts \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.messages.ts \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.scss \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx \
  source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.spec.tsx \
  storybook/stories/governance/CurrentVoteSummary.stories.tsx \
  tests/jest/i18n/preliminaryCopyMarkers.spec.ts
```

(Prettier runs ONLY on the files cv-1 created — never `yarn prettier:check`
repo-wide and never on pre-existing files; the repo carries pre-existing
formatting drift.)

Grep floors — all must hold:

```bash
grep -rn "voting_and_delegating" source tests storybook || echo "OK"   # task-127 AC: zero stale literals
grep -rn "interface DRepIdentity" source | wc -l                       # exactly 1 (governance.types.ts — never redefined)
grep -n "votingTarget" source/renderer/app/domains/Wallet.ts | wc -l   # exactly 4 (props, field, pick list, computed)
grep -n "GovernanceStore\|drepIndex\|DRepStatusBadge" source/renderer/app/components/voting/voting-governance/CurrentVoteSummary.tsx || echo "OK"
git diff package.json || echo "OK: no new dependency"                  # task-129 AC-7
grep -rn "task-1[0-9][0-9]" source tests storybook || echo "OK"        # task-131 Step 4: no task ids in code
```

(The ja-JP `!!!` floor is not a grep here — task-171's Jest guard is the
durable check and it runs inside the whole-tree gate above.)

Manual verification (this devcontainer has no running cardano-wallet and no
network — plan D-5 — so the manual pass is Storybook-scoped):

- [ ] Storybook "Governance / Current Vote Summary → Core states": all four
      knob values render in en-US AND ja-JP via the global toggle, no
      console errors, no overflow, no missing-message warnings after
      task-135.
- [ ] The DRep knob state shows: "Delegated to DRep" label, on-chain source
      label, truncated id with full-id tooltip/copy — and NO status badge,
      NO name, NO links.
- [ ] The noDelegation state shows warning + subline + CTA and never
      collapses.
- [ ] All four fixture files decode: every bech32 string passes the task-126
      one-liners.
- [ ] `VotingPowerDelegation.tsx`, `VotingStore.ts`, `routes-config.ts`
      byte-identical (`git diff --stat` shows no hits outside the cv-1 file
      list).
- [ ] Commits: one subject-only Conventional Commits line per task, explicit
      paths staged (never `git add -A`).

Slice close-out: fill the PRD "Final Outcome" placeholder, append the
per-task `Code Review:` entries and the closing `Planner:` entry to
`cv-1-code-review.md`, and re-verify plan :152 still reads task-127 (F-1).

## References

- PRD: [cv-1-PRD.md](./cv-1-PRD.md) (decisions D-1…D-10, verbatim ACs,
  Definition of Done)
- Code-review log: [cv-1-code-review.md](./cv-1-code-review.md)
- Findings: `../research/cv-1-findings.md` (F-1 attribution, F-2 counts,
  F-3 test naming)
- Parent plan: `../governance-drep-discovery-plan.md` (:152-159 Key
  Decisions; :174/:196 fixture pin; :253-259 delegation integration;
  :284/:295 Track V boundary)
- Task tracker: `../governance-drep-discovery-plan-tasks.json` :804-995
  (cv-1 phase — authoritative ACs)
- Design: `../designs/current-vote-display-design.md` (:14 renderer-only;
  :74-99 shapes; :110/:122-146 mapper; :151-166 normalizer; :170-187
  component)
- UX: `../designs/current-vote-display-ux.md` (:31 IA; :54-105 states;
  :154-186 i18n inventory; :188-197 accessibility; :199-211 knob spec)
- Research: `../research/external-research.md` (:49-53 sentinels; :52
  key-or-script DReps; :57-65 CIP-119 deferral)
- Live-code anchors (verified at `b900b99b3`, pre-implementation — line
  numbers shift as tasks land; re-anchor by quoted content):
  `source/renderer/app/domains/Wallet.ts:33-43,112-132,161-164,172-174,177-201,239-247`;
  `source/renderer/app/api/wallets/types.ts:31-55,80-84,105-114`;
  `source/renderer/app/api/api.ts:3010-3112`;
  `source/common/types/governance.types.ts:20-31`;
  `source/renderer/app/components/governance/_shared/DRepIdDisplay.tsx:28-32`;
  `source/renderer/app/components/governance/_shared/DRepSourceLabel.tsx:18-24`;
  `source/renderer/app/api/utils/request.ts:20-21`;
  `jest.config.js:63-66,129,156,203`; `storybook/main.ts:8`;
  `storybook/stories/index.ts:17`; `declaration.d.ts:11`;
  `package.json:21,43,45,48,52-55,73`;
  `tests/jest/governance/GovernanceStore.spec.ts:23-32`;
  `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx:30-55`.
