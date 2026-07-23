# Slice-3 Implementation Guide: Hardware-Wallet Delegate

> **Companion PRD:** [slice-3-PRD.md](./slice-3-PRD.md) | **Task Tracker:** [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json)
>
> All line anchors below were verified against the live worktree
> `/home/node/daedalus.worktrees/slice-3-hw-delegate` (branch
> `feat/drep-discovery-slice-3`, base `883ffae09`) on 2026-07-23. Re-verify an anchor
> only if an earlier step of this same guide already touched the file.

---

## Implementation Order

Single task: **task-115**. Execute the steps below in order — Step 1 (the production
fix) must land before Step 5 (the floor-suite additions), because those tests fail
against the unfixed catch blocks by design.

## Cross-Cutting Notes (apply to every step)

- **Locked invariants, inline:**
  - **#2 sanitization floor** — no DRep id, no `abstain`/`no_confidence` literal, no
    CIP-129/CIP-105 bech32 string in any `logger.*`, `analytics.sendEvent`, or
    electron-store payload. This slice FIXES the two `VotingStore` HW catch blocks that
    violate it and adds spy proof. The floor suite must end at **20 tests, never below
    17**.
  - **#4 no second delegation backend** — production flow code is untouched except the
    two catch blocks + one comment removal. Tests must prove
    `delegateVotesRequest.execute` / `api.ada.delegateVotes` are NEVER invoked for a
    hardware wallet.
  - **#10 byte-equality** — `vote: chosenOption` reaches the device-bound certificate
    verbatim (assert with `toBe`, not just `toEqual`); the device credential equals
    `Cardano.DRepID.toCredential(Cardano.DRepID(chosenOption))` for CIP-129 and CIP-105,
    key-hash and script-hash forms; the confirmation dialog renders the byte-equal raw
    ID in the HW flow.
  - **#11 preliminary copy** — this slice adds NO copy. Do not touch messages files or
    locale JSONs.
  - **#13 form-only sentinels** — `abstain`/`no_confidence` map to device
    `ABSTAIN`/`NO_CONFIDENCE` types, never to a DRep credential.
- **Jest is 27.5.1**: there is NO `jest.advanceTimersByTimeAsync`. Use the exact
  fake-timer patterns given below. Config quirks that matter: `clearMocks: true` (mock
  state auto-clears between tests), `testEnvironment: jsdom`, `globals.environment =
  { network: {} }` (so `utils/logging.ts` imports safely), and
  `transformIgnorePatterns` leaves `node_modules` untransformed (reason for the
  `@trezor/device-authenticity` stub in Step 3).
- **Assertion style**: never `toHaveBeenCalledWith('str', { literal: 'object' })` —
  always `expect.objectContaining({ … })` for object arguments (prettier 2.1.2
  oscillates otherwise). Never write inline `import { type X }` — use a separate
  `import type` statement.
- **Comments**: 1–3 plain why-lines only. No task IDs, no slice IDs, no ALL-CAPS tags,
  no change history.
- **Verification commands** (from the worktree root) — full set in Step 8.
- **Commit**: exactly ONE for the whole task, subject only (Step 9).

**Files touched (complete list — nothing else):**

| # | File | Action |
|---|---|---|
| 1 | `source/renderer/app/stores/VotingStore.ts` | EDIT (Step 1: comment removal + two catch blocks) |
| 2 | `source/renderer/app/utils/shelleyLedger.spec.ts` | CREATE (Step 2) |
| 3 | `source/renderer/app/utils/shelleyTrezor.spec.ts` | CREATE (Step 3) |
| 4 | `source/renderer/app/stores/VotingStore.spec.ts` | CREATE (Step 4) |
| 5 | `tests/jest/security/governance-sanitization.spec.ts` | EDIT (Step 5: +3 tests, header trim) |
| 6 | `source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx` | EDIT (Step 6: +9 tests) |
| 7 | `source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx` | EDIT (Step 7: +3 tests, comment strip) |

Do NOT touch `HardwareWalletsStore.ts`, `shelleyLedger.ts`, `shelleyTrezor.ts`, any
component/container/messages/locale file, `routes-config.ts`, the tracker JSON, or
Storybook.

**Shared test vectors** (generated with `Cardano.DRepID.cip129FromCredential` /
`cip105FromCredential` from the fixed hashes and round-trip verified via
`Cardano.DRepID.toCredential` — see findings F-4; copy them literally):

```ts
const KEY_HASH_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_HASH_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
const CIP129_KEY =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const CIP129_SCRIPT =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const CIP105_KEY =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const CIP105_SCRIPT =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';
```

---

## Step 1: Sanitize the two HW error logs and remove the stale TODO (`VotingStore.ts`)

Three surgical edits. The rest of the file stays byte-identical.

### 1a. Remove the stale TODO comment

At `VotingStore.ts:370`, delete this single line (the HW branch below it is fully
implemented — the comment is stale):

```ts
    // TODO: handle HW case
```

### 1b. `initializeVPDelegationTx` catch (`:347-358` pre-edit)

Replace exactly this block:

```ts
    } catch (error) {
      logger.error(
        'VotingStore: error while initializing VP delegation TX with HW',
        {
          error,
        }
      );
      return {
        success: false,
        errorCode: parseApiCode(expectedInitializeVPDelegationTxErrors, error),
      };
    }
```

with:

```ts
    } catch (error) {
      const errorCode = parseApiCode(
        expectedInitializeVPDelegationTxErrors,
        error
      );
      // Device and API error messages can embed the vote target; log only the
      // derived code so no DRep id or sentinel ever reaches the log file.
      logger.error(
        'VotingStore: error while initializing VP delegation TX with HW',
        {
          errorCode,
        }
      );
      return {
        success: false,
        errorCode,
      };
    }
```

### 1c. `delegateVotes` HW catch (`:403-412` pre-edit; after 1a the block sits one line higher)

Replace exactly this block:

```ts
      } catch (error) {
        logger.error('VotingStore: error while delegating vote with HW', {
          error,
        });
        const errorCode: GenericErrorCode = 'generic';
        return {
          success: false,
          errorCode,
        };
      }
```

with (note the optional catch binding — the error value must not be logged and is
otherwise unused):

```ts
      } catch {
        const errorCode: GenericErrorCode = 'generic';
        // Device and API error messages can embed the vote target; log only the
        // derived code so no DRep id or sentinel ever reaches the log file.
        logger.error('VotingStore: error while delegating vote with HW', {
          errorCode,
        });
        return {
          success: false,
          errorCode,
        };
      }
```

Behavior is unchanged (same return values); only the log payload shrinks to the derived
code.

---

## Step 2: Create `source/renderer/app/utils/shelleyLedger.spec.ts` (7 tests)

Full file content:

```ts
import { DRepParamsType } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { Cardano } from '@cardano-sdk/core';
import { toLedgerCertificate } from './shelleyLedger';
import type { CoinSelectionCertificate } from '../api/transactions/types';

// Vectors generated from the fixed credential hashes below via
// Cardano.DRepID.cip129FromCredential / cip105FromCredential and verified
// round-trip with Cardano.DRepID.toCredential.
const KEY_HASH_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_HASH_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
const CIP129_KEY =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const CIP129_SCRIPT =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const CIP105_KEY =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const CIP105_SCRIPT =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';

const castVote = (vote: string): CoinSelectionCertificate =>
  ({
    certificateType: 'cast_vote',
    rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    vote,
  } as CoinSelectionCertificate);

// The on-device identity is the credential the device derives from the vote
// string; it must be byte-equal to the credential decoded from chosenOption.
const decodedHash = (vote: string): string =>
  Cardano.DRepID.toCredential(Cardano.DRepID(vote)).hash;

describe('shelleyLedger cast_vote certificate mapping', () => {
  it('binds a CIP-129 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP129_KEY));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP129_KEY));
  });

  it('binds a CIP-129 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP129_SCRIPT));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.SCRIPT_HASH,
      scriptHashHex: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP129_SCRIPT));
  });

  it('binds a CIP-105 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP105_KEY));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.KEY_HASH,
      keyHashHex: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP105_KEY));
  });

  it('binds a CIP-105 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toLedgerCertificate(castVote(CIP105_SCRIPT));
    expect(result.params.dRep).toEqual({
      type: DRepParamsType.SCRIPT_HASH,
      scriptHashHex: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP105_SCRIPT));
  });

  it('maps the abstain sentinel to the device ABSTAIN type', () => {
    expect(toLedgerCertificate(castVote('abstain')).params.dRep).toEqual({
      type: DRepParamsType.ABSTAIN,
    });
  });

  it('maps the no_confidence sentinel to the device NO_CONFIDENCE type', () => {
    expect(toLedgerCertificate(castVote('no_confidence')).params.dRep).toEqual({
      type: DRepParamsType.NO_CONFIDENCE,
    });
  });

  it('leaves dRep undefined for non-vote certificates', () => {
    const result = toLedgerCertificate({
      certificateType: 'register_reward_account',
      rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    } as CoinSelectionCertificate);
    expect(result.params.dRep).toBeUndefined();
  });
});
```

Seams under test (do not modify them): `parseVoteDelegation` guard at
`shelleyLedger.ts:56` (`certificateType !== 'cast_vote' || !('vote' in cert)` →
`undefined`), sentinel branches `:59-69`, credential derivation `:71`
(`Cardano.DRepID.toCredential(Cardano.DRepID(cert.vote))`), script/key branches
`:73-83`; `toLedgerCertificate` puts the result under `params.dRep` at `:97`.

---

## Step 3: Create `source/renderer/app/utils/shelleyTrezor.spec.ts` (7 tests)

The `jest.mock` line MUST be the first statement (before all imports — Jest hoists it,
but keeping it first documents why the file loads at all).

Full file content:

```ts
// @trezor/connect transitively pulls an ESM-only @noble/curves build that
// Jest cannot parse; the authenticity module is irrelevant to certificate
// mapping, so stub it to keep the real PROTO enums importable.
jest.mock('@trezor/device-authenticity', () => ({}));

import { PROTO } from '@trezor/connect';
import { Cardano } from '@cardano-sdk/core';
import { toTrezorCertificate } from './shelleyTrezor';
import type { CoinSelectionCertificate } from '../api/transactions/types';

// Same verified vectors as the Ledger spec (round-trip proven via
// Cardano.DRepID.toCredential).
const KEY_HASH_HEX =
  'a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c';
const SCRIPT_HASH_HEX =
  '0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4';
const CIP129_KEY =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const CIP129_SCRIPT =
  'drep1yv83utfufddxj7y8j6jmfs7ju8cq783d839456tcs7t2tdq508myt';
const CIP105_KEY =
  'drep15xev84897cr3s2f6fdwx6l50jzsm9s75uhmqwxpf8f94cymwqu9';
const CIP105_SCRIPT =
  'drep_script1pu0z60zttf5h3puk5k6v85hp7q83utfufddxj7y8j6jmg6wjkzc';

const castVote = (vote: string): CoinSelectionCertificate =>
  ({
    certificateType: 'cast_vote',
    rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    vote,
  } as CoinSelectionCertificate);

const decodedHash = (vote: string): string =>
  Cardano.DRepID.toCredential(Cardano.DRepID(vote)).hash;

describe('shelleyTrezor cast_vote certificate mapping', () => {
  it('binds a CIP-129 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP129_KEY)) as {
      type: number;
      dRep?: { type: number; keyHash?: string; scriptHash?: string };
    };
    expect(result.type).toBe(PROTO.CardanoCertificateType.VOTE_DELEGATION);
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.KEY_HASH,
      keyHash: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP129_KEY));
  });

  it('binds a CIP-129 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP129_SCRIPT)) as {
      dRep?: { type: number; scriptHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.SCRIPT_HASH,
      scriptHash: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP129_SCRIPT));
  });

  it('binds a CIP-105 key-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP105_KEY)) as {
      dRep?: { type: number; keyHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.KEY_HASH,
      keyHash: KEY_HASH_HEX,
    });
    expect(KEY_HASH_HEX).toBe(decodedHash(CIP105_KEY));
  });

  it('binds a CIP-105 script-hash DRep ID byte-equal to vote.chosenOption', () => {
    const result = toTrezorCertificate(castVote(CIP105_SCRIPT)) as {
      dRep?: { type: number; scriptHash?: string };
    };
    expect(result.dRep).toEqual({
      type: PROTO.CardanoDRepType.SCRIPT_HASH,
      scriptHash: SCRIPT_HASH_HEX,
    });
    expect(SCRIPT_HASH_HEX).toBe(decodedHash(CIP105_SCRIPT));
  });

  it('maps the abstain sentinel to the device ABSTAIN type', () => {
    const result = toTrezorCertificate(castVote('abstain')) as {
      dRep?: { type: number };
    };
    expect(result.dRep).toEqual({ type: PROTO.CardanoDRepType.ABSTAIN });
  });

  it('maps the no_confidence sentinel to the device NO_CONFIDENCE type', () => {
    const result = toTrezorCertificate(castVote('no_confidence')) as {
      dRep?: { type: number };
    };
    expect(result.dRep).toEqual({ type: PROTO.CardanoDRepType.NO_CONFIDENCE });
  });

  it('carries no dRep for non-vote certificates', () => {
    const result = toTrezorCertificate({
      certificateType: 'register_reward_account',
      rewardAccountPath: ['1852H', '1815H', '0H', '2', '0'],
    } as CoinSelectionCertificate) as { dRep?: unknown };
    expect(result.dRep).toBeUndefined();
  });
});
```

Seams under test: `shelleyTrezor.ts:71` (same `Cardano.DRepID` derivation), `:95-101`
(`cast_vote` → `{ type: PROTO.CardanoCertificateType.VOTE_DELEGATION, path, dRep }`).
If TS complains about the `as` result shapes, loosen to `as any` on the
`toTrezorCertificate(...)` result — the runtime assertions carry the contract.

---

## Step 4: Create `source/renderer/app/stores/VotingStore.spec.ts` (6 tests)

Construction precedent: the sanitization suite builds
`new VotingStore(api, {} as any, analytics)` (Store constructor `(api, actions,
analytics)` — `Store.ts:13-17`) and `store.configure(stores)` (`Store.ts:25-27`)
attaches the stores map. `logger.error` MUST be stubbed in every test that reaches a
catch block: `utils/logging.ts` calls `electronLog[level]` which is undefined under
Jest.

Full file content:

```ts
import BigNumber from 'bignumber.js';
import VotingStore from './VotingStore';
import { logger } from '../utils/logging';
import { EventCategories } from '../analytics';

const CIP129_KEY =
  'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
const REWARD_ACCOUNT_PATH = ['1852H', '1815H', '0H', '2', '0'];

const hwWallet = {
  id: 'hw-wallet-1',
  isDelegating: false,
  isHardwareWallet: true,
} as any;

const softwareWallet = {
  id: 'sw-wallet-1',
  isDelegating: false,
  isHardwareWallet: false,
} as any;

const buildAnalytics = () => ({
  disableTracking: jest.fn(),
  enableTracking: jest.fn(),
  sendEvent: jest.fn(),
  sendPageNavigationEvent: jest.fn(),
});

const buildHardwareWallets = (overrides: Record<string, unknown> = {}) => ({
  selectDelegationCoins: jest.fn(async () => ({
    certificates: [],
    fee: new BigNumber('0.180989'),
  })),
  updateTxSignRequest: jest.fn(),
  initiateTransaction: jest.fn(async () => undefined),
  _sendMoney: jest.fn(async () => undefined),
  sendMoneyRequest: { isExecuting: false },
  isTransactionPending: false,
  ...overrides,
});

const buildStore = (hardwareWallets: ReturnType<typeof buildHardwareWallets>) => {
  const api = { ada: { delegateVotes: jest.fn() } };
  const analytics = buildAnalytics();
  const store = new VotingStore(api as any, {} as any, analytics as any);
  store.configure({
    hardwareWallets,
    staking: { stakePools: [{ id: 'pool-1' }] },
  } as any);
  return { analytics, api, store };
};

describe('VotingStore hardware-wallet delegation branches', () => {
  beforeEach(() => {
    // The renderer logger writes through global.electronLog, which does not
    // exist under Jest; stub it so error-path tests can run and be asserted.
    jest.spyOn(logger, 'error').mockImplementation(() => undefined);
  });

  afterEach(() => {
    jest.restoreAllMocks();
    jest.useRealTimers();
  });

  describe('initializeVPDelegationTx', () => {
    it('hands the signing layer a cast_vote certificate carrying chosenOption verbatim', async () => {
      const hardwareWallets = buildHardwareWallets();
      const { store } = buildStore(hardwareWallets);

      const result = await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: hwWallet,
      });

      expect(result).toEqual({ success: true, fees: expect.any(BigNumber) });
      expect(hardwareWallets.updateTxSignRequest).toHaveBeenCalledTimes(1);
      const [coinSelection] = hardwareWallets.updateTxSignRequest.mock.calls[0];
      expect(coinSelection.certificates).toEqual([
        {
          certificateType: 'cast_vote',
          rewardAccountPath: REWARD_ACCOUNT_PATH,
          vote: CIP129_KEY,
        },
      ]);
      // Byte-equality: the exact chosenOption string reaches the device-bound
      // certificate untouched.
      expect(coinSelection.certificates[0].vote).toBe(CIP129_KEY);
      expect(hardwareWallets.initiateTransaction).toHaveBeenCalledWith(
        expect.objectContaining({ walletId: hwWallet.id })
      );
      expect(
        hardwareWallets.updateTxSignRequest.mock.invocationCallOrder[0]
      ).toBeLessThan(
        hardwareWallets.initiateTransaction.mock.invocationCallOrder[0]
      );
    });

    it('prepends register_reward_account when the coin selection requires it', async () => {
      const hardwareWallets = buildHardwareWallets({
        selectDelegationCoins: jest.fn(async () => ({
          certificates: [{ certificateType: 'register_reward_account' }],
          fee: new BigNumber('0.2'),
        })),
      });
      const { store } = buildStore(hardwareWallets);

      await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: hwWallet,
      });

      const [coinSelection] = hardwareWallets.updateTxSignRequest.mock.calls[0];
      expect(coinSelection.certificates).toEqual([
        {
          certificateType: 'register_reward_account',
          rewardAccountPath: REWARD_ACCOUNT_PATH,
        },
        {
          certificateType: 'cast_vote',
          rewardAccountPath: REWARD_ACCOUNT_PATH,
          vote: CIP129_KEY,
        },
      ]);
    });

    it('leaves the hardware signing seams untouched for software wallets', async () => {
      const hardwareWallets = buildHardwareWallets();
      const { store } = buildStore(hardwareWallets);

      const result = await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: softwareWallet,
      });

      expect(result).toEqual({ success: true, fees: expect.any(BigNumber) });
      expect(hardwareWallets.updateTxSignRequest).not.toHaveBeenCalled();
      expect(hardwareWallets.initiateTransaction).not.toHaveBeenCalled();
    });

    it('returns a generic error code when the device is not connected', async () => {
      const hardwareWallets = buildHardwareWallets({
        initiateTransaction: jest.fn(() => {
          throw new Error('Wallet not paired or Device not connected');
        }),
      });
      const { store } = buildStore(hardwareWallets);

      const result = await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: hwWallet,
      });

      expect(result).toEqual({ success: false, errorCode: 'generic' });
      expect(logger.error).toHaveBeenCalledWith(
        'VotingStore: error while initializing VP delegation TX with HW',
        expect.objectContaining({ errorCode: 'generic' })
      );
    });
  });

  describe('delegateVotes', () => {
    it('submits through the HW path and never invokes the software delegateVotes request', async () => {
      jest.useFakeTimers();
      const hardwareWallets = buildHardwareWallets({
        sendMoneyRequest: { isExecuting: true },
      });
      const { analytics, api, store } = buildStore(hardwareWallets);
      const executeSpy = jest.spyOn(store.delegateVotesRequest, 'execute');

      const resultPromise = store.delegateVotes({
        chosenOption: CIP129_KEY,
        passphrase: '',
        wallet: hwWallet,
      });

      // Flush microtasks so _sendMoney resolves and the 2s polling timer arms.
      await Promise.resolve();
      await Promise.resolve();
      await Promise.resolve();
      jest.advanceTimersByTime(2000);
      hardwareWallets.sendMoneyRequest.isExecuting = false;
      jest.advanceTimersByTime(2000);

      const result = await resultPromise;
      expect(result).toEqual({ success: true });
      expect(hardwareWallets._sendMoney).toHaveBeenCalledWith(
        expect.objectContaining({ selectedWalletId: hwWallet.id })
      );
      expect(executeSpy).not.toHaveBeenCalled();
      expect(api.ada.delegateVotes).not.toHaveBeenCalled();
      expect(analytics.sendEvent).toHaveBeenCalledWith(
        EventCategories.VOTING,
        'Casted governance vote',
        'drep'
      );
    });

    it('returns a generic error code and sends no analytics when HW submission fails', async () => {
      const hardwareWallets = buildHardwareWallets({
        _sendMoney: jest.fn(async () => {
          throw new Error('signing rejected on device');
        }),
      });
      const { analytics, store } = buildStore(hardwareWallets);

      const result = await store.delegateVotes({
        chosenOption: CIP129_KEY,
        passphrase: '',
        wallet: hwWallet,
      });

      expect(result).toEqual({ success: false, errorCode: 'generic' });
      expect(analytics.sendEvent).not.toHaveBeenCalled();
      expect(logger.error).toHaveBeenCalledWith(
        'VotingStore: error while delegating vote with HW',
        expect.objectContaining({ errorCode: 'generic' })
      );
    });
  });
});
```

Notes for the implementer:
- Seams exercised: HW branch `VotingStore.ts:309-341` (certificate build `:310-316`,
  prepend `:318-330`, `updateTxSignRequest` `:337`, `initiateTransaction` `:338-340`),
  `delegateVotes` HW branch `:370-412` post-edit (`_sendMoney` `:372-374`, 2-second
  polling loop `:376-391`, analytics `:393-397`).
- The fake-timer choreography is deliberate: even if the first
  `advanceTimersByTime(2000)` fires before the timer arms, the second tick observes
  `isExecuting === false` and resolves — the test cannot hang. If the test still
  flakes, the sanctioned adaptation is adding one more `await Promise.resolve()` to
  the microtask flush — do not restructure the choreography.
- `expect.any(BigNumber)` matches the mocked `fee`; do not assert an exact value (it is
  a mock artifact, not a contract).

---

## Step 5: Grow the sanitization floor suite 17 → 20 (`tests/jest/security/governance-sanitization.spec.ts`)

### 5a. Header trim (comment convention — no task IDs)

Line 2, replace:

```ts
 * Governance sanitization regression tests (task-111).
```

with:

```ts
 * Governance sanitization regression tests.
```

### 5b. Append three tests INSIDE the existing `describe('Governance sanitization — call boundaries', …)` block

Insert after the closing of the existing
`it('sends only the sanitized drepOption analytics field for governance votes', …)`
test (currently ending at `:256`), before the describe's closing `});`. The describe's
existing `afterEach(() => { jest.restoreAllMocks(); })` covers the new tests; the
imports already present (`BigNumber`, `rendererLogger`, `VotingStore`,
`EventCategories`) are sufficient — add none.

```ts
  it('keeps DRep IDs and sentinel literals out of logger payloads when HW VP-delegation initialization fails', async () => {
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const store = new VotingStore({ ada: {} } as any, {} as any, {
      sendEvent: jest.fn(),
    } as any);
    store.configure({
      hardwareWallets: {
        selectDelegationCoins: jest.fn(async () => ({
          certificates: [],
          fee: new BigNumber('0.2'),
        })),
        updateTxSignRequest: jest.fn(),
        // Adversarial error: embeds the vote target the way an uncontrolled
        // device or API message could.
        initiateTransaction: jest.fn(() => {
          throw new Error(
            `Wallet not paired for ${CIP129_DREP} after abstain and no_confidence checks`
          );
        }),
      },
      staking: { stakePools: [{ id: 'pool-1' }] },
    } as any);

    const result = await store.initializeVPDelegationTx({
      chosenOption: CIP129_DREP,
      wallet: {
        id: 'wallet-1',
        isDelegating: false,
        isHardwareWallet: true,
      } as any,
    });

    expect(result).toEqual({ success: false, errorCode: 'generic' });
    expect(errorSpy).toHaveBeenCalled();
    const logged = JSON.stringify(errorSpy.mock.calls);
    expect(logged).not.toContain(CIP129_DREP);
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain('no_confidence');
  });

  it('keeps DRep IDs and sentinel literals out of logger payloads when HW vote submission fails', async () => {
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const store = new VotingStore({ ada: {} } as any, {} as any, {
      sendEvent: jest.fn(),
    } as any);
    store.configure({
      hardwareWallets: {
        _sendMoney: jest.fn(async () => {
          throw new Error(
            `submission failed for ${CIP129_DREP} after abstain and no_confidence checks`
          );
        }),
        sendMoneyRequest: { isExecuting: false },
        isTransactionPending: false,
      },
    } as any);

    const result = await store.delegateVotes({
      chosenOption: CIP129_DREP,
      passphrase: '',
      wallet: { id: 'wallet-1', isHardwareWallet: true } as any,
    });

    expect(result).toEqual({ success: false, errorCode: 'generic' });
    expect(errorSpy).toHaveBeenCalled();
    const logged = JSON.stringify(errorSpy.mock.calls);
    expect(logged).not.toContain(CIP129_DREP);
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain('no_confidence');
  });

  it('sends only the sanitized vote-kind analytics field for HW governance votes', async () => {
    jest.useFakeTimers();
    try {
      const analytics = {
        disableTracking: jest.fn(),
        enableTracking: jest.fn(),
        sendEvent: jest.fn(),
        sendPageNavigationEvent: jest.fn(),
      };
      const store = new VotingStore(
        { ada: { delegateVotes: jest.fn() } } as any,
        {} as any,
        analytics as any
      );
      store.configure({
        hardwareWallets: {
          _sendMoney: jest.fn(async () => undefined),
          sendMoneyRequest: { isExecuting: false },
          isTransactionPending: false,
        },
      } as any);

      const resultPromise = store.delegateVotes({
        chosenOption: CIP129_DREP,
        passphrase: '',
        wallet: { id: 'wallet-1', isHardwareWallet: true } as any,
      });

      // Flush microtasks so the 2s polling timer arms, then run its one tick.
      await Promise.resolve();
      await Promise.resolve();
      await Promise.resolve();
      jest.advanceTimersByTime(2000);

      const result = await resultPromise;
      expect(result).toEqual({ success: true });
      expect(analytics.sendEvent).toHaveBeenCalledWith(
        EventCategories.VOTING,
        'Casted governance vote',
        'drep'
      );
      expect(analytics.sendEvent.mock.calls[0]).toHaveLength(3);
      expect(JSON.stringify(analytics.sendEvent.mock.calls)).not.toContain(
        CIP129_DREP
      );
    } finally {
      jest.useRealTimers();
    }
  });
```

Notes:
- `CIP129_DREP` is the suite's existing top-level vector (`:26-27`) — reuse it; the
  store never decodes it on these paths, so its non-canonical padding is irrelevant.
- These tests FAIL against the pre-Step-1 code (the raw `{ error }` payload contains
  the ID) — that is the point; they prove the fix.
- Suite count: 15 (filterLogData) + 2 (existing boundaries) + 3 (new) = **20**.

---

## Step 6: Extend the dialog spec (+9 tests) (`VotingPowerDelegationConfirmationDialog.spec.tsx`)

### 6a. Add the HW wallet fixture

After the `softwareWallet` constant (`:18-22`), add:

```ts
const hardwareWallet = {
  id: 'hw-wallet-1',
  isHardwareWallet: true,
  name: 'HW Test Wallet',
} as any;
```

### 6b. Append a new describe block after the existing one (`:50-93`)

The expected texts below are the **live en-US values** (read from `en-US.json` during
planning — do not "fix" them to the `!!!defaultMessage` variants). The dialog renders
`HardwareWalletStatus` (`VotingPowerDelegationConfirmationDialog.tsx:179-185`) for HW
wallets and gates Confirm at `:141-147` on `VERIFYING_TRANSACTION_SUCCEEDED`.

```ts
describe('VotingPowerDelegationConfirmationDialog — hardware-wallet device states', () => {
  afterEach(cleanup);

  // The AC device states map onto the real HwDeviceStatuses: disconnected and
  // locked surface as CONNECTING/CONNECTING_FAILED (PIN-unlock copy),
  // app-not-open as LAUNCHING_CARDANO_APP, signing-rejected as
  // VERIFYING_TRANSACTION_FAILED, Trezor invalid-state as UNRECOGNIZED_WALLET.
  it.each([
    [
      HwDeviceStatuses.CONNECTING,
      'Connect the "HW Test Wallet" device and enter your PIN to unlock it',
    ],
    [
      HwDeviceStatuses.CONNECTING_FAILED,
      'Disconnect and reconnect your hardware wallet to restart the process.',
    ],
    [
      HwDeviceStatuses.LAUNCHING_CARDANO_APP,
      'Launch Cardano application on your device',
    ],
    [
      HwDeviceStatuses.VERIFYING_TRANSACTION,
      'Confirm the transaction using the "HW Test Wallet" device',
    ],
    [
      HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED,
      'Transaction confirmation failed',
    ],
    [
      HwDeviceStatuses.UNRECOGNIZED_WALLET,
      'We do not recognize this wallet on your device. Please ensure that you are using the same device that you selected for pairing "HW Test Wallet" and that you have entered the correct passphrase.',
    ],
  ])('renders the %s device state', (hwDeviceStatus, expectedText) => {
    renderDialog({ hwDeviceStatus, selectedWallet: hardwareWallet });
    expect(screen.getByText(expectedText)).toBeInTheDocument();
  });

  it('shows the Trezor passphrase hint while the device verifies the transaction', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      isTrezor: true,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByText('Enter passphrase if needed')).toBeInTheDocument();
  });

  it('shows the byte-equal DRep ID and no passphrase input on the hardware-wallet confirmation', () => {
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
    expect(document.querySelector('input[type="password"]')).toBeNull();
  });

  it('enables Confirm only after the device reports signing success', () => {
    const { unmount } = renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      selectedWallet: hardwareWallet,
    });
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
    unmount();
    renderDialog({
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      selectedWallet: hardwareWallet,
    });
    expect(
      screen.getByRole('button', { name: 'Confirm' })
    ).not.toBeDisabled();
  });
});
```

Notes:
- `renderDialog` (`:24-48`) already spreads overrides — no helper changes needed.
- `HwDeviceStatuses` is already imported (`:12`).
- react-polymorph's `ButtonSkin` forwards `disabled` to the DOM `<button>` via
  `pickDOMProps`, so `toBeDisabled()` works. If it ever fails, fall back to asserting
  the button's `disabled` attribute directly — do not change the component.
- File count after this step: 4 + 9 = **13 tests** (the `it.each` has 6 parameter
  rows, so Jest reports 6 tests from it, plus the 3 further `it`s).

---

## Step 7: Extend the flow spec (+3 tests) and strip task-ID comments (`VotingGovernancePage.spec.tsx`)

### 7a. Comment strips (slice-2 review rider — plain why-comments, no task/slice IDs)

1. Replace the block at `:64-67`:

```ts
// Test-only stand-in for the slice-4 Detail route: it forwards the inherited
// { from, selectedWalletId, voteType } plus the route's DRep ID back to the
// form, exactly as task-117's acceptance criteria specify. It is registered
// ONLY in this harness — production has no Detail route in this slice.
```

with:

```ts
// Test-only stand-in for the future DRep detail route: it forwards the
// inherited { from, selectedWalletId, voteType } plus the route's DRep ID
// back to the form. It is registered only in this harness — production has
// no detail route yet.
```

2. Replace the comment at `:210-211`:

```ts
    // Simulate the slice-4 "View details" push: the Directory forwards its
    // inherited state toward the detail path via the production picker.
```

with:

```ts
    // Simulate the future "View details" push: the Directory forwards its
    // inherited state toward the detail path via the production picker.
```

3. Replace the comment at `:244`:

```ts
    // The confirmation renders the selected ID itself (task-113), byte-equal.
```

with:

```ts
    // The confirmation renders the selected ID itself, byte-equal.
```

### 7b. Imports

Line 24 currently reads:

```ts
import { HwDeviceStatuses } from '../../domains/Wallet';
```

Add directly below it (separate `import type` statement — prettier 2.1.2 cannot parse
inline type specifiers):

```ts
import type { HwDeviceStatus } from '../../domains/Wallet';
```

### 7c. HW wallet fixture

After the `softwareWallet` constant (`:48-52`), add:

```ts
const HW_WALLET_ID = 'hw-wallet-1';

const hardwareWallet = {
  id: HW_WALLET_ID,
  name: 'HW Flow Wallet',
  isHardwareWallet: true,
} as any;
```

### 7d. Parametrize `buildStores` and `renderFlow`

Replace the current `const buildStores = () => ({` opener (`:89`) with:

```ts
type StoreOverrides = {
  hwDeviceStatus?: HwDeviceStatus;
  isTrezor?: boolean;
  wallets?: any[];
};

const buildStores = ({
  hwDeviceStatus = HwDeviceStatuses.READY,
  isTrezor = false,
  wallets = [softwareWallet],
}: StoreOverrides = {}) => ({
```

Inside the returned object, replace the `hardwareWallets` block (`:101-104`):

```ts
  hardwareWallets: {
    checkIsTrezorByWalletId: jest.fn(() => false),
    hwDeviceStatus: HwDeviceStatuses.READY,
  },
```

with:

```ts
  hardwareWallets: {
    checkIsTrezorByWalletId: jest.fn(() => isTrezor),
    hwDeviceStatus,
  },
```

and replace `wallets: { all: [softwareWallet] },` (`:114`) with:

```ts
  wallets: { all: wallets },
```

Then widen `renderFlow` (`:119-122`): replace

```ts
const renderFlow = (initialEntries: InitialEntry[]) => {
  const history = createMemoryHistory({ initialEntries });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = buildStores();
```

with:

```ts
const renderFlow = (
  initialEntries: InitialEntry[],
  storeOverrides: StoreOverrides = {}
) => {
  const history = createMemoryHistory({ initialEntries });
  const pushSpy = jest.spyOn(history, 'push');
  const stores = buildStores(storeOverrides);
```

All four existing tests keep their behavior (defaults reproduce the old stores).

### 7e. Append a new describe block after the existing one

```ts
describe('Hardware-wallet delegate flow via location.state handoff', () => {
  afterEach(() => {
    cleanup();
    jest.restoreAllMocks();
  });

  const hwEntry = {
    pathname: ROUTES.GOVERNANCE.DREPS,
    state: {
      from: ROUTES.VOTING.GOVERNANCE,
      selectedWalletId: HW_WALLET_ID,
      voteType: 'drep',
    },
  };

  it('propagates the selected DRep ID byte-for-byte into the HW signing payload (Ledger)', async () => {
    const { stores } = renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getByText(VALID_DREP_ID).textContent).toBe(VALID_DREP_ID);
    // The HW confirmation collects no passphrase: signing happened on-device.
    expect(document.querySelector('input[type="password"]')).toBeNull();
    expect(stores.voting.initializeVPDelegationTx).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        wallet: expect.objectContaining({
          id: HW_WALLET_ID,
          isHardwareWallet: true,
        }),
      })
    );

    fireEvent.click(screen.getByRole('button', { name: 'Confirm' }));

    await waitFor(() =>
      expect(stores.voting.delegateVotes).toHaveBeenCalledTimes(1)
    );
    expect(stores.voting.delegateVotes).toHaveBeenCalledWith(
      expect.objectContaining({
        chosenOption: VALID_DREP_ID,
        passphrase: '',
        wallet: expect.objectContaining({ id: HW_WALLET_ID }),
      })
    );
  });

  it('keeps Confirm disabled until the device reports signing success', async () => {
    renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(screen.getByRole('button', { name: 'Confirm' })).toBeDisabled();
  });

  it('applies the Trezor status treatment for Trezor devices', async () => {
    const { stores } = renderFlow([hwEntry], {
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION,
      isTrezor: true,
      wallets: [hardwareWallet],
    });

    fireEvent.click(
      screen.getByRole('button', { name: '!!!Select for delegation' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

    await screen.findByText('Confirm Transaction');
    expect(
      stores.hardwareWallets.checkIsTrezorByWalletId
    ).toHaveBeenCalledWith(HW_WALLET_ID);
    expect(screen.getByText('Enter passphrase if needed')).toBeInTheDocument();
  });
});
```

Notes:
- The container seams exercised: `VotingGovernancePage.tsx:60`
  (`initiateTransaction={voting.initializeVPDelegationTx}`), `:89-92`
  (`hwDeviceStatus` + `checkIsTrezorByWalletId`), `:95-101` (`onSubmit` →
  `delegateVotes`). The form's Submit path has no HW-specific gating
  (`VotingPowerDelegation.tsx:139-143`), so the software-path harness carries over.
- File count after this step: 4 + 3 = **7 tests**.

---

## Step 8: Verification (run all, report honestly)

From the worktree root:

```bash
# 1. Typecheck — MUST exit 0 with zero errors (do not use `yarn compile`)
node_modules/.bin/tsc --noEmit

# 2. Lint the touched files
node_modules/.bin/eslint \
  source/renderer/app/stores/VotingStore.ts \
  source/renderer/app/stores/VotingStore.spec.ts \
  source/renderer/app/utils/shelleyLedger.spec.ts \
  source/renderer/app/utils/shelleyTrezor.spec.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  tests/jest/security/governance-sanitization.spec.ts

# 3. Focused Jest — expected counts per suite:
#    shelleyLedger 7 · shelleyTrezor 7 · VotingStore 6 · dialog 13 · flow 7
yarn test:jest \
  source/renderer/app/utils/shelleyLedger.spec.ts \
  source/renderer/app/utils/shelleyTrezor.spec.ts \
  source/renderer/app/stores/VotingStore.spec.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx

# 4. Inherited sanitization floor — MUST report 20 passed (grew from 17)
yarn test:jest tests/jest/security/governance-sanitization.spec.ts

# 5. Format ONLY the changed .ts/.tsx files (never any JSON)
node_modules/.bin/prettier --write \
  source/renderer/app/stores/VotingStore.ts \
  source/renderer/app/stores/VotingStore.spec.ts \
  source/renderer/app/utils/shelleyLedger.spec.ts \
  source/renderer/app/utils/shelleyTrezor.spec.ts \
  source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog.spec.tsx \
  source/renderer/app/containers/voting/VotingGovernancePage.spec.tsx \
  tests/jest/security/governance-sanitization.spec.ts
```

**AC ↔ test mapping (all must hold):**

| AC | Proof |
|---|---|
| AC-1 selector → confirmation → signing payload | Flow test "propagates the selected DRep ID byte-for-byte…" + `VotingStore` certificate test |
| AC-2 identity display + device states | Dialog `it.each` (6 states incl. disconnected/locked → `CONNECTING`/`CONNECTING_FAILED`, app-not-open → `LAUNCHING_CARDANO_APP`, rejected → `VERIFYING_TRANSACTION_FAILED`, Trezor invalid-state → `UNRECOGNIZED_WALLET`) + Trezor hint + HW identity/no-password + gating tests |
| AC-3 no software-signing fallback | `VotingStore` HW submit test (`executeSpy` + `api.ada.delegateVotes` never called) |
| AC-4 browse → select → confirm-on-device → delegate, mocked Ledger + Trezor | Flow HW tests (Ledger + Trezor treatments) + both mapper suites (the real device-library payloads) |
| AC-5 on-device DRep ID = `vote.chosenOption` (byte-equality) | Mapper suites (device credential === `Cardano.DRepID.toCredential(chosenOption)`, 4 ID forms + sentinels) + `toBe` verbatim-string assert in the `VotingStore` spec |
| AC-6 inherited sanitization floor | Floor suite at 20/20 incl. both adversarial HW error paths + HW vote-kind analytics; enabled by the Step-1 fix |

---

## Step 9: Tracker + docs + commit

1. **Tracker** (`governance-drep-discovery-plan-tasks.json`, task-115 block at ~:416-433):
   set `status` to `complete` (NEVER `verified` — that requires dedicated proof beyond
   in-task tests, realistically task-125), a truthful prose `statusReason` naming the
   gates run, the test counts, and the review outcome (style precedent: task-112/113/114
   entries), `evidence` as an array of the seven touched file paths, and
   `updatedAt: "2026-07-23"`. Edit with targeted string replacement — never reformat
   the JSON.
2. **Findings**: append the implementation outcome to
   `research/slice-3-findings.md` under "Implementation findings" (at minimum: floor
   count 17 → 20, whether any adaptation from this guide was needed).
3. **PRD**: fill the Final Outcome section of `slice-3-PRD.md` at slice close.
4. **Commit** — exactly one, subject only, no body, no trailers:

```
feat(gov): task-115 harden and verify the hardware-wallet delegate path
```
