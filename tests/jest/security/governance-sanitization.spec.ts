/**
 * Governance sanitization regression tests (task-111).
 *
 * Asserts that no DRep ID, abstain/no_confidence literal, or CIP-129/CIP-105
 * bech32 string reaches any logger call or analytics payload.
 *
 * This is the sanitization floor established in slice-1. Every later slice
 * inherits this invariant as a non-negotiable acceptance check.
 */
jest.mock(
  '../../../source/renderer/app/api/voting/requests/delegateVotes',
  () => ({
    delegateVotes: jest.fn(() => Promise.resolve(Buffer.from('ok'))),
  })
);

import { filterLogData } from '../../../source/common/utils/logging';
import BigNumber from 'bignumber.js';
import { logger as rendererLogger } from '../../../source/renderer/app/utils/logging';
import VotingStore from '../../../source/renderer/app/stores/VotingStore';
import { EventCategories } from '../../../source/renderer/app/analytics';

// ---- Test vectors ----

/** CIP-129 DRep ID (drep1…). */
const CIP129_DREP =
  'drep1yg7shg8raj8f0q0ra0v6q5q3q6z8qkqz7q9q8q7q6q5q4q3q2q1q0qz7q9q8q7q6q5q4q3q2q1q0qz7q9q8';
/** CIP-105 key-hash DRep ID (drep_vkh1…). */
const CIP105_KEY =
  'drep_vkh1abc123def456ghi789jkl012mno345pqr678stu901vwx234yz';
/** CIP-105 script-hash DRep ID (drep_script1…). */
const CIP105_SCRIPT =
  'drep_script1abc123def456ghi789jkl012mno345pqr678stu901vwx234yz';

function jsonStr(value: unknown): string {
  return JSON.stringify(value);
}

describe('Governance sanitization — filterLogData', () => {
  // --- DRep ID redaction ---

  it('redacts CIP-129 drepId at the top level', () => {
    const data = { drepId: CIP129_DREP };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('redacts CIP-129 drepId nested under delegation.active.voting', () => {
    const data = {
      delegation: {
        active: { voting: CIP129_DREP },
      },
    };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('redacts CIP-129 drepId in next delegation array', () => {
    const data = {
      delegation: {
        next: [{ voting: CIP129_DREP }, { voting: CIP129_DREP }],
      },
    };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('redacts CIP-129 drepId inside certificates vote array', () => {
    const data = { certificates: [{ vote: CIP129_DREP }] };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('redacts CIP-129 drepId via the "vote" key', () => {
    const data = { vote: CIP129_DREP };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('redacts CIP-129 drepId via the "voting" key', () => {
    const data = { voting: CIP129_DREP };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  // --- CIP-105 redaction ---

  it('redacts CIP-105 key-hash DRep ID', () => {
    const data = { delegation: { active: { voting: CIP105_KEY } } };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP105_KEY);
  });

  it('redacts CIP-105 script-hash DRep ID', () => {
    const data = { delegation: { active: { voting: CIP105_SCRIPT } } };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain(CIP105_SCRIPT);
  });

  // --- Sentinel literal redaction ---

  it('redacts "abstain" sentinel via the voting key', () => {
    const data = { delegation: { active: { voting: 'abstain' } } };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain('abstain');
  });

  it('redacts "no_confidence" sentinel via the voting key', () => {
    const data = { delegation: { active: { voting: 'no_confidence' } } };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain('no_confidence');
  });

  it('redacts "abstain" sentinel via the vote key', () => {
    const data = { certificates: [{ vote: 'abstain' }] };
    const result = filterLogData(data);
    expect(jsonStr(result)).not.toContain('abstain');
  });

  // --- Deeply nested redaction ---

  it('redacts DRep IDs at arbitrary nesting depth', () => {
    const data = {
      a: {
        b: {
          c: {
            d: {
              drepId: CIP129_DREP,
              delegation: { active: { voting: CIP105_KEY } },
            },
          },
        },
      },
    };
    const result = filterLogData(data);
    const s = jsonStr(result);
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
  });

  // --- Non-governance keys are preserved ---

  it('preserves non-sensitive fields', () => {
    const data = {
      walletId: 'wallet-123',
      name: 'Test Wallet',
      amount: 1000000,
    };
    const result = filterLogData(data);
    expect(result).toEqual(data);
  });

  // --- Array redaction ---
  // The governance sanitization floor must redact sensitive keys recursively,
  // including nested objects inside arrays.

  it('redacts voting keys inside arrays of objects', () => {
    // This DOES work because omit-deep-lodash handles the vote/voting keys
    // inside array elements
    const data = {
      certificates: [{ vote: CIP129_DREP }, { vote: 'abstain' }],
    };
    const result = filterLogData(data);
    const s = jsonStr(result);
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain('abstain');
  });

  it('preserves array structures while redacting nested object keys', () => {
    const data = {
      items: [
        { drepId: CIP129_DREP, name: 'A' },
        { drepId: CIP105_KEY, name: 'B' },
      ],
    };
    const result = filterLogData(data);
    // Array structure is preserved
    expect((result as Record<string, unknown>).items).toBeDefined();
    const s = jsonStr(result);
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).toContain('A');
    expect(s).toContain('B');
  });
});

describe('Governance sanitization — call boundaries', () => {
  afterEach(() => {
    jest.restoreAllMocks();
  });

  it('redacts DRep IDs before logger payloads are emitted by AdaApi', async () => {
    (global as any).environment = {
      ...(global as any).environment,
      isSelfnode: false,
    };
    (global as any).https = require('https');

    const loggerSpy = jest
      .spyOn(rendererLogger, 'debug')
      .mockImplementation(() => undefined);
    // eslint-disable-next-line global-require
    const AdaApi = require('../../../source/renderer/app/api/api').default;
    const api = new AdaApi(false, {} as any);

    await api.delegateVotes({
      dRepId: CIP129_DREP,
      passphrase: 'test-passphrase',
      walletId: 'wallet-1',
    });

    const delegateVotesLog = loggerSpy.mock.calls.find(
      ([message]) => message === 'AdaApi::delegateVotes called'
    );

    expect(delegateVotesLog).toBeDefined();
    expect(JSON.stringify(delegateVotesLog?.[1])).not.toContain(CIP129_DREP);
    expect(JSON.stringify(delegateVotesLog?.[1])).not.toContain(
      'test-passphrase'
    );
  });

  it('sends only the sanitized drepOption analytics field for governance votes', async () => {
    const analytics = {
      disableTracking: jest.fn(),
      enableTracking: jest.fn(),
      sendEvent: jest.fn(),
      sendPageNavigationEvent: jest.fn(),
    };
    const store = new VotingStore(
      {
        ada: {
          delegateVotes: jest.fn(() => Promise.resolve(Buffer.from('ok'))),
        },
      } as any,
      {} as any,
      analytics as any
    );

    await store.delegateVotes({
      chosenOption: CIP129_DREP,
      passphrase: 'test-passphrase',
      wallet: {
        amount: new BigNumber('123000000'),
        id: 'wallet-1',
        isHardwareWallet: false,
      } as any,
    });

    expect(analytics.sendEvent).toHaveBeenCalledWith(
      EventCategories.VOTING,
      'Casted governance vote',
      'drep'
    );
    expect(analytics.sendEvent.mock.calls[0]).toHaveLength(3);
  });
});
