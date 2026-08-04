/**
 * Governance sanitization regression tests.
 *
 * Asserts that no DRep ID, CIP-129/CIP-105 bech32 string, anchor URL or verified
 * anchor content reaches any logger call, and that no vote target reaches an
 * analytics payload. The derived vote kind is a sanctioned analytics dimension;
 * the vote target never is.
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

jest.mock('matomo-tracker', () =>
  jest.fn().mockImplementation(() => ({ track: jest.fn() }))
);

import { filterLogData } from '../../../source/common/utils/logging';
import BigNumber from 'bignumber.js';
import { logger as rendererLogger } from '../../../source/renderer/app/utils/logging';
import VotingStore from '../../../source/renderer/app/stores/VotingStore';
import { EventCategories } from '../../../source/renderer/app/analytics';
import MatomoTracker from 'matomo-tracker';
import { maskAnalyticsRoute } from '../../../source/renderer/app/analytics/maskAnalyticsRoute';
import { MatomoClient } from '../../../source/renderer/app/analytics/MatomoClient';
import walletVotingDRepFixture from '../../mocks/wallets/wallet-voting-drep.json';
import https from 'https';
import dns from 'dns';
import { EventEmitter } from 'events';
import { logger as mainLogger } from '../../../source/main/utils/logging';
import { fetchAnchorBytes } from '../../../source/main/governance/AnchorFetchService';

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

// Error message/stack are non-enumerable and invisible to JSON.stringify;
// expand them so a DRep ID embedded in an error message cannot slip past
// the containment assertions below.
function jsonStrWithErrors(value: unknown): string {
  return JSON.stringify(value, (_key, val) =>
    val instanceof Error ? `${val.message} ${val.stack}` : val
  );
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

  it('redacts the vote target from a full wallet-list wire payload', () => {
    const result = jsonStr(filterLogData(walletVotingDRepFixture));
    expect(result).not.toContain(
      'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy'
    );
    expect(result).toContain('cv1 fixture voting drep');
  });

  // --- Governance identity and anchor redaction ---

  it('removes a nested drepIdentity object entirely', () => {
    const data = {
      context: {
        drepIdentity: {
          raw: CIP129_DREP,
          cip129: CIP129_DREP,
          cip105: CIP105_KEY,
          credentialHex: 'a1b2',
        },
      },
    };
    const s = jsonStr(filterLogData(data));
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).not.toContain(CIP105_SCRIPT);
    expect(s).not.toContain('a1b2');
  });

  it('removes the standalone raw, cip129, cip105 and credentialHex keys', () => {
    const data = {
      raw: CIP129_DREP,
      cip129: CIP129_DREP,
      cip105: CIP105_SCRIPT,
      credentialHex: 'a1b2',
    };
    const result = filterLogData(data);
    expect(Object.keys(result)).toHaveLength(0);
  });

  it('removes a currentVote object carrying a vote kind and a drep id', () => {
    const data = { currentVote: { voteKind: 'abstain', drepId: CIP129_DREP } };
    const s = jsonStr(filterLogData(data));
    expect(s).not.toContain('abstain');
    expect(s).not.toContain(CIP129_DREP);
  });

  it('removes votingTarget', () => {
    const data = { votingTarget: CIP129_DREP };
    const result = filterLogData(data);
    expect(Object.keys(result)).toHaveLength(0);
    expect(jsonStr(result)).not.toContain(CIP129_DREP);
  });

  it('removes chosenOption', () => {
    const data = { chosenOption: 'no_confidence' };
    const s = jsonStr(filterLogData(data));
    expect(s).not.toContain('no_confidence');
  });

  it('removes anchorUrl', () => {
    const data = { anchorUrl: 'https://anchor.example.org/profile.jsonld' };
    const s = jsonStr(filterLogData(data));
    expect(s).not.toContain('anchor.example.org');
  });

  it('removes givenName, verifiedName and a nested anchorContent object', () => {
    const data = {
      givenName: 'Sample DRep',
      verifiedName: 'Sample DRep',
      anchorContent: { givenName: 'Sample DRep' },
    };
    const s = jsonStr(filterLogData(data));
    expect(s).not.toContain('Sample DRep');
  });

  it('removes every CIP-119 profile field name added by the anchor pipeline', () => {
    const data = {
      objectives: 'Objectives prose',
      motivations: 'Motivations prose',
      qualifications: 'Qualifications prose',
      references: [{ type: 'identity', uri: 'https://example.org/id' }],
      paymentAddress: 'addr1qxexamplepaymentaddressvalue',
      doNotList: true,
    };

    expect(filterLogData(data)).toEqual({});
  });

  it('removes CIP-119 profile fields nested under a verified anchor content object', () => {
    const address = 'addr1qxexamplepaymentaddressvalue';
    const data = {
      anchorState: {
        state: 'verified',
        content: { paymentAddress: address, objectives: 'Objectives prose' },
      },
    };

    expect(jsonStr(filterLogData(data))).not.toContain(address);
    expect(jsonStr(filterLogData(data))).not.toContain('Objectives prose');
  });

  it('retains a sensitive-looking value under a key that is not on the list', () => {
    // sensitiveData.includes(key) at source/common/utils/logging.ts:74 is exact
    // string equality, so the filter redacts by key name and never by value
    // shape.
    const data = { note: CIP129_DREP };
    const s = jsonStr(filterLogData(data));
    expect(s).toContain(CIP129_DREP);
  });

  it('carries no bech32 string in the DRepIdDisplay clipboard-unavailable payload', () => {
    const payload = { drepIdLength: CIP129_DREP.length };
    const s = jsonStr(filterLogData(payload));
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).not.toContain(CIP105_SCRIPT);
  });

  it('carries no bech32 string in the DRepIdDisplay copy-failure payload', () => {
    const payload = {
      error: new Error('clipboard write denied'),
      drepIdLength: CIP105_KEY.length,
    };
    const s = jsonStrWithErrors(filterLogData(payload));
    expect(s).not.toContain(CIP129_DREP);
    expect(s).not.toContain(CIP105_KEY);
    expect(s).not.toContain(CIP105_SCRIPT);
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

  it('keeps DRep IDs and sentinel literals out of logger payloads when HW VP-delegation initialization fails', async () => {
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const store = new VotingStore(
      { ada: {} } as any,
      {} as any,
      {
        sendEvent: jest.fn(),
      } as any
    );
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
    const logged = jsonStrWithErrors(errorSpy.mock.calls);
    expect(logged).not.toContain(CIP129_DREP);
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain('no_confidence');
  });

  it('keeps DRep IDs and sentinel literals out of logger payloads when HW vote submission fails', async () => {
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const store = new VotingStore(
      { ada: {} } as any,
      {} as any,
      {
        sendEvent: jest.fn(),
      } as any
    );
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
    const logged = jsonStrWithErrors(errorSpy.mock.calls);
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

  it('reduces a sentinel delegation to the vote kind in the analytics payload', async () => {
    const analytics = {
      disableTracking: jest.fn(),
      enableTracking: jest.fn(),
      sendEvent: jest.fn(),
      sendPageNavigationEvent: jest.fn(),
    };
    const errorSpy = jest
      .spyOn(rendererLogger, 'error')
      .mockImplementation(() => undefined);
    const debugSpy = jest
      .spyOn(rendererLogger, 'debug')
      .mockImplementation(() => undefined);
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
      chosenOption: 'abstain',
      passphrase: 'test-passphrase',
      wallet: {
        amount: new BigNumber('123000000'),
        id: 'wallet-1',
        isHardwareWallet: false,
      } as any,
    });

    // The third argument is the derived vote kind, not a delegation target.
    expect(analytics.sendEvent).toHaveBeenCalledWith(
      EventCategories.VOTING,
      'Casted governance vote',
      'abstain'
    );
    expect(analytics.sendEvent.mock.calls[0]).toHaveLength(3);
    const analyticsPayload = jsonStr(analytics.sendEvent.mock.calls);
    expect(analyticsPayload).not.toContain('drep1');
    expect(analyticsPayload).not.toContain('drep_vkh');
    expect(analyticsPayload).not.toContain('drep_script');

    const logged = jsonStrWithErrors([
      errorSpy.mock.calls,
      debugSpy.mock.calls,
    ]);
    expect(logged).not.toContain('abstain');
    expect(logged).not.toContain(CIP129_DREP);
  });
});

describe('Governance sanitization — analytics URL masking', () => {
  it('masks the DRep id out of the detail route for both CIP forms', () => {
    expect(maskAnalyticsRoute(`governance/dreps/${CIP129_DREP}`)).toBe(
      'governance/dreps/:drepId'
    );
    expect(maskAnalyticsRoute(`governance/dreps/${CIP105_SCRIPT}`)).toBe(
      'governance/dreps/:drepId'
    );
  });

  it('leaves non-detail routes untouched', () => {
    expect(maskAnalyticsRoute('governance/dreps')).toBe('governance/dreps');
    expect(maskAnalyticsRoute('voting/governance')).toBe('voting/governance');
    expect(maskAnalyticsRoute('wallets/add')).toBe('wallets/add');
  });

  it('never embeds the current detail-route DRep id in a tracked event URL', async () => {
    window.location.hash = `#/governance/dreps/${CIP129_DREP}`;
    try {
      const client = new MatomoClient(
        { isDev: true } as any,
        {} as any,
        'user-1'
      );
      await client.sendEvent('Governance', 'Test event');

      const tracker = (MatomoTracker as unknown as jest.Mock).mock.results[0]
        .value;
      expect(tracker.track).toHaveBeenCalledTimes(1);
      const { url } = tracker.track.mock.calls[0][0];
      expect(url).toBe('http://daedalus/governance/dreps/:drepId');
      expect(url).not.toContain(CIP129_DREP);
    } finally {
      window.location.hash = '';
    }
  });
});

describe('Governance sanitization — main-process anchor fetch', () => {
  const ANCHOR_URL = `https://anchor.example.org/${CIP129_DREP}.jsonld`;

  class FakeRequest extends EventEmitter {
    end = jest.fn();
    destroy = jest.fn();
  }
  class FakeResponse extends EventEmitter {
    statusCode = 200;
    headers: Record<string, string> = { 'content-type': 'text/html' };
    destroy = jest.fn();
  }

  it('keeps anchor URLs, hosts, DRep ids and raw errors out of anchor fetch logger payloads', async () => {
    const spies = [
      jest.spyOn(mainLogger, 'debug').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'info').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'warn').mockImplementation(() => undefined),
      jest.spyOn(mainLogger, 'error').mockImplementation(() => undefined),
    ];
    const requestSpy = jest.spyOn(https, 'request');
    const lookupSpy = jest.spyOn(dns.promises, 'lookup');
    const SENTINEL = 'sentinel-error-detail';

    // unsupported scheme, malformed input
    await fetchAnchorBytes(`ipfs://${CIP129_DREP}`);
    await fetchAnchorBytes('not a url');
    await fetchAnchorBytes(`http://anchor.example.org/${CIP105_KEY}.jsonld`);

    // dns failure carrying an error whose message names the host
    lookupSpy.mockRejectedValue(
      Object.assign(new Error(`${SENTINEL} anchor.example.org`), {
        code: 'ENOTFOUND',
      })
    );
    await fetchAnchorBytes(ANCHOR_URL);

    // blocked address
    lookupSpy.mockResolvedValue([
      { address: '169.254.169.254', family: 4 },
    ] as any);
    await fetchAnchorBytes(ANCHOR_URL);

    lookupSpy.mockResolvedValue([
      { address: '93.184.216.34', family: 4 },
    ] as any);

    // transport error
    const failingRequest = new FakeRequest();
    requestSpy.mockImplementation(((): any => {
      process.nextTick(() =>
        failingRequest.emit(
          'error',
          Object.assign(new Error(`${SENTINEL} ${ANCHOR_URL}`), {
            code: 'CERT_HAS_EXPIRED',
          })
        )
      );
      return failingRequest;
    }) as any);
    await fetchAnchorBytes(ANCHOR_URL);

    // redirect, rejected content type, oversized body
    const drive = (response: FakeResponse, after?: () => void) => {
      const request = new FakeRequest();
      requestSpy.mockImplementation(((_options: any, callback: any): any => {
        process.nextTick(() => {
          callback(response);
          if (after) process.nextTick(after);
        });
        return request;
      }) as any);
    };

    const redirect = new FakeResponse();
    redirect.statusCode = 302;
    redirect.headers = { location: `https://evil.example.net/${CIP129_DREP}` };
    drive(redirect);
    await fetchAnchorBytes(ANCHOR_URL);

    const wrongType = new FakeResponse();
    drive(wrongType);
    await fetchAnchorBytes(ANCHOR_URL);

    const oversized = new FakeResponse();
    oversized.headers = {
      'content-type': 'application/json',
      'content-length': String(2 * 1024 * 1024),
    };
    drive(oversized);
    await fetchAnchorBytes(ANCHOR_URL);

    const logged = jsonStrWithErrors(spies.map((spy) => spy.mock.calls));
    expect(logged).not.toContain(ANCHOR_URL);
    expect(logged).not.toContain('anchor.example.org');
    expect(logged).not.toContain('evil.example.net');
    expect(logged).not.toContain('93.184.216.34');
    expect(logged).not.toContain('169.254.169.254');
    expect(logged).not.toContain(CIP129_DREP);
    expect(logged).not.toContain(CIP105_KEY);
    expect(logged).not.toContain('drep1');
    expect(logged).not.toContain('drep_vkh');
    expect(logged).not.toContain(SENTINEL);
    expect(logged).toContain('ANCHOR_TLS_FAILED');

    spies.forEach((spy) => spy.mockRestore());
    requestSpy.mockRestore();
    lookupSpy.mockRestore();
  });
});
