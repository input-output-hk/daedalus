import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import type { Api } from '../api/index';
import type { ActionsMap } from '../actions/index';
import VotingStore, {
  FundPhase,
  expectedInitializeVPDelegationTxErrors,
} from './VotingStore';
import type { CatalystFund } from '../api/voting/types';
import { EventCategories, noopAnalyticsTracker } from '../analytics';
import { logger } from '../utils/logging';
import ApiError from '../domains/ApiError';
import translations from '../i18n/locales/en-US.json';
import { daedalusTheme } from '../themes/daedalus';
import { themeOverrides } from '../themes/overrides';
import VotingPowerDelegation from '../components/voting/voting-governance/VotingPowerDelegation';

jest.mock('../components/widgets/forms/WalletsDropdown', () => {
  return function WalletsDropdownMock() {
    return null;
  };
});

jest.mock('../components/widgets/forms/ItemsDropdown', () => {
  return function ItemsDropdownMock() {
    return null;
  };
});

const mockFundInfo = {
  current: {
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
};

describe('VotingStore', () => {
  const api: Api = {
    ada: jest.fn(),
  } as any;
  const actions: ActionsMap = jest.fn() as any;

  const cases = [
    [undefined, null],
    [
      new Date(mockFundInfo.current.registrationSnapshotTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.registrationSnapshotTime, FundPhase.SNAPSHOT],
    [
      new Date(mockFundInfo.current.startTime.getTime() - 60000),
      FundPhase.SNAPSHOT,
    ],
    [mockFundInfo.current.startTime, FundPhase.VOTING],
    [
      new Date(mockFundInfo.current.endTime.getTime() - 60000),
      FundPhase.VOTING,
    ],
    [mockFundInfo.current.endTime, FundPhase.TALLYING],
    [
      new Date(mockFundInfo.current.resultsTime.getTime() - 60000),
      FundPhase.TALLYING,
    ],
    [mockFundInfo.current.resultsTime, FundPhase.RESULTS],
  ];
  const votingStore = new VotingStore(api, actions, noopAnalyticsTracker);

  beforeAll(() => {
    votingStore.catalystFund = mockFundInfo as CatalystFund;
  });

  test.each(cases)(
    `should have correct fund phase for date %s - %s phase`,
    (date: Date, expected: FundPhase) => {
      votingStore._checkFundPhase(date);
      expect(votingStore.fundPhase).toEqual(expected);
    }
  );
});

const CIP129_KEY = 'drep1y2sm9s75uhmqwxpf8f94cmt737g2rvkr6njlvpcc9yaykhq23nmjy';
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

const buildStore = (
  hardwareWallets: ReturnType<typeof buildHardwareWallets>
) => {
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

    it('surfaces the same_vote server error without logging the vote target', async () => {
      const hardwareWallets = buildHardwareWallets({
        selectDelegationCoins: jest.fn(async () => {
          throw new ApiError({ code: 'same_vote' } as any);
        }),
      });
      const { store } = buildStore(hardwareWallets);

      const result = await store.initializeVPDelegationTx({
        chosenOption: CIP129_KEY,
        wallet: softwareWallet,
      });

      expect(expectedInitializeVPDelegationTxErrors).toContain('same_vote');
      expect(result).toEqual({ success: false, errorCode: 'same_vote' });
      expect(logger.error).toHaveBeenCalledWith(
        'VotingStore: error while initializing VP delegation TX with HW',
        expect.objectContaining({ errorCode: 'same_vote' })
      );

      // Re-spying returns the mock the outer beforeEach installed, so the
      // recorded calls are the ones asserted above.
      const errorSpy = jest.spyOn(logger, 'error');
      expect(JSON.stringify(errorSpy.mock.calls)).not.toContain(CIP129_KEY);
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

const DelegationForm = (VotingPowerDelegation as unknown) as React.ComponentType<
  any
>;

describe('same-vote server error in the delegation form', () => {
  afterEach(cleanup);

  it('renders the server same_vote copy when the wallet has no matching current vote', async () => {
    const wallet = {
      currentVote: null,
      id: 'sw-wallet-2',
      isHardwareWallet: false,
      name: 'Form Wallet',
    } as any;
    const initiateTransaction = jest.fn(async () => ({
      errorCode: 'same_vote' as const,
      success: false as const,
    }));

    render(
      React.createElement(
        ThemeProvider,
        {
          theme: daedalusTheme,
          skins: SimpleSkins,
          variables: SimpleDefaults,
          themeOverrides,
        },
        React.createElement(
          IntlProvider,
          { locale: 'en-US', messages: translations },
          React.createElement(DelegationForm, {
            getStakePoolById: jest.fn(),
            initialFormState: {
              selectedDRepId: CIP129_KEY,
              selectedWalletId: wallet.id,
              voteType: 'drep',
            },
            initiateTransaction,
            onBrowseDRepsClick: jest.fn(),
            onExternalLinkClick: jest.fn(),
            renderConfirmationDialog: () => null,
            stakePools: [],
            wallets: [wallet],
          })
        )
      )
    );

    const submit = screen.getByRole('button', { name: 'Submit' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);

    expect(
      await screen.findByText(
        'This voting power delegation choice has already been successfully recorded in a previous transaction. Please change the registration type or DRep ID in order to proceed.'
      )
    ).toBeInTheDocument();
    expect(initiateTransaction).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: CIP129_KEY })
    );
  });
});
