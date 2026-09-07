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
  submitConstructedNativeTransaction: jest.fn(async () => ({ id: 'tx-1' })),
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

  describe('delegateVotes', () => {
    it('submits the frozen vote through the hardware native approval path', async () => {
      const hardwareWallets = buildHardwareWallets();
      const { analytics, store } = buildStore(hardwareWallets);

      await expect(
        store.delegateVotes({
          chosenOption: CIP129_KEY,
          passphrase: '',
          wallet: hwWallet,
        })
      ).resolves.toEqual({ success: true });
      expect(
        hardwareWallets.submitConstructedNativeTransaction
      ).toHaveBeenCalledWith({
        walletId: hwWallet.id,
        action: 'drep-delegation',
        data: { encoding: 'base16', vote: CIP129_KEY },
      });
      expect(analytics.sendEvent).toHaveBeenCalledWith(
        EventCategories.VOTING,
        'Casted governance vote',
        'drep'
      );
    });

    it('returns a generic error code and sends no analytics when HW submission fails', async () => {
      const hardwareWallets = buildHardwareWallets({
        submitConstructedNativeTransaction: jest.fn(async () => {
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
      currentDRep: null,
      id: 'sw-wallet-2',
      isHardwareWallet: false,
      name: 'Form Wallet',
    } as any;
    const submitTransaction = jest.fn(async () => ({
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
            submitTransaction,
            onBrowseDRepsClick: jest.fn(),
            onExternalLinkClick: jest.fn(),
            onCancel: jest.fn(),
            onSuccess: jest.fn(),
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
        'This wallet already delegates to this choice. Please change delegation in order to proceed.'
      )
    ).toBeInTheDocument();
    expect(submitTransaction).toHaveBeenCalledWith(
      expect.objectContaining({ chosenOption: CIP129_KEY })
    );
  });
});
