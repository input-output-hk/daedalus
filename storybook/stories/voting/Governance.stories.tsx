import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import {
  withKnobs,
  boolean,
  number,
  select,
  text,
} from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import { find } from 'lodash';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import Navigation from '../../../source/renderer/app/components/navigation/Navigation';
import VotingPowerDelegation from '../../../source/renderer/app/components/voting/voting-governance/VotingPowerDelegation';
import VotingPowerDelegationConfirmationDialog from '../../../source/renderer/app/components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
import VotingUnavailable from '../../../source/renderer/app/components/voting/VotingUnavailable';
import VotingInfo from '../../../source/renderer/app/components/voting/voting-info/VotingInfo';
import { VotingFooterLinks } from '../../../source/renderer/app/components/voting/VotingFooterLinks';
import BorderedBox from '../../../source/renderer/app/components/widgets/BorderedBox';
import Sidebar from '../../../source/renderer/app/components/sidebar/Sidebar';
import type { SidebarMenus } from '../../../source/renderer/app/components/sidebar/types';
import SidebarLayout from '../../../source/renderer/app/components/layout/SidebarLayout';
import TopBar from '../../../source/renderer/app/components/layout/TopBar';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import {
  CATEGORIES_BY_NAME,
  SidebarCategoryInfo,
} from '../../../source/renderer/app/config/sidebarConfig';
import {
  DATE_ENGLISH_OPTIONS,
  LANGUAGE_OPTIONS,
  TIME_OPTIONS,
} from '../../../source/renderer/app/config/profileConfig';
import Wallet, {
  HwDeviceStatus,
  HwDeviceStatuses,
  WalletSyncStateStatuses,
} from '../../../source/renderer/app/domains/Wallet';
import type StakePool from '../../../source/renderer/app/domains/StakePool';
import type { CatalystFund } from '../../../source/renderer/app/api/voting/types';
import { TESTNET } from '../../../source/common/types/environment.types';
import type { Locale } from '../../../source/common/types/locales.types';
import { ROUTES } from '../../../source/renderer/app/routes-config';
import { FundPhase } from '../../../source/renderer/app/stores/VotingStore';
import type {
  DelegateVotesError,
  InitializeVPDelegationTxError,
} from '../../../source/renderer/app/stores/VotingStore';
import { generateWallet } from '../_support/utils';

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

const GOVERNANCE_WALLETS = [
  generateWallet(
    'Governance wallet',
    '125000000000',
    undefined,
    0,
    null,
    true,
    WalletSyncStateStatuses.READY,
    false,
    'governance-wallet-1'
  ),
  generateWallet(
    'Ledger governance wallet',
    '58000000000',
    undefined,
    0,
    null,
    false,
    WalletSyncStateStatuses.READY,
    true,
    'governance-wallet-2'
  ),
  generateWallet(
    'Syncing wallet',
    '42000000000',
    undefined,
    0,
    null,
    true,
    WalletSyncStateStatuses.SYNCING,
    false,
    'governance-wallet-3'
  ),
];

const voteOptions = {
  'Delegate to DRep': VALID_DREP_ID,
  Abstain: 'abstain',
  'No confidence': 'no_confidence',
};

const initializeTxErrorOptions: Record<string, InitializeVPDelegationTxError> =
  {
    Generic: 'generic',
    'Same vote': 'same_vote',
    'No UTxOs available': 'no_utxos_available',
    'Not enough money': 'not_enough_money',
  };

const delegateVotesErrorOptions: Record<string, DelegateVotesError> = {
  Generic: 'generic',
  'Wrong spending password': 'wrong_encryption_passphrase',
};

const hwDeviceStatusOptions = {
  Connecting: HwDeviceStatuses.CONNECTING,
  Verifying: HwDeviceStatuses.VERIFYING_TRANSACTION,
  Verified: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
  Failed: HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED,
};

const STAKE_POOLS_LIST = STAKE_POOLS as unknown as Array<StakePool>;

const mockFundInfo: CatalystFund = {
  current: {
    number: 7,
    startTime: new Date('Jan 20, 2022, 11:00 UTC'),
    endTime: new Date('Feb 3, 2022, 11:00 UTC'),
    resultsTime: new Date('Feb 10, 2022'),
    registrationSnapshotTime: new Date('Jan 6, 2022, 11:00 UTC'),
  },
  next: {
    number: 8,
    startTime: new Date('Apr 6, 2022, 11:00 UTC'),
    registrationSnapshotTime: new Date('Apr 7, 2022, 11:00 UTC'),
  },
};

const CENTERED_STORY_STYLE = {
  margin: '0 auto',
  maxWidth: 820,
};

const CONNECTED_FLOW_STYLE = {
  height: 780,
};

const FLOW_CONTENT_STYLE = {
  padding: 32,
};

const FLOW_SECTION_STYLE = {
  display: 'flex',
  flexDirection: 'column' as const,
  gap: 24,
};

const VOTING_NAV_ITEMS = [
  {
    id: ROUTES.VOTING.GOVERNANCE,
    label: 'Governance',
  },
  {
    id: ROUTES.VOTING.REGISTRATION,
    label: 'Catalyst Voting',
  },
];

const VOTING_SIDEBAR_CATEGORIES: Array<SidebarCategoryInfo> = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.VOTING,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

const EMPTY_SIDEBAR_MENUS: SidebarMenus = {
  wallets: null,
};

const CATALYST_PHASE_OPTIONS = {
  Snapshot: FundPhase.SNAPSHOT,
  Voting: FundPhase.VOTING,
  Tallying: FundPhase.TALLYING,
  Results: FundPhase.RESULTS,
};

const locale = LANGUAGE_OPTIONS[0].value as Locale;

const getStakePoolById = (stakePoolId: string): StakePool | undefined =>
  find(STAKE_POOLS_LIST, (stakePool) => stakePool.id === stakePoolId);

const governanceStoryDecorator = (story: () => React.ReactNode) => (
  <StoryProvider>
    <StoryDecorator>
      <div style={{ padding: 24 }}>{story()}</div>
    </StoryDecorator>
  </StoryProvider>
);

const renderGovernancePanel = () => {
  const transactionFee = new BigNumber(
    number('Initialized transaction fee', 0.174257, {
      min: 0,
      step: 0.000001,
    })
  );
  text('Valid DRep ID fixture', VALID_DREP_ID);

  return (
    <VotingPowerDelegation
      getStakePoolById={getStakePoolById}
      initiateTransaction={async (params) => {
        action('initiateTransaction')(params);
        return boolean('Initialization succeeds', true)
          ? { success: true, fees: transactionFee }
          : {
              success: false,
              errorCode: select(
                'Initialization error',
                initializeTxErrorOptions,
                'same_vote'
              ),
            };
      }}
      onExternalLinkClick={action('onExternalLinkClick')}
      renderConfirmationDialog={renderGovernanceConfirmationDialog}
      stakePools={STAKE_POOLS_LIST}
      wallets={GOVERNANCE_WALLETS}
    />
  );
};

const renderCatalystPanel = () => (
  <div style={FLOW_SECTION_STYLE}>
    <VotingInfo
      currentLocale={locale}
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
      fundPhase={
        select(
          'Catalyst phase',
          CATALYST_PHASE_OPTIONS,
          FundPhase.SNAPSHOT
        ) as FundPhase
      }
      fundInfo={mockFundInfo}
      onRegisterToVoteClick={action('onRegisterToVoteClick')}
      onExternalLinkClick={action('onExternalLinkClick')}
    />
    <VotingFooterLinks onClickExternalLink={action('onExternalLinkClick')} />
  </div>
);

const renderNonVotingPlaceholder = (activeSidebarCategory: string) => (
  <BorderedBox>
    <h1 style={{ marginTop: 0 }}>Navigation Context</h1>
    <p style={{ marginBottom: 0 }}>
      Active sidebar route: {activeSidebarCategory}. Use the Voting icon to jump
      back into the connected governance flow.
    </p>
  </BorderedBox>
);

const renderGovernanceConfirmationDialog = ({
  chosenOption,
  fees,
  onClose,
  selectedWallet,
}: {
  chosenOption: string;
  fees: BigNumber;
  onClose: () => void;
  selectedWallet: Wallet;
}) => (
  <VotingPowerDelegationConfirmationDialog
    chosenOption={chosenOption}
    fees={fees}
    hwDeviceStatus={
      select(
        'Hardware wallet status',
        hwDeviceStatusOptions,
        HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
      ) as HwDeviceStatus
    }
    isTrezor={boolean('Hardware wallet is Trezor', false)}
    onClose={onClose}
    onExternalLinkClick={action('onExternalLinkClick')}
    onSubmit={async (passphrase) => {
      action('delegateVotes')({
        chosenOption,
        passphrase,
        walletId: selectedWallet.id,
      });
      return boolean('Delegation submission succeeds', true)
        ? { success: true }
        : {
            success: false,
            errorCode: select(
              'Delegation submission error',
              delegateVotesErrorOptions,
              'wrong_encryption_passphrase'
            ),
          };
    }}
    redirectToWallet={action('redirectToWallet')}
    selectedWallet={selectedWallet}
  />
);

storiesOf('Voting / Governance', module)
  .addDecorator(governanceStoryDecorator)
  .addDecorator(withKnobs)
  .add(
    'Connected flow',
    withState(
      {
        activeSidebarCategory: ROUTES.VOTING.GOVERNANCE,
        activeVotingRoute: ROUTES.VOTING.GOVERNANCE,
        currentContentRoute: ROUTES.VOTING.GOVERNANCE,
      },
      (store) => {
        const isVotingSection =
          store.state.currentContentRoute.indexOf(ROUTES.VOTING.ROOT) === 0;
        const activeVotingItem = VOTING_NAV_ITEMS.find(
          ({ id }) => id === store.state.activeVotingRoute
        );

        return (
          <div style={CONNECTED_FLOW_STYLE}>
            <SidebarLayout
              sidebar={
                <Sidebar
                  menus={EMPTY_SIDEBAR_MENUS}
                  categories={VOTING_SIDEBAR_CATEGORIES}
                  activeSidebarCategory={store.state.activeSidebarCategory}
                  isShowingSubMenus={false}
                  pathname={store.state.currentContentRoute}
                  network={TESTNET}
                  onActivateCategory={(category) => {
                    action('onActivateCategory')(category);

                    if (category === ROUTES.VOTING.GOVERNANCE) {
                      store.set({
                        activeSidebarCategory: ROUTES.VOTING.GOVERNANCE,
                        activeVotingRoute: ROUTES.VOTING.GOVERNANCE,
                        currentContentRoute: ROUTES.VOTING.GOVERNANCE,
                      });
                      return;
                    }

                    store.set({
                      activeSidebarCategory: category,
                      currentContentRoute: category,
                    });
                  }}
                  onAddWallet={action('onAddWallet')}
                  isShelleyActivated
                />
              }
              topbar={<TopBar isShelleyActivated />}
            >
              <div style={FLOW_CONTENT_STYLE}>
                {isVotingSection ? (
                  <div style={FLOW_SECTION_STYLE}>
                    <Navigation
                      items={VOTING_NAV_ITEMS}
                      activeItem={activeVotingItem?.label || 'Governance'}
                      isActiveNavItem={(navItemId: string) =>
                        navItemId === store.state.activeVotingRoute
                      }
                      onNavItemClick={(navItemId: string) => {
                        action('onNavItemClick')(navItemId);
                        store.set({
                          activeSidebarCategory: ROUTES.VOTING.GOVERNANCE,
                          activeVotingRoute: navItemId,
                          currentContentRoute: navItemId,
                        });
                      }}
                    />
                    {store.state.activeVotingRoute === ROUTES.VOTING.GOVERNANCE
                      ? renderGovernancePanel()
                      : renderCatalystPanel()}
                  </div>
                ) : (
                  renderNonVotingPlaceholder(store.state.activeSidebarCategory)
                )}
              </div>
            </SidebarLayout>
          </div>
        );
      }
    )
  )
  .add('Voting power delegation', () => (
    <div style={CENTERED_STORY_STYLE}>{renderGovernancePanel()}</div>
  ))
  .add('Confirmation dialog - software wallet', () => (
    <div style={CENTERED_STORY_STYLE}>
      <VotingPowerDelegationConfirmationDialog
        chosenOption={select('Vote option', voteOptions, VALID_DREP_ID)}
        fees={
          new BigNumber(
            number('Transaction fee', 0.174257, {
              min: 0,
              step: 0.000001,
            })
          )
        }
        hwDeviceStatus={HwDeviceStatuses.READY}
        isTrezor={false}
        onClose={action('onClose')}
        onExternalLinkClick={action('onExternalLinkClick')}
        onSubmit={async (passphrase) => {
          action('delegateVotes')({ passphrase });
          return boolean('Submission succeeds', true)
            ? { success: true }
            : {
                success: false,
                errorCode: select(
                  'Submission error',
                  delegateVotesErrorOptions,
                  'wrong_encryption_passphrase'
                ),
              };
        }}
        redirectToWallet={action('redirectToWallet')}
        selectedWallet={GOVERNANCE_WALLETS[0]}
      />
    </div>
  ))
  .add('Confirmation dialog - hardware wallet', () => (
    <div style={CENTERED_STORY_STYLE}>
      <VotingPowerDelegationConfirmationDialog
        chosenOption={select('Vote option', voteOptions, VALID_DREP_ID)}
        fees={
          new BigNumber(
            number('Transaction fee', 0.174257, {
              min: 0,
              step: 0.000001,
            })
          )
        }
        hwDeviceStatus={
          select(
            'Hardware wallet status',
            hwDeviceStatusOptions,
            HwDeviceStatuses.VERIFYING_TRANSACTION
          ) as HwDeviceStatus
        }
        isTrezor={boolean('Is Trezor', false)}
        onClose={action('onClose')}
        onExternalLinkClick={action('onExternalLinkClick')}
        onSubmit={async () => {
          action('delegateVotes')();
          return { success: true };
        }}
        redirectToWallet={action('redirectToWallet')}
        selectedWallet={GOVERNANCE_WALLETS[1]}
      />
    </div>
  ))
  .add('Unavailable while syncing', () => (
    <div style={CENTERED_STORY_STYLE}>
      <VotingUnavailable
        syncPercentage={number('Sync percentage', 62.45, {
          min: 0,
          max: 100,
          step: 0.01,
        })}
      />
    </div>
  ));
