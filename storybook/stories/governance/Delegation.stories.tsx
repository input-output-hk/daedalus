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
} from '../../../source/renderer/app/domains/Wallet';
import type StakePool from '../../../source/renderer/app/domains/StakePool';
import type { CatalystFund } from '../../../source/renderer/app/api/voting/types';
import { TESTNET } from '../../../source/common/types/environment.types';
import type { Locale } from '../../../source/common/types/locales.types';
import type { DRepIdentity } from '../../../source/common/types/governance.types';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import { ROUTES } from '../../../source/renderer/app/routes-config';
import { FundPhase } from '../../../source/renderer/app/stores/VotingStore';
import type {
  DelegateVotesError,
  InitializeVPDelegationTxError,
} from '../../../source/renderer/app/stores/VotingStore';
import GovernanceWrapper from './_utils/GovernanceWrapper';
import {
  makeGovernanceWallets,
  useCurrentVoteKnob,
  VERIFIED_CIP129,
} from './_utils/fixtures';
import type { CurrentVoteOption } from './_utils/fixtures';
import { normalizeDRepIdentity } from '../../../source/renderer/app/utils/governance/normalizeDRepIdentity';

const VALID_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

const toStoryDRepIdentity = (option: string): DRepIdentity | null =>
  option === 'abstain' || option === 'no_confidence'
    ? null
    : normalizeDRepIdentity(option);

const voteOptions = {
  'Delegate to DRep': VALID_DREP_ID,
  Abstain: 'abstain',
  'No confidence': 'no_confidence',
};

const toStoryVerifiedName = (option: string) =>
  option === 'abstain' ||
  option === 'no_confidence' ||
  !boolean('Verified anchor name available', true)
    ? null
    : {
        host: text('Verified name host', 'raw.githubusercontent.com'),
        name: text('Verified name', 'Daedalus Test DRep'),
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
  CATEGORIES_BY_NAME.GOVERNANCE,
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

// A wallet already chosen and a DRep already in the form, which is how this
// screen opens when it is reached from the directory rather than the sidebar.
const makeFetchDRep =
  (drepIndex: ReadonlyMap<string, AppDRepDirectoryEntry>) =>
  async (drepId: string) => {
    const entry = drepIndex.get(drepId.toLowerCase()) ?? null;
    action('onFetchDRep')(drepId, entry);
    // Rejecting is what the store does for a DRep it cannot find, and it is
    // what tells the panel to stop saying "loading".
    if (entry == null) throw new Error(`No DRep entry for ${drepId}`);
    return entry;
  };

const renderPrefilledPanel = (
  option: CurrentVoteOption,
  selectedDRepId?: string
) => {
  // Read before the tree is built rather than inside it, so the knobs are
  // registered by the story itself and appear in the panel from the first
  // render, whether or not anything has reached the component using them.
  const initiateTransaction = makeInitiateTransaction();

  return (
    <div style={CENTERED_STORY_STYLE}>
      <GovernanceWrapper option={option}>
        {({ wallets, drepIndex }) => (
          <VotingPowerDelegation
            getStakePoolById={getStakePoolById}
            onFetchDRep={makeFetchDRep(drepIndex)}
            initiateTransaction={initiateTransaction}
            initialFormState={{
              ...(selectedDRepId ? { selectedDRepId } : {}),
              selectedWalletId: 'governance-wallet-1',
              voteType: 'drep',
            }}
            onBrowseDRepsClick={action('onBrowseDRepsClick')}
            onCancel={action('onCancel')}
            onExternalLinkClick={action('onExternalLinkClick')}
            renderConfirmationDialog={renderGovernanceConfirmationDialog}
            stakePools={STAKE_POOLS_LIST}
            wallets={wallets}
          />
        )}
      </GovernanceWrapper>
    </div>
  );
};

// The knobs are read here, while the story renders, and the values closed
// over. Read inside the returned function instead, they run only when a
// transaction is attempted: addon-knobs registers a knob when its call
// executes, so a knob nothing has executed yet never appears in the panel at
// all, and changing one drives no re-render.
const makeInitiateTransaction = (
  fee: BigNumber = new BigNumber('0.174257')
) => {
  const succeeds = boolean('Initialization succeeds', true);
  const errorCode = select(
    'Initialization error',
    initializeTxErrorOptions,
    'same_vote'
  );
  return async (params: unknown) => {
    action('initiateTransaction')(params);
    return succeeds
      ? { success: true, fees: fee }
      : { success: false, errorCode };
  };
};

const renderGovernancePanel = (option: CurrentVoteOption) => {
  const transactionFee = new BigNumber(
    number('Initialized transaction fee', 0.174257, {
      min: 0,
      step: 0.000001,
    })
  );
  text('Valid DRep ID fixture', VALID_DREP_ID);
  const initiateTransaction = makeInitiateTransaction(transactionFee);

  return (
    <GovernanceWrapper option={option}>
      {({ wallets, drepIndex }) => (
        <VotingPowerDelegation
          getStakePoolById={getStakePoolById}
          onFetchDRep={makeFetchDRep(drepIndex)}
          initiateTransaction={initiateTransaction}
          onBrowseDRepsClick={action('onBrowseDRepsClick')}
          onCancel={action('onCancel')}
          onExternalLinkClick={action('onExternalLinkClick')}
          renderConfirmationDialog={renderGovernanceConfirmationDialog}
          stakePools={STAKE_POOLS_LIST}
          wallets={wallets}
        />
      )}
    </GovernanceWrapper>
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
    drepIdentity={toStoryDRepIdentity(chosenOption)}
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
    verifiedName={toStoryVerifiedName(chosenOption)}
  />
);

storiesOf('Governance / Delegation', module)
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
        const option = useCurrentVoteKnob();
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
                      ? renderGovernancePanel(option)
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
  .add('Voting power delegation', () => {
    const option = useCurrentVoteKnob();
    return (
      <div style={CENTERED_STORY_STYLE}>{renderGovernancePanel(option)}</div>
    );
  })
  .add('Voting power delegation - prefilled from directory', () =>
    renderPrefilledPanel(useCurrentVoteKnob(), VALID_DREP_ID)
  )
  // The two changes of mind that cross vote kinds. A wallet on Abstain or No
  // Confidence has a delegation already, so the panel above the form states
  // one thing while the form below it proposes another, and those two have to
  // read as one screen rather than as a contradiction. Neither was reachable
  // before: the prefilled story took its current vote from the knob, and the
  // knob's abstain settings left the form empty.
  .add('Abstain to a DRep', () =>
    renderPrefilledPanel('abstain', VERIFIED_CIP129)
  )
  .add('No Confidence to a DRep', () =>
    renderPrefilledPanel('noConfidence', VERIFIED_CIP129)
  )
  // And the same two changes in reverse. The directory hands Abstain and No
  // Confidence to this form as the selected id, the same way it hands over a
  // DRep, so the form receives the literal strings 'abstain' and
  // 'no_confidence' where it otherwise receives a bech32 identifier.
  .add('DRep to Abstain', () => renderPrefilledPanel('drepVerified', 'abstain'))
  .add('DRep to No Confidence', () =>
    renderPrefilledPanel('drepVerified', 'no_confidence')
  )
  // Choosing what the wallet already has. The form refuses to submit and says
  // so, and the sentence it says has a branch per vote kind, so both branches
  // need somewhere to be read.
  .add('Already delegated to this DRep', () =>
    renderPrefilledPanel('drepVerified', VERIFIED_CIP129)
  )
  .add('Already delegated to Abstain', () =>
    renderPrefilledPanel('abstain', 'abstain')
  )
  // A wallet with no delegation at all. The current-delegation panel renders
  // nothing here by design, so the form runs straight from the wallet select
  // to the DRep selection, and the only thing that mentions rewards is the
  // paragraph at the top of the page.
  .add('Not delegated yet', () => renderPrefilledPanel('noDelegation'))
  .add('Confirmation dialog - software wallet', () => {
    const voteOption = select('Vote option', voteOptions, VALID_DREP_ID);
    return (
      <div style={CENTERED_STORY_STYLE}>
        <VotingPowerDelegationConfirmationDialog
          chosenOption={voteOption}
          drepIdentity={toStoryDRepIdentity(voteOption)}
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
          selectedWallet={makeGovernanceWallets('noDelegation')[0]}
          verifiedName={toStoryVerifiedName(voteOption)}
        />
      </div>
    );
  })
  .add('Confirmation dialog - hardware wallet', () => {
    const voteOption = select('Vote option', voteOptions, VALID_DREP_ID);
    return (
      <div style={CENTERED_STORY_STYLE}>
        <VotingPowerDelegationConfirmationDialog
          chosenOption={voteOption}
          drepIdentity={toStoryDRepIdentity(voteOption)}
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
          selectedWallet={makeGovernanceWallets('noDelegation')[1]}
          verifiedName={toStoryVerifiedName(voteOption)}
        />
      </div>
    );
  })
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
