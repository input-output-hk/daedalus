import React from 'react';
import { action } from '@storybook/addon-actions';
import {
  withKnobs,
  boolean,
  number,
  select,
  text,
} from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
import { find } from 'lodash';
import { withState } from '../_support/WithLocalState';
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
    id: ROUTES.GOVERNANCE.DELEGATE,
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

// A form that will always fail to initialize, with the error fixed by the
// story rather than by a knob. The current delegation and the chosen DRep are
// deliberately different, so the message is read against a screen that does
// not contradict it.
const renderErrorPanel = (errorCode: InitializeVPDelegationTxError) => (
  <div style={CENTERED_STORY_STYLE}>
    <GovernanceWrapper option="drepVerified">
      {({ wallets, drepIndex }) => (
        <VotingPowerDelegation
          getStakePoolById={getStakePoolById}
          onFetchDRep={makeFetchDRep(drepIndex)}
          initiateTransaction={async (params) => {
            action('initiateTransaction')(params);
            return { success: false, errorCode };
          }}
          initialFormState={{
            selectedDRepId: VALID_DREP_ID,
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
    'generic'
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

// One story per device state. The knob that drove these shares its name with
// the one the in-flow dialog registers, and addon-knobs stores values by name,
// so a value set while looking at one story arrived in the other and the
// device appeared to be stuck in whichever state was last chosen.
const renderHardwareDialog = (hwDeviceStatus: HwDeviceStatus) => (
  <div style={CENTERED_STORY_STYLE}>
    <VotingPowerDelegationConfirmationDialog
      chosenOption={VALID_DREP_ID}
      drepIdentity={toStoryDRepIdentity(VALID_DREP_ID)}
      fees={new BigNumber('0.174257')}
      hwDeviceStatus={hwDeviceStatus}
      isTrezor={boolean('Is Trezor', false)}
      onClose={action('onClose')}
      onExternalLinkClick={action('onExternalLinkClick')}
      onSubmit={async () => {
        action('delegateVotes')();
        return { success: true };
      }}
      redirectToWallet={action('redirectToWallet')}
      selectedWallet={makeGovernanceWallets('noDelegation')[1]}
      verifiedName={toStoryVerifiedName(VALID_DREP_ID)}
    />
  </div>
);

const renderSentinelDialog = (option: 'abstain' | 'no_confidence') => (
  <div style={CENTERED_STORY_STYLE}>
    <VotingPowerDelegationConfirmationDialog
      chosenOption={option}
      drepIdentity={null}
      fees={new BigNumber('0.174257')}
      hwDeviceStatus={HwDeviceStatuses.READY}
      isTrezor={false}
      onClose={action('onClose')}
      onExternalLinkClick={action('onExternalLinkClick')}
      onSubmit={async (passphrase) => {
        action('delegateVotes')({ passphrase });
        return { success: true };
      }}
      redirectToWallet={action('redirectToWallet')}
      selectedWallet={makeGovernanceWallets('noDelegation')[0]}
      verifiedName={null}
    />
  </div>
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
        HwDeviceStatuses.VERIFYING_TRANSACTION
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

export default {
  title: 'Governance / Delegation',
  decorators: [governanceStoryDecorator, withKnobs],
};

export const ConnectedFlow = withState(
  {
    activeSidebarCategory: ROUTES.GOVERNANCE.DELEGATE,
    activeVotingRoute: ROUTES.GOVERNANCE.DELEGATE,
    currentContentRoute: ROUTES.GOVERNANCE.DELEGATE,
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

                if (category === ROUTES.GOVERNANCE.DELEGATE) {
                  store.set({
                    activeSidebarCategory: ROUTES.GOVERNANCE.DELEGATE,
                    activeVotingRoute: ROUTES.GOVERNANCE.DELEGATE,
                    currentContentRoute: ROUTES.GOVERNANCE.DELEGATE,
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
                      activeSidebarCategory: ROUTES.GOVERNANCE.DELEGATE,
                      activeVotingRoute: navItemId,
                      currentContentRoute: navItemId,
                    });
                  }}
                />
                {store.state.activeVotingRoute === ROUTES.GOVERNANCE.DELEGATE
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
);

ConnectedFlow.storyName = 'Connected flow';

export const _VotingPowerDelegation = {
  render: () => {
    const option = useCurrentVoteKnob();
    return (
      <div style={CENTERED_STORY_STYLE}>{renderGovernancePanel(option)}</div>
    );
  },

  name: 'Voting power delegation',
};

export const VotingPowerDelegationPrefilledFromDirectory = {
  render: () => renderPrefilledPanel(useCurrentVoteKnob(), VALID_DREP_ID),

  name: 'Voting power delegation - prefilled from directory',
};

export const AbstainToADRep = {
  render: () => renderPrefilledPanel('abstain', VERIFIED_CIP129),

  name: 'Abstain to a DRep',
};

export const NoConfidenceToADRep = {
  render: () => renderPrefilledPanel('noConfidence', VERIFIED_CIP129),

  name: 'No Confidence to a DRep',
};

export const DRepToAbstain = {
  render: () => renderPrefilledPanel('drepVerified', 'abstain'),

  name: 'DRep to Abstain',
};

export const DRepToNoConfidence = {
  render: () => renderPrefilledPanel('drepVerified', 'no_confidence'),

  name: 'DRep to No Confidence',
};

export const AlreadyDelegatedToThisDRep = {
  render: () => renderPrefilledPanel('drepVerified', VERIFIED_CIP129),

  name: 'Already delegated to this DRep',
};

export const AlreadyDelegatedToAbstain = {
  render: () => renderPrefilledPanel('abstain', 'abstain'),

  name: 'Already delegated to Abstain',
};

export const InitializationErrorGeneric = {
  render: () => renderErrorPanel('generic'),
  name: 'Initialization error - generic',
};

export const InitializationErrorSameVote = {
  render: () => renderErrorPanel('same_vote'),
  name: 'Initialization error - same vote',
};

export const InitializationErrorNoUTxOs = {
  render: () => renderErrorPanel('no_utxos_available'),

  name: 'Initialization error - no UTxOs',
};

export const InitializationErrorNotEnoughMoney = {
  render: () => renderErrorPanel('not_enough_money'),

  name: 'Initialization error - not enough money',
};

export const NotDelegatedYet = {
  render: () => renderPrefilledPanel('noDelegation'),
  name: 'Not delegated yet',
};

export const ConfirmationDialogSoftwareWallet = {
  render: () => {
    const voteOption = select('Vote option', voteOptions, VALID_DREP_ID);
    // Read while the story renders. Inside onSubmit they run only once a
    // transaction has been submitted, so addon-knobs never registers them and
    // the panel offers nothing to change.
    const submissionSucceeds = boolean('Submission succeeds', true);
    const submissionError = select(
      'Submission error',
      delegateVotesErrorOptions,
      'wrong_encryption_passphrase'
    );
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
            return submissionSucceeds
              ? { success: true }
              : { success: false, errorCode: submissionError };
          }}
          redirectToWallet={action('redirectToWallet')}
          selectedWallet={makeGovernanceWallets('noDelegation')[0]}
          verifiedName={toStoryVerifiedName(voteOption)}
        />
      </div>
    );
  },

  name: 'Confirmation dialog - software wallet',
};

export const HardwareWalletConnecting = {
  render: () => renderHardwareDialog(HwDeviceStatuses.CONNECTING),

  name: 'Hardware wallet - connecting',
};

export const HardwareWalletVerifying = {
  render: () => renderHardwareDialog(HwDeviceStatuses.VERIFYING_TRANSACTION),

  name: 'Hardware wallet - verifying',
};

export const HardwareWalletVerified = {
  render: () =>
    renderHardwareDialog(HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED),

  name: 'Hardware wallet - verified',
};

export const HardwareWalletVerificationFailed = {
  render: () =>
    renderHardwareDialog(HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED),

  name: 'Hardware wallet - verification failed',
};

export const ConfirmationDialogAbstain = {
  render: () => renderSentinelDialog('abstain'),
  name: 'Confirmation dialog - Abstain',
};

export const ConfirmationDialogNoConfidence = {
  render: () => renderSentinelDialog('no_confidence'),

  name: 'Confirmation dialog - No Confidence',
};

export const ConfirmationDialogSubmissionFails = {
  render: () => {
    const voteOption = select('Vote option', voteOptions, VALID_DREP_ID);
    const errorCode = select(
      'Submission error',
      delegateVotesErrorOptions,
      'generic'
    );
    return (
      <div style={CENTERED_STORY_STYLE}>
        <VotingPowerDelegationConfirmationDialog
          chosenOption={voteOption}
          drepIdentity={toStoryDRepIdentity(voteOption)}
          fees={new BigNumber('0.174257')}
          hwDeviceStatus={HwDeviceStatuses.READY}
          isTrezor={false}
          onClose={action('onClose')}
          onExternalLinkClick={action('onExternalLinkClick')}
          onSubmit={async (passphrase) => {
            action('delegateVotes')({ passphrase });
            return { success: false, errorCode };
          }}
          redirectToWallet={action('redirectToWallet')}
          selectedWallet={makeGovernanceWallets('noDelegation')[0]}
          verifiedName={toStoryVerifiedName(voteOption)}
        />
      </div>
    );
  },

  name: 'Confirmation dialog - submission fails',
};

export const ConfirmationDialogHardwareWallet = {
  render: () => {
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
  },

  name: 'Confirmation dialog - hardware wallet',
};

export const UnavailableWhileSyncing = {
  render: () => (
    <div style={CENTERED_STORY_STYLE}>
      <VotingUnavailable
        syncPercentage={number('Sync percentage', 62.45, {
          min: 0,
          max: 100,
          step: 0.01,
        })}
      />
    </div>
  ),

  name: 'Unavailable while syncing',
};
