import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get, take } from 'lodash';
import BigNumber from 'bignumber.js';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import {
  MIN_DELEGATION_FUNDS,
  RECENT_STAKE_POOLS_COUNT,
  DELEGATION_ACTIONS,
} from '../../../config/stakingConfig';
import type { DelegationCalculateFeeResponse } from '../../../api/staking/types';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import StakePool from '../../../domains/StakePool';

const messages = defineMessages({
  learnMoreLinkUrl: {
    id: 'staking.delegationSetup.intro.step.dialog.learnMore.url',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/',
    description:
      '"Learn more" link URL on the delegation setup "intro" dialog.',
  },
  delegationSetupStep1Label: {
    id: 'staking.delegationSetup.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on delegation steps dialog.',
  },
  delegationSetupStep2Label: {
    id: 'staking.delegationSetup.steps.step.2.label',
    defaultMessage: '!!!Stake pool',
    description: 'Step 2 label text on delegation steps dialog.',
  },
  delegationSetupStep3Label: {
    id: 'staking.delegationSetup.steps.step.3.label',
    defaultMessage: '!!!Confirmation',
    description: 'Step 3 label text on delegation steps dialog.',
  },
});
type State = {
  activeStep: number;
  selectedWalletId: string;
  selectedPoolId: string;
  stakePoolJoinFee: DelegationCalculateFeeResponse | null | undefined;
};
type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class DelegationSetupWizardDialogContainer extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  handleIsWalletAcceptable = (
    walletAmount?: BigNumber,
    // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    walletReward: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_DELEGATION_FUNDS)) &&
    !walletAmount.isEqualTo(walletReward);

  get selectedWalletId() {
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }

  get selectedPoolId() {
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'poolId'],
      null
    );
  }

  state = {
    activeStep: 0,
    selectedWalletId: this.selectedWalletId,
    selectedPoolId: this.selectedPoolId,
    stakePoolJoinFee: null,
  };
  STEPS_LIST = [
    this.context.intl.formatMessage(messages.delegationSetupStep1Label),
    this.context.intl.formatMessage(messages.delegationSetupStep2Label),
    this.context.intl.formatMessage(messages.delegationSetupStep3Label),
  ];
  handleDialogClose = () => {
    const { stores, actions } = this.props;
    stores.staking.joinStakePoolRequest.reset();
    actions.dialogs.closeActiveDialog.trigger();

    stores.hardwareWallets._resetTransaction({
      cancelDeviceAction: true,
    });
  };
  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({
      activeStep: activeStep + 1,
    });
  };
  onBack = () => {
    const { activeStep } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.setState({
      activeStep: activeStep - 1,
    });
  };
  handleLearnMoreClick = (event: React.SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };
  handleConfirm = (
    spendingPassword: string | null | undefined,
    isHardwareWallet: boolean
  ) => {
    const { selectedPoolId, selectedWalletId } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.props.actions.staking.joinStakePool.trigger({
      stakePoolId: selectedPoolId,
      walletId: selectedWalletId,
      passphrase: spendingPassword,
      isHardwareWallet,
    });
  };
  handleSelectWallet = (walletId: string) => {
    this.setState({
      selectedWalletId: walletId,
    });
    this.props.actions.staking.selectDelegationWallet.trigger(walletId);
    this.handleContinue();
  };
  handleChoosePool = (poolId: string) => {
    this.setState({
      selectedPoolId: poolId,
    });
  };
  handleSelectPool = (pool: StakePool) => {
    this._handleCalculateTransactionFee(pool.id);

    this.handleContinue();
  };

  render() {
    const {
      activeStep,
      selectedWalletId,
      selectedPoolId,
      stakePoolJoinFee,
    } = this.state;
    const {
      app,
      staking,
      wallets,
      profile,
      networkStatus,
      hardwareWallets,
    } = this.props.stores;
    const { futureEpoch } = networkStatus;
    const { currentTheme, currentLocale } = profile;
    const {
      hwDeviceStatus,
      sendMoneyRequest,
      selectCoinsRequest,
      checkIsTrezorByWalletId,
    } = hardwareWallets;
    const {
      stakePools,
      recentStakePools,
      joinStakePoolRequest,
      getStakePoolById,
      isDelegationTransactionPending,
      maxDelegationFunds,
    } = staking;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);
    const selectedPool = find(stakePools, (pool) => pool.id === selectedPoolId);
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );
    const acceptableWallets = find(wallets.allWallets, ({ amount, reward }) =>
      this.handleIsWalletAcceptable(amount, reward)
    );
    let isTrezor = false;

    if (selectedWallet) {
      isTrezor = checkIsTrezorByWalletId(selectedWallet.id);
    }

    return (
      <DelegationSetupWizardDialog
        wallets={wallets.allWallets}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && !acceptableWallets}
        isWalletAcceptable={this.handleIsWalletAcceptable}
        maxDelegationFunds={maxDelegationFunds}
        selectedWallet={selectedWallet}
        selectedPool={selectedPool || null}
        stakePoolsList={stakePools}
        recentStakePools={take(recentStakePools, RECENT_STAKE_POOLS_COUNT)}
        stakePoolJoinFee={stakePoolJoinFee}
        futureEpochStartTime={futureEpochStartTime}
        currentLocale={currentLocale}
        onOpenExternalLink={app.openExternalLink}
        currentTheme={currentTheme}
        onClose={this.handleDialogClose}
        onContinue={this.handleContinue}
        onSelectWallet={this.handleSelectWallet}
        onSelectPool={this.handleSelectPool}
        onThumbPoolSelect={this.handleChoosePool}
        onBack={this.onBack}
        onLearnMoreClick={this.handleLearnMoreClick}
        onConfirm={this.handleConfirm}
        getStakePoolById={getStakePoolById}
        isSubmitting={
          joinStakePoolRequest.isExecuting ||
          sendMoneyRequest.isExecuting ||
          isDelegationTransactionPending
        }
        error={
          joinStakePoolRequest.error ||
          sendMoneyRequest.error ||
          selectCoinsRequest.error
        }
        hwDeviceStatus={hwDeviceStatus}
        isTrezor={isTrezor}
      />
    );
  }

  async _handleCalculateTransactionFee(poolId: string) {
    const { staking, uiDialogs, wallets, hardwareWallets } = this.props.stores;
    const { isOpen } = uiDialogs;
    const { calculateDelegationFee } = staking;
    const { selectedWalletId } = this.state;
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );
    let stakePoolJoinFee;

    if (selectedWallet.isHardwareWallet) {
      // Calculate fee from coins selections
      const coinsSelection = await hardwareWallets.selectDelegationCoins({
        walletId: selectedWallet.id,
        poolId,
        delegationAction: DELEGATION_ACTIONS.JOIN,
      });
      const { deposits, depositsReclaimed, fee } = coinsSelection;
      stakePoolJoinFee = {
        deposits,
        depositsReclaimed,
        fee,
      };
      // Initiate Transaction (Delegation)
      hardwareWallets.initiateTransaction({
        walletId: selectedWalletId,
      });
    } else {
      stakePoolJoinFee = await calculateDelegationFee({
        walletId: selectedWalletId,
      });
    }

    // Update state only if DelegationSetupWizardDialog is still mounted and active
    // and fee calculation was successful
    if (
      this._isMounted &&
      isOpen(DelegationSetupWizardDialog) &&
      stakePoolJoinFee
    ) {
      this.setState({
        stakePoolJoinFee,
      });
    }
  }
}

export default DelegationSetupWizardDialogContainer;
