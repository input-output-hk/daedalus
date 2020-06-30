// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get, take } from 'lodash';
import BigNumber from 'bignumber.js';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import {
  MIN_DELEGATION_FUNDS,
  RECENT_STAKE_POOLS_COUNT,
} from '../../../config/stakingConfig';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

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
  activeStep: number,
  selectedWalletId: string,
  selectedPoolId: string,
  stakePoolJoinFee: ?BigNumber,
};

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class DelegationSetupWizardDialogContainer extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  handleIsWalletAcceptable = (walletAmount: BigNumber) =>
    walletAmount.gte(new BigNumber(MIN_DELEGATION_FUNDS));

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
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({ activeStep: activeStep + 1 });
  };

  onBack = () => {
    const { activeStep } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.setState({ activeStep: activeStep - 1 });
  };

  handleLearnMoreClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  handleConfirm = (spendingPassword: string) => {
    const { selectedPoolId, selectedWalletId } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.props.actions.staking.joinStakePool.trigger({
      stakePoolId: selectedPoolId,
      walletId: selectedWalletId,
      passphrase: spendingPassword,
    });
  };

  handleSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
    this.handleContinue();
  };

  handleSelectPool = (poolId: string) => {
    this._handleCalculateTransactionFee();
    this.setState({ selectedPoolId: poolId });
    this.handleContinue();
  };

  render() {
    const {
      activeStep,
      selectedWalletId,
      selectedPoolId,
      stakePoolJoinFee,
    } = this.state;
    const { app, staking, wallets, profile, networkStatus } = this.props.stores;
    const { futureEpoch } = networkStatus;
    const { currentTheme, currentLocale } = profile;
    const {
      stakePools,
      recentStakePools,
      joinStakePoolRequest,
      getStakePoolById,
      isDelegationTransactionPending,
    } = staking;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);
    const selectedPool = find(stakePools, pool => pool.id === selectedPoolId);

    const selectedWallet = find(
      wallets.allWallets,
      wallet => wallet.id === selectedWalletId
    );

    const acceptableWallets = find(wallets.allWallets, wallet =>
      this.handleIsWalletAcceptable(wallet.amount)
    );

    return (
      <DelegationSetupWizardDialog
        wallets={wallets.allWallets}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && !acceptableWallets}
        isWalletAcceptable={this.handleIsWalletAcceptable}
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
        onBack={this.onBack}
        onLearnMoreClick={this.handleLearnMoreClick}
        onConfirm={this.handleConfirm}
        getStakePoolById={getStakePoolById}
        isSubmitting={
          joinStakePoolRequest.isExecuting || isDelegationTransactionPending
        }
        error={joinStakePoolRequest.error}
      />
    );
  }

  async _handleCalculateTransactionFee() {
    const { staking, uiDialogs } = this.props.stores;
    const { isOpen } = uiDialogs;
    const { calculateDelegationFee } = staking;
    const { selectedWalletId } = this.state;
    const stakePoolJoinFee = await calculateDelegationFee({
      walletId: selectedWalletId,
    });

    // Update state only if DelegationSetupWizardDialog is still active
    // and fee calculation was successful
    if (isOpen(DelegationSetupWizardDialog) && stakePoolJoinFee) {
      this.setState({ stakePoolJoinFee });
    }
  }
}
