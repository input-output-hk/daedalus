// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { MIN_DELEGATION_FUNDS } from '../../../config/stakingConfig';
import { getNetworkExplorerUrlByType } from '../../../utils/network';
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
    stakePoolJoinFee: new BigNumber(0),
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
    const { nextEpoch } = networkStatus;
    const { currentTheme, currentLocale, environment } = profile;
    const {
      stakePools,
      recentStakePools,
      joinStakePoolRequest,
      getStakePoolById,
      isJoinTransactionPending,
    } = staking;
    const { network, rawNetwork } = environment;
    const nextEpochStartTime = get(nextEpoch, 'epochStart', 0);
    const selectedPool = find(stakePools, pool => pool.id === selectedPoolId);

    const selectedWallet = find(
      wallets.allWallets,
      wallet => wallet.id === selectedWalletId
    );

    const acceptableWallets = find(wallets.allWallets, wallet =>
      this.handleIsWalletAcceptable(wallet.amount)
    );

    const getPledgeAddressUrl = (pledgeAddress: string) =>
      getNetworkExplorerUrlByType(
        'address',
        pledgeAddress,
        network,
        rawNetwork,
        currentLocale
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
        recentStakePools={recentStakePools}
        stakePoolJoinFee={stakePoolJoinFee}
        nextEpochStartTime={nextEpochStartTime}
        currentLocale={currentLocale}
        onOpenExternalLink={app.openExternalLink}
        getPledgeAddressUrl={getPledgeAddressUrl}
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
          joinStakePoolRequest.isExecuting || isJoinTransactionPending
        }
        error={joinStakePoolRequest.error}
      />
    );
  }

  async _handleCalculateTransactionFee() {
    const { calculateDelegationFee } = this.props.stores.staking;
    const { selectedWalletId } = this.state;
    const stakePoolJoinFee = await calculateDelegationFee({
      walletId: selectedWalletId,
    });
    this.setState({ stakePoolJoinFee });
  }
}
