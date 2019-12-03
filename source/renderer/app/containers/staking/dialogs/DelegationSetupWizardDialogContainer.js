// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { MIN_DELEGATION_FUNDS } from '../../../config/stakingConfig';
import Wallet from '../../../domains/Wallet';
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
  selectedPoolId: ?string,
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

  state = {
    activeStep: 0,
    selectedWalletId: get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    ),
    selectedPoolId: get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'poolId'],
      null
    ),
    stakePoolJoinFee: new BigNumber(0),
  };

  componentWillReceiveProps(nextProps) {
    console.debug('RECEICE PROPS: ', {
      THIS: this.props.stores.staking.joinStakePoolRequest,
      NEXT: nextProps.stores.staking.joinStakePoolRequest,
    })
    if (this.props.stores.staking.joinStakePoolRequest.isExecuting && !nextProps.stores.staking.joinStakePoolRequest.isExecuting && !nextProps.joinStakePoolRequest.error) {
      this.handleContinue();
    }
  }

  STEPS_LIST = [
    this.context.intl.formatMessage(messages.delegationSetupStep1Label),
    this.context.intl.formatMessage(messages.delegationSetupStep2Label),
    this.context.intl.formatMessage(messages.delegationSetupStep3Label),
  ];

  handleDialogClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({ activeStep: activeStep + 1 });
  };

  onBack = () => {
    const { activeStep } = this.state;
    this.setState({ activeStep: activeStep - 1 });
  };

  handleLearnMoreClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  handleConfirm = (spendingPassword: string) => {
    console.debug('CONFIRM: ', spendingPassword);
    const { stakePoolJoinFee, selectedPoolId, selectedWalletId } = this.state;
    this.props.actions.staking.joinStakePool.trigger({
      stakePoolId: selectedPoolId,
      walletId: selectedWalletId,
      passphrase: spendingPassword,
    });
    // this.handleContinue();
  };

  handleSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
    this.handleContinue();
  };

  handleSelectPool = (poolId: string) => {
    this._handleCalculateTransactionFee(poolId);
    this.setState({ selectedPoolId: poolId });
    this.handleContinue();
  };

  handleIsWalletAcceptable = (walletAmount: BigNumber) =>
    walletAmount.gte(MIN_DELEGATION_FUNDS);

  render() {
    const { activeStep, selectedWalletId, selectedPoolId, stakePoolJoinFee } = this.state;
    const { app, staking, wallets, profile } = this.props.stores;
    const { currentTheme, currentLocale } = profile;
    const { stakePools, delegatingStakePools, joinStakePoolRequest, startDateTime } = staking;
    const selectedPool = find(stakePools, pool => pool.id === selectedPoolId);
    const selectedWallet = find(wallets.allWallets, wallet => wallet.id === selectedWalletId);
    const isDisabled = wallets.allWallets.reduce(
      (disabled: boolean, { amount }: Wallet) => {
        if (!disabled) return false;
        return this.handleIsWalletAcceptable(amount);
      },
      false
    );

    console.debug('REQ ---> ', {
      isSubmitting: joinStakePoolRequest.isExecuting,
      error: joinStakePoolRequest.error,
      currentLocale,
    })

    return (
      <DelegationSetupWizardDialog
        wallets={wallets.allWallets}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && isDisabled}
        isWalletAcceptable={this.handleIsWalletAcceptable}
        selectedWallet={selectedWallet}
        selectedPool={selectedPool || null}
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
        stakePoolJoinFee={stakePoolJoinFee}
        startDateTime={startDateTime}
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
        isSubmitting={joinStakePoolRequest.isExecuting}
        error={joinStakePoolRequest.error}
      />
    );
  }

  async _handleCalculateTransactionFee(poolId: string) {
    const { estimateJoinFee } = this.props.stores.staking;
    const { selectedWalletId } = this.state;
    const stakePoolJoinFee = await estimateJoinFee({
      walletId: selectedWalletId,
      stakePoolId: poolId
    });
    this.setState({ stakePoolJoinFee });
  }
}


