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
import Wallet from '../../../domains/Wallet';

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
    defaultMessage: '!!!Delegation',
    description: 'Step 3 label text on delegation steps dialog.',
  },
  delegationSetupStep4Label: {
    id: 'staking.delegationSetup.steps.step.4.label',
    defaultMessage: '!!!Activation',
    description: 'Step 4 label text on delegation steps dialog.',
  },
});

type State = {
  activeStep: number,
  selectedWalletId: string,
  selectedPoolId: ?string,
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
  };

  STEPS_LIST = [
    this.context.intl.formatMessage(messages.delegationSetupStep1Label),
    this.context.intl.formatMessage(messages.delegationSetupStep2Label),
    this.context.intl.formatMessage(messages.delegationSetupStep3Label),
    this.context.intl.formatMessage(messages.delegationSetupStep4Label),
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

  handleConfirm = () => {
    // @TODO - proceed confirmation data
    this.handleContinue();
  };

  handleActivate = () => {
    // @TODO - proceed activation data
    this.handleDialogClose();
  };

  handleSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
    this.handleContinue();
  };

  handleSelectPool = (poolId: string) => {
    this.setState({ selectedPoolId: poolId });
    this.handleContinue();
  };

  handleIsWalletAcceptable = (walletAmount: BigNumber) =>
    walletAmount.gte(MIN_DELEGATION_FUNDS);

  render() {
    const { activeStep, selectedWalletId, selectedPoolId } = this.state;
    const { app, staking, wallets, profile } = this.props.stores;
    const { currentTheme, currentLocale, environment } = profile;
    const { stakePools, delegatingStakePools } = staking;
    const { network, rawNetwork } = environment;
    const isDisabled = wallets.allWallets.reduce(
      (disabled: boolean, { amount }: Wallet) => {
        if (!disabled) return false;
        return this.handleIsWalletAcceptable(amount);
      },
      false
    );
    const selectedPool = find(stakePools, pool => pool.id === selectedPoolId);

    const getPledgeAddressUrl = (pledge: string) =>
      getNetworkExplorerUrlByType(
        'address',
        pledge,
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
        isDisabled={activeStep === 1 && isDisabled}
        isWalletAcceptable={this.handleIsWalletAcceptable}
        selectedWalletId={selectedWalletId}
        selectedPool={selectedPool || null}
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
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
        onActivate={this.handleActivate}
      />
    );
  }
}
