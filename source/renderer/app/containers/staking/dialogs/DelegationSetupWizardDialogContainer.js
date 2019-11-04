// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { map, find, get } from 'lodash';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { formattedWalletAmount } from '../../../utils/formatters';
import { MIN_DELEGATION_FUNDS } from '../../../config/stakingConfig';
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
  selectedWalletId: ?string,
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

  render() {
    const { activeStep, selectedWalletId, selectedPoolId } = this.state;
    const { app, staking, wallets, profile } = this.props.stores;
    const { currentTheme } = profile;
    const { stakePools, delegatingStakePools } = staking;

    let setupDisabled = true;
    const walletsData = map(wallets.all, wallet => {
      const value = formattedWalletAmount(wallet.amount);
      const isAcceptableSetupWallet = parseFloat(value) >= MIN_DELEGATION_FUNDS;

      // Setup enabled if at least one wallet has more that 1 ADA
      if (isAcceptableSetupWallet) {
        setupDisabled = false;
      }

      return {
        id: wallet.id,
        label: wallet.name,
        value,
        isAcceptableSetupWallet,
      };
    });

    const selectedWallet = find(
      walletsData,
      wallet => wallet.id === selectedWalletId
    );

    const selectedPool = find(stakePools, pool => pool.id === selectedPoolId);

    return (
      <DelegationSetupWizardDialog
        wallets={walletsData}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && setupDisabled}
        selectedWallet={selectedWallet || null}
        selectedPool={selectedPool || null}
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
        onOpenExternalLink={app.openExternalLink}
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
