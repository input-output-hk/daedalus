// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { map } from 'lodash';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import type { InjectedContainerProps } from '../../../types/injectedPropsType';
import { formattedWalletAmount } from '../../../utils/formatters';
import { MIN_DELEGATION_FUNDS } from '../../../config/stakingConfig';

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

type Props = InjectedContainerProps;

type State = {
  activeStep: number,
};

@inject('stores', 'actions')
@observer
export default class DelegationSetupWizardDialogContainer extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    activeStep: 0,
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

    // TODO - remove once all steps are constructed
    if (activeStep === 1) {
      this.handleDialogClose();
    }

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

  render() {
    const { activeStep } = this.state;
    const { stores } = this.props;
    const { wallets } = stores;

    let setupDisabled = true;
    const walletsData = map(wallets.all, wallet => {
      const value = formattedWalletAmount(wallet.amount);
      const isAcceptableSetupWallet = parseFloat(value) > MIN_DELEGATION_FUNDS;

      // Setup enabled if at least one wallet has more that 1 ADA
      if (isAcceptableSetupWallet) {
        setupDisabled = false;
      }

      return {
        value,
        label: wallet.name,
        isAcceptableSetupWallet,
      };
    });

    return (
      <DelegationSetupWizardDialog
        wallets={walletsData}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && setupDisabled}
        onClose={this.handleDialogClose}
        onContinue={this.handleContinue}
        onBack={this.onBack}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}
