// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
// import WalletCreateDialog from '../../../components/wallet/WalletCreateDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

import InstructionsDialog from '../../../components/wallet/wallet-create/InstructionsDialog';
import InstructionsDialogContainer from './wallet-create/InstructionsDialogContainer';

type Props = InjectedDialogContainerProps;

type State = {
  currentStep: ?number,
  showConfirmationDialog: boolean,
};

export const CREATE_WALLET_STEPS = [
  'instructions',
  'template',
  'mnemonics',
  'validate',
  'hashImage',
  'config',
];

@inject('stores', 'actions')
@observer
export default class WalletCreateDialogContainer extends Component<
  Props,
  State
> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  state = {
    currentStep: 0,
    showConfirmationDialog: false,
  };

  onSubmit = (values: { name: string, spendingPassword: ?string }) => {
    this.props.actions.wallets.createWallet.trigger(values);
  };

  render() {
    const { uiDialogs } = this.props.stores;
    const { showConfirmationDialog } = this.state;
    let activeDialog = null;

    if (uiDialogs.isOpen(InstructionsDialog)) {
      activeDialog = (
        <InstructionsDialogContainer
          onClose={this.onClose}
          onContinue={this.onContinue}
        />
      );
    }

    if (showConfirmationDialog) return <div>Are you sure?</div>;

    return activeDialog;
  }

  onContinue = () => {
    const { currentStep } = this.state;
    const nextStep =
      currentStep + 1 < CREATE_WALLET_STEPS.length
        ? currentStep + 1
        : currentStep;
    this.changeStep(nextStep);
  };

  onBack = () => {
    const { currentStep } = this.state;
    const prevStep = currentStep ? currentStep - 1 : currentStep;
    this.changeStep(prevStep);
  };

  changeStep = (newStep: number) => {
    const { currentStep } = this.state;
    const isBack = newStep < currentStep;
    const newDialog = CREATE_WALLET_STEPS[newStep];
    this.switchDialog(newDialog);
    this.setState({ currentStep: newStep });
    this.props.actions.wallets.updateCertificateStep.trigger(isBack);
  };

  onClose = () => {
    this.setState({
      currentStep: 0,
      showConfirmationDialog: false,
    });
    this.props.actions.wallets.closeCertificateGeneration.trigger();
  };

  showConfirmationDialog = () => {
    this.setState({ showConfirmationDialog: true });
  };

  hideConfirmationDialog = () => {
    this.setState({ showConfirmationDialog: false });
  };

  switchDialog = (dialog: string) => {
    switch (dialog) {
      case 'instructions':
        this.props.actions.dialogs.open.trigger({
          dialog: InstructionsDialog,
        });
        break;
      default:
        this.onClose();
    }
  };
}
