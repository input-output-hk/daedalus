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

  componentWillReceiveProps(nextProps: Props) {
    console.log('nextProps', nextProps);
    const stepChanged =
      nextProps.stores.wallets.walletCreateStep !== this.state.currentStep;
    if (nextProps.stores.wallets.walletCreateStep && stepChanged) {
      this.onContinue(nextProps.stores.wallets.walletCreateStep);
    }
  }

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
          onContinue={this.onContinue}
          onClose={this.onClose}
        />
      );
    }

    if (showConfirmationDialog) return <div>Are you sure?</div>;

    return activeDialog;
  }

  onContinue = (nextStep: number) => {
    const nextDialog = CREATE_WALLET_STEPS[nextStep];
    this.switchDialog(nextDialog);
    this.setState({ currentStep: nextStep });
  };

  onBack = () => {
    // eslint-disable-next-line react/no-access-state-in-setstate
    const prevStep = this.state.currentStep ? this.state.currentStep - 1 : 0;
    const prevDialog = CREATE_WALLET_STEPS[prevStep];
    this.setState({ currentStep: prevStep });
    this.switchDialog(prevDialog);
    this.props.actions.wallets.updateCertificateStep.trigger(true);
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
