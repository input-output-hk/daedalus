// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import InstructionsDialog from '../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import InstructionsDialogContainer from './dialogs/paper-wallet-certificate/InstructionsDialogContainer';
import PasswordChoiceDialog from '../../components/wallet/paper-wallet-certificate/PasswordChoiceDialog';
import PasswordChoiceDialogContainer from './dialogs/paper-wallet-certificate/PasswordChoiceDialogContainer';
import PrintDialog from '../../components/wallet/paper-wallet-certificate/PrintDialog';
import PrintDialogContainer from './dialogs/paper-wallet-certificate/PrintDialogContainer';
import SecuringPasswordDialog from '../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import SecuringPasswordDialogContainer from './dialogs/paper-wallet-certificate/SecuringPasswordDialogContainer';
import VerificationDialog from '../../components/wallet/paper-wallet-certificate/VerificationDialog';
import VerificationDialogContainer from './dialogs/paper-wallet-certificate/VerificationDialogContainer';
import CompletionDialog from '../../components/wallet/paper-wallet-certificate/CompletionDialog';
import CompletionDialogContainer from './dialogs/paper-wallet-certificate/CompletionDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

type State = {
  currentStep: ?number,
};

@inject('actions', 'stores') @observer
export default class PaperWalletCreateCertificatePage extends Component<Props, State> {

  static defaultProps = { actions: null, stores: null };

  componentWillReceiveProps(nextProps: Props) {
    const stepChanged = nextProps.stores.ada.wallets.certificateStep !== this.state.currentStep;
    if (nextProps.stores.ada.wallets.certificateStep && stepChanged) {
      this.onContinue(nextProps.stores.ada.wallets.certificateStep);
    }
  }

  static defaultProps = { actions: null, stores: null };

  CREATE_CERTIFICATE_DIALOGS = [
    'instructions',
    'print',
    'securingPassword',
    'verification',
    'completion',
  ];

  state = {
    currentStep: 0,
  }

  render() {
    const { uiDialogs } = this.props.stores;
    let activeDialog = null;

    if (uiDialogs.isOpen(InstructionsDialog)) {
      activeDialog = (
        <InstructionsDialogContainer
          onContinue={this.onContinue}
          onClose={this.onClose}
        />
      );
    }

    if (uiDialogs.isOpen(PasswordChoiceDialog)) {
      activeDialog = (
        <PasswordChoiceDialogContainer
          onContinue={this.onContinue}
          onClose={this.onClose}
          onBack={this.onBack}
        />
      );
    }

    if (uiDialogs.isOpen(PrintDialog)) {
      activeDialog = (
        <PrintDialogContainer
          onContinue={this.onContinue}
        />
      );
    }

    if (uiDialogs.isOpen(SecuringPasswordDialog)) {
      activeDialog = (
        <SecuringPasswordDialogContainer
          onContinue={this.onContinue}
        />
      );
    }

    if (uiDialogs.isOpen(VerificationDialog)) {
      activeDialog = (
        <VerificationDialogContainer
          onContinue={this.onContinue}
        />
      );
    }

    if (uiDialogs.isOpen(CompletionDialog)) {
      activeDialog = (
        <CompletionDialogContainer
          onFinish={this.onClose}
        />
      );
    }

    return activeDialog;
  }

  onContinue = (nextStep: number) => {
    const nextDialog = this.CREATE_CERTIFICATE_DIALOGS[nextStep];
    this.switchDialog(nextDialog);
    this.setState({ currentStep: nextStep });
  };

  onBack = () => {
    const prevStep = this.state.currentStep ? this.state.currentStep - 1 : 0;
    const prevDialog = this.CREATE_CERTIFICATE_DIALOGS[prevStep];
    this.setState({ currentStep: prevStep });
    this.switchDialog(prevDialog);
    this.props.actions.ada.wallets.updateCertificateStep.trigger(true);
  };

  onClose = () => {
    this.setState({ currentStep: 0 });
    this.props.actions.ada.wallets.closeCertificateGeneration.trigger();
  };

  switchDialog = (dialog: string) => {
    switch (dialog) {
      case 'instructions':
        this.props.actions.dialogs.open.trigger({
          dialog: InstructionsDialog,
        });
        break;
      case 'passwordChoice':
        this.props.actions.dialogs.open.trigger({
          dialog: PasswordChoiceDialog,
        });
        break;
      case 'print':
        this.props.actions.dialogs.open.trigger({
          dialog: PrintDialog,
        });
        break;
      case 'securingPassword':
        this.props.actions.dialogs.open.trigger({
          dialog: SecuringPasswordDialog,
        });
        break;
      case 'verification':
        this.props.actions.dialogs.open.trigger({
          dialog: VerificationDialog,
        });
        break;
      case 'completion':
        this.props.actions.dialogs.open.trigger({
          dialog: CompletionDialog,
        });
        break;
      default:
        this.onClose();
    }
  };
}
