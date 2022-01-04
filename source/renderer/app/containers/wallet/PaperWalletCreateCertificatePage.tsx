import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import InstructionsDialog from '../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import InstructionsDialogContainer from './dialogs/paper-wallet-certificate/InstructionsDialogContainer';
import PrintDialog from '../../components/wallet/paper-wallet-certificate/PrintDialog';
import PrintDialogContainer from './dialogs/paper-wallet-certificate/PrintDialogContainer';
import SecuringPasswordDialog from '../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import SecuringPasswordDialogContainer from './dialogs/paper-wallet-certificate/SecuringPasswordDialogContainer';
import VerificationDialog from '../../components/wallet/paper-wallet-certificate/VerificationDialog';
import VerificationDialogContainer from './dialogs/paper-wallet-certificate/VerificationDialogContainer';
import CompletionDialog from '../../components/wallet/paper-wallet-certificate/CompletionDialog';
import CompletionDialogContainer from './dialogs/paper-wallet-certificate/CompletionDialogContainer';
import ConfirmationDialog from '../../components/wallet/paper-wallet-certificate/ConfirmationDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;
type State = {
  currentStep: number | null | undefined;
  showConfirmationDialog: boolean;
};

@inject('actions', 'stores')
@observer
class PaperWalletCreateCertificatePage extends Component<Props, State> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  componentDidUpdate() {
    const stepChanged =
      this.props.stores.wallets.certificateStep !== this.state.currentStep;

    if (this.props.stores.wallets.certificateStep && stepChanged) {
      this.onContinue(this.props.stores.wallets.certificateStep);
    }
  }

  CREATE_CERTIFICATE_DIALOGS = [
    'instructions',
    'print',
    'securingPassword',
    'verification',
    'completion',
  ];
  state = {
    currentStep: 0,
    showConfirmationDialog: false,
  };

  render() {
    const { uiDialogs, app } = this.props.stores;
    const { showConfirmationDialog } = this.state;
    const { environment: network } = app;
    let activeDialog = null;

    if (uiDialogs.isOpen(InstructionsDialog)) {
      activeDialog = (
        <InstructionsDialogContainer
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue={this.onContinue}
          onClose={this.onClose}
        />
      );
    }

    if (uiDialogs.isOpen(PrintDialog)) {
      activeDialog = (
        <PrintDialogContainer
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue={this.onContinue}
          onClose={this.showConfirmationDialog}
        />
      );
    }

    if (uiDialogs.isOpen(SecuringPasswordDialog)) {
      activeDialog = (
        <SecuringPasswordDialogContainer
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue={this.onContinue}
          onClose={this.showConfirmationDialog}
        />
      );
    }

    if (uiDialogs.isOpen(VerificationDialog)) {
      activeDialog = (
        <VerificationDialogContainer
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onContinue={this.onContinue}
          onClose={this.showConfirmationDialog}
        />
      );
    }

    if (uiDialogs.isOpen(CompletionDialog)) {
      activeDialog = <CompletionDialogContainer onClose={this.onClose} />;
    }

    return (
      <div>
        {activeDialog}
        {showConfirmationDialog && (
          <ConfirmationDialog
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            network={network}
            onCancel={this.hideConfirmationDialog}
            onConfirm={this.onClose}
          />
        )}
      </div>
    );
  }

  onContinue = (nextStep: number) => {
    const nextDialog = this.CREATE_CERTIFICATE_DIALOGS[nextStep];
    this.switchDialog(nextDialog);
    this.setState({
      currentStep: nextStep,
    });
  };
  onBack = () => {
    // eslint-disable-next-line react/no-access-state-in-setstate
    const prevStep = this.state.currentStep ? this.state.currentStep - 1 : 0;
    const prevDialog = this.CREATE_CERTIFICATE_DIALOGS[prevStep];
    this.setState({
      currentStep: prevStep,
    });
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
    this.setState({
      showConfirmationDialog: true,
    });
  };
  hideConfirmationDialog = () => {
    this.setState({
      showConfirmationDialog: false,
    });
  };
  switchDialog = (dialog: string) => {
    switch (dialog) {
      case 'instructions':
        this.props.actions.dialogs.open.trigger({
          dialog: InstructionsDialog,
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

export default PaperWalletCreateCertificatePage;
