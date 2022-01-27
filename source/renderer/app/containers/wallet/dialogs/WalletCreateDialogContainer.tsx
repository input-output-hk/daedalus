import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialogContainer from './wallet-create/InstructionsDialogContainer';
import TemplateDialogContainer from './wallet-create/TemplateDialogContainer';
import MnemonicsDialogContainer from './wallet-create/MnemonicsDialogContainer';
import ValidateDialogContainer from './wallet-create/ValidateDialogContainer';
import HashDialogContainer from './wallet-create/HashDialogContainer';
import ConfigDialogContainer from './wallet-create/ConfigDialogContainer';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { CREATE_WALLET_STEPS } from '../../../config/walletsConfig';

type Props = InjectedProps;

// TODO create component;
const CreateWalletAbortConfirmation = () => <div>Are you sure</div>;

@inject('stores', 'actions')
@observer
class WalletCreateDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  get containers() {
    return {
      instructions: InstructionsDialogContainer,
      template: TemplateDialogContainer,
      mnemonics: MnemonicsDialogContainer,
      validate: ValidateDialogContainer,
      hashImage: HashDialogContainer,
      config: ConfigDialogContainer,
    };
  }

  get shouldDisplayAbortAlert() {
    return (
      this.currentStep !== null &&
      (this.currentStep < 0 || this.currentStep > CREATE_WALLET_STEPS.length)
    );
  }

  get currentStep() {
    return this.props.stores.wallets.createWalletStep;
  }

  onContinue = () => {
    const {
      createWalletChangeStep,
      createWalletClose,
    } = this.props.actions.wallets;

    if (this.currentStep !== null) {
      if (this.currentStep < CREATE_WALLET_STEPS.length - 1) {
        createWalletChangeStep.trigger();
      } else {
        createWalletClose.trigger();
      }
    }
  };
  onBack = () => {
    this.props.actions.wallets.createWalletChangeStep.trigger(true);
  };
  onClose = () => {
    const { createWalletAbort, createWalletClose } = this.props.actions.wallets;

    if (this.shouldDisplayAbortAlert) {
      createWalletAbort.trigger();
    } else {
      createWalletClose.trigger();
    }
  };
  onAbort = () => this.props.actions.wallets.createWalletAbort.trigger();

  render() {
    const {
      createWalletStep,
      createWalletShowAbortConfirmation,
    } = this.props.stores.wallets;

    if (createWalletStep === null) {
      return null;
    }

    const stepId = CREATE_WALLET_STEPS[createWalletStep];
    const CurrentContainer = this.containers[stepId];
    return (
      <Fragment>
        {createWalletShowAbortConfirmation && (
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ onAbort: () => any; }' is not assignable t... Remove this comment to see the full error message
          <CreateWalletAbortConfirmation onAbort={this.onAbort} />
        )}
        <CurrentContainer
          onContinue={this.onContinue}
          onBack={this.onBack}
          onClose={this.onClose}
        />
      </Fragment>
    );
  }
}

export default WalletCreateDialogContainer;
