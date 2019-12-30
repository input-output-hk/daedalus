// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StepWalletTypeContainer from './wallet-restore/StepWalletTypeContainer';
import StepMnemonicsContainer from './wallet-restore/StepMnemonicsContainer';
import StepConfigurationContainer from './wallet-restore/StepConfigurationContainer';
import StepSuccessContainer from './wallet-restore/StepSuccessContainer';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { RESTORE_WALLET_STEPS } from '../../../config/walletRestoreConfig';

type Props = InjectedProps;

// TODO restore component;
const RestoreWalletAbortConfirmation = () => <div>Are you sure</div>;

@inject('stores', 'actions')
@observer
export default class WalletRestoreContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  get containers() {
    return {
      type: StepWalletTypeContainer,
      mnemonics: StepMnemonicsContainer,
      configuration: StepConfigurationContainer,
      success: StepSuccessContainer,
    };
  }

  get shouldDisplayAbortAlert() {
    return (
      this.currentStep < 0 && this.currentStep > RESTORE_WALLET_STEPS.length
    );
  }

  get currentStep() {
    return this.props.stores.wallets.restoreWalletStep;
  }

  onContinue = () => {
    const { restoreWalletChangeStep } = this.props.actions.wallets;
    if (this.currentStep < RESTORE_WALLET_STEPS.length - 1) {
      restoreWalletChangeStep.trigger();
    }
  };

  onBack = () => {
    this.props.actions.wallets.restoreWalletChangeStep.trigger(true);
  };

  onClose = () => {
    const {
      restoreWalletAbort,
      restoreWalletClose,
      restoreWalletEnd,
    } = this.props.actions.wallets;
    if (this.shouldDisplayAbortAlert) {
      restoreWalletAbort.trigger();
    } else {
      if (this.props.stores.wallets.restoredWallet) {
        restoreWalletEnd.trigger();
      }
      restoreWalletClose.trigger();
    }
  };

  onAbort = () => this.props.actions.wallets.restoreWalletAbort.trigger();

  render() {
    const { wallets } = this.props.stores;
    const { restoreWalletStep, restoreWalletShowAbortConfirmation } = wallets;
    const stepId = RESTORE_WALLET_STEPS[restoreWalletStep];
    const CurrentContainer = this.containers[stepId];
    return (
      <Fragment>
        {restoreWalletShowAbortConfirmation && (
          <RestoreWalletAbortConfirmation onAbort={this.onAbort} />
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
