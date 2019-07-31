// @flow
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
export default class WalletCreateDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

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

  onContinue = () => {
    this.props.actions.wallets.createWalletChangeStep.trigger();
  };

  onBack = () => {
    this.props.actions.wallets.createWalletClose.trigger();
  };

  onClose = () => {
    this.props.actions.wallets.createWalletClose.trigger();
  };

  onAbort = () => {
    this.props.actions.wallets.createWalletAbort.trigger();
  };

  render() {
    const {
      createWalletStep,
      createWalletAbortConfirmation,
    } = this.props.stores.wallets;
    const stepId = CREATE_WALLET_STEPS[createWalletStep];
    const CurrentContainer = this.containers[stepId];
    return (
      <Fragment>
        {createWalletAbortConfirmation && (
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
