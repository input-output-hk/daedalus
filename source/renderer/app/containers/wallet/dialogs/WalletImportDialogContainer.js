// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { IMPORT_WALLET_STEPS } from '../../../config/walletRestoreConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';
import StepWalletFileImportContainer from './wallet-import/StepWalletFileImportContainer';
import StepWalletSelectImportContainer from './wallet-import/StepWalletSelectImportContainer';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<
  Props,
  State
> {
  static defaultProps = { actions: null, stores: null };

  get containers() {
    return {
      walletImportFile: StepWalletFileImportContainer,
      walletSelectImport: StepWalletSelectImportContainer,
    };
  }

  componentWillUnmount() {
    this.props.actions.walletMigration.finishMigration.trigger();
  }

  onConfirm = () => {
    this.props.actions.walletMigration.nextStep.trigger();
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores, actions } = this.props;
    const { walletMigration } = stores;
    const { walletMigrationStep } = walletMigration;
    const { nextStep, resetMigration } = actions.walletMigration;
    const stepId = IMPORT_WALLET_STEPS[walletMigrationStep];
    const CurrentContainer = this.containers[stepId];

    return (
      <CurrentContainer
        onContinue={() => nextStep.trigger()}
        onClose={() => resetMigration.trigger()}
      />
    );
  }
}
