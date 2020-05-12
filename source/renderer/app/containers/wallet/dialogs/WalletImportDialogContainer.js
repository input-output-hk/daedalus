// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { IMPORT_WALLET_STEPS } from '../../../config/walletRestoreConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';
import WalletFileImportStepContainer from './wallet-import/WalletFileImportStepContainer';
import WalletSelectImportStepContainer from './wallet-import/WalletSelectImportStepContainer';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  get containers() {
    return {
      walletImportFile: WalletFileImportStepContainer,
      walletSelectImport: WalletSelectImportStepContainer,
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
    const migrationStep = IMPORT_WALLET_STEPS[walletMigrationStep];
    const CurrentContainer = this.containers[migrationStep];

    return (
      <CurrentContainer
        onContinue={() => nextStep.trigger()}
        onClose={() => resetMigration.trigger()}
      />
    );
  }
}
