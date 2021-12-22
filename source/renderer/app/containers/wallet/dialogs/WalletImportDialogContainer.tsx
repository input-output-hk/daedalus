import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../../types/injectedPropsType';
import WalletFileImportStepContainer from './wallet-import/WalletFileImportStepContainer';
import WalletSelectImportStepContainer from './wallet-import/WalletSelectImportStepContainer';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class WalletImportDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  get containers() {
    return {
      WalletImportFile: WalletFileImportStepContainer,
      WalletSelectImport: WalletSelectImportStepContainer,
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
    const CurrentContainer = walletMigrationStep
      ? this.containers[walletMigrationStep]
      : WalletFileImportStepContainer;
    return (
      <CurrentContainer
        onContinue={() => nextStep.trigger()}
        onClose={() => resetMigration.trigger()}
      />
    );
  }
}

export default WalletImportDialogContainer;
