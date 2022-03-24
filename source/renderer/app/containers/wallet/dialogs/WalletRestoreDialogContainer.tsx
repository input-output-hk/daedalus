import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StepWalletTypeContainer from './wallet-restore/StepWalletTypeContainer';
import StepMnemonicsContainer from './wallet-restore/StepMnemonicsContainer';
import StepConfigurationContainer from './wallet-restore/StepConfigurationContainer';
import StepSuccessContainer from './wallet-restore/StepSuccessContainer';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { RESTORE_WALLET_STEPS } from '../../../config/walletRestoreConfig';
import ConfirmationDialog from '../../../components/wallet/wallet-restore/widgets/ConfirmationDialog';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class WalletRestoreContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  get containers() {
    return {
      type: StepWalletTypeContainer,
      mnemonics: StepMnemonicsContainer,
      configuration: StepConfigurationContainer,
      success: StepSuccessContainer,
    };
  }

  render() {
    const { stores, actions } = this.props;
    const {
      restoreWalletStep,
      restoreWalletShowAbortConfirmation,
    } = stores.wallets;
    const {
      restoreWalletClose,
      restoreWalletCancelClose,
      restoreWalletChangeStep,
    } = actions.wallets;

    if (restoreWalletStep === null) {
      return null;
    }

    const stepId = RESTORE_WALLET_STEPS[restoreWalletStep];
    const CurrentContainer = this.containers[stepId];
    return (
      <Fragment>
        {restoreWalletShowAbortConfirmation && (
          <ConfirmationDialog
            onConfirm={() => restoreWalletClose.trigger()}
            onCancel={() => restoreWalletCancelClose.trigger()}
          />
        )}
        <CurrentContainer
          onContinue={() => restoreWalletChangeStep.trigger()}
          onBack={() => restoreWalletChangeStep.trigger(true)}
          onClose={() => restoreWalletClose.trigger()}
        />
      </Fragment>
    );
  }
}

export default WalletRestoreContainer;
