import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TransferFundsStep1Container from './dialogs/transfer-funds/TransferFundsStep1Container';
import TransferFundsStep2Container from './dialogs/transfer-funds/TransferFundsStep2Container';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
class TransferFundsPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets: walletsActions } = actions;
    const { wallets: walletsStore } = stores;
    const {
      transferFundsNextStep,
      transferFundsPrevStep,
      transferFundsClose,
    } = walletsActions;
    const { transferFundsStep } = walletsStore;
    if (!transferFundsStep) return null;
    let Container;

    if (transferFundsStep === 1) {
      Container = TransferFundsStep1Container;
    } else {
      Container = TransferFundsStep2Container;
    }

    return (
      <Container
        onContinue={() => transferFundsNextStep.trigger()}
        onBack={() => transferFundsPrevStep.trigger()}
        onClose={() => transferFundsClose.trigger()}
      />
    );
  }
}

export default TransferFundsPage;
