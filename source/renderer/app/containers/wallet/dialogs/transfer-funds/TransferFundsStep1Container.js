// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TransferFundsStep1Dialog from '../../../../components/wallet/transfer-funds/TransferFundsStep1Dialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores')
@observer
export default class TransferFundsStep1Container extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, onClose, onContinue } = this.props;
    const { wallets: walletsStore } = stores;
    return (
      <TransferFundsStep1Dialog
        wallets={walletsStore.all}
        onClose={onClose}
        onContinue={onContinue}
      />
    );
  }
}
