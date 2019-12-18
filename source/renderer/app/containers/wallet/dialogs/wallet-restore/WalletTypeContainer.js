// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletTypeDialog from '../../../../components/wallet/wallet-restore/WalletTypeDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class WalletTypeDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, onContinue } = this.props;
    return <WalletTypeDialog onClose={onClose} onContinue={onContinue} />;
  }
}
