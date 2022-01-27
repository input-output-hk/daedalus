import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import PrintDialog from '../../../../components/wallet/paper-wallet-certificate/PrintDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('actions')
@observer
class PrintDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onContinue = () => {
    this.props.actions.wallets.updateCertificateStep.trigger();
  };

  render() {
    return (
      <PrintDialog onContinue={this.onContinue} onClose={this.props.onClose} />
    );
  }
}

export default PrintDialogContainer;
