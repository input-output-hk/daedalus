// @flow
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { observer, inject } from 'mobx-react';
import PasswordChoiceDialog from '../../../../components/wallet/paper-wallet-certificate/PasswordChoiceDialog';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  onClose: Function,
  onBack: Function,
};

@inject('stores', 'actions') @observer
export default class PasswordChoiceDialogContainer extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  onContinue = (values: { password: string, repeatPassword: string }) => {
    this.context.intl
    const data = {
      ...values,
      intl: this.context.intl,
    }
    console.info('DATA: ', data);
    this.props.actions.ada.wallets.generateCertificate.trigger(data);
  };

  render() {
    const { stores } = this.props;
    const { wallets } = stores.ada;

    return (
      <PasswordChoiceDialog
        inProgress={wallets.generatingCertificateInProgress}
        onContinue={this.onContinue}
        onClose={this.props.onClose}
        onBack={this.props.onBack}
      />
    );
  }
}
