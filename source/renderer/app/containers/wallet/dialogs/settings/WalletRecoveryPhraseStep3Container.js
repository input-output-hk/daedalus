// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseStep2Container extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { closeActiveDialog } = this.props.actions.dialogs;
    return (
      <WalletRecoveryPhraseStep3Dialog onClose={closeActiveDialog.trigger} />
    );
  }
}
