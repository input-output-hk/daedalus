// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

type State = {
  safetyAgreement: boolean,
};

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseStep2Container extends Component<
  Props,
  State
> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  state = {
    safetyAgreement: false,
  };

  handleToggleSafetyAgreement = checked => {
    this.setState({
      safetyAgreement: checked,
    });
  };

  render() {
    const { closeActiveDialog } = this.props.actions.dialogs;
    const { safetyAgreement } = this.state;
    return (
      <WalletRecoveryPhraseStep3Dialog
        onClose={closeActiveDialog.trigger}
        safetyAgreement={safetyAgreement}
        onToggleSafetyAgreement={this.handleToggleSafetyAgreement}
      />
    );
  }
}
