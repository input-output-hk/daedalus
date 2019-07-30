// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/wallet-create/InstructionsDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;

@inject('stores', 'actions')
@observer
export default class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
    onContinue: () => {},
  };

  handleAcceptTermsAndConditions = () => {};

  render() {
    const { onClose, onContinue } = this.props;
    return (
      <InstructionsDialog
        onClose={onClose}
        onContinue={onContinue}
        onAcceptTermsAndConditions={this.handleAcceptTermsAndConditions}
      />
    );
  }
}
