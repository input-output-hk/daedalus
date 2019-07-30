// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/wallet-create/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  handleAcceptTermsAndConditions = () => {};

  render() {
    return (
      <InstructionsDialog
        onAcceptTermsAndConditions={this.handleAcceptTermsAndConditions}
      />
    );
  }
}
