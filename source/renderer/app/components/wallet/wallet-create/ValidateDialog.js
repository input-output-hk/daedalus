// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class ValidateDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;

    return (
      <WalletCreateDialog
        stepNumber={3}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        VALIDATE STEP CONTENT
      </WalletCreateDialog>
    );
  }
}
