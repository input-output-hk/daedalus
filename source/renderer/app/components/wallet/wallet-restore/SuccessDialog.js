// @flow
import React, { Component } from 'react';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class SuccessDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletRestoreDialog
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        <div>SUCCESS STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
