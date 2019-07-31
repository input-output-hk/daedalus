// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class HashDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletCreateDialog
        stepNumber={4}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        HASH AND IMAGE DIALOG CONTENT
      </WalletCreateDialog>
    );
  }
}
