// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class MnemonicsDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;

    return (
      <WalletCreateDialog
        stepNumber={2}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        MNEMONICS STEP CONTENT
      </WalletCreateDialog>
    );
  }
}
