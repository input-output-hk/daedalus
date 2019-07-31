// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class InstructionsDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletCreateDialog
        stepNumber={0}
        actions={[
          {
            primary: true,
            label: 'Skip video and continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        INSTRUCTIONS DIALOG CONTENT
      </WalletCreateDialog>
    );
  }
}
