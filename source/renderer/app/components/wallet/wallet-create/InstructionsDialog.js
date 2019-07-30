// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';
// import WalletCreateFooter from './WalletCreateFooter';

type Props = {
  onContinue: Function,
  onAcceptTermsAndConditions: Function,
};

export default class InstructionsDialog extends Component<Props> {
  render() {
    const { onAcceptTermsAndConditions, onContinue } = this.props;

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
      >
        <button onClick={onAcceptTermsAndConditions}>
          onAcceptTermsAndConditions
        </button>
      </WalletCreateDialog>
    );
  }
}
