// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class TemplateDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;

    return (
      <WalletCreateDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
            label: 'Print template',
            onClick: () => {},
          },
          {
            label: 'Continue without template',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        TEMPLATE STEP CONTENT
      </WalletCreateDialog>
    );
  }
}
