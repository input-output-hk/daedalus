// @flow
import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';
import commonStyles from './WalletCreateStyles.scss';

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
        <div className={commonStyles.component}>
          INSTRUCTIONS DIALOG CONTENT
        </div>
      </WalletCreateDialog>
    );
  }
}
