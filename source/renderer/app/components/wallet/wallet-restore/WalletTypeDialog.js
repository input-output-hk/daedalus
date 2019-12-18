// @flow
import React, { Component } from 'react';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './WalletRestoreStyles.scss';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class WalletTypeDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletRestoreDialog
        stepNumber={0}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        <div className={commonStyles.component}>WALLET TYPE STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
