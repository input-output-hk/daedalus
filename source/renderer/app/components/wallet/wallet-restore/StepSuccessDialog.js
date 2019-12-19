// @flow
import React, { Component } from 'react';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './StepDialogStyles.scss';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class StepSuccessDialog extends Component<Props> {
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
        <div className={commonStyles.component}>SUCCESS STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
