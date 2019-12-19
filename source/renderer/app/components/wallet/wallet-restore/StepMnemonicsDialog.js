// @flow
import React, { Component } from 'react';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './StepDialogStyles.scss';

type Props = {
  onContinue: Function,
  onClose: Function,
};

export default class MnemonicsDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletRestoreDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        <div className={commonStyles.component}>MNEMONICS STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
