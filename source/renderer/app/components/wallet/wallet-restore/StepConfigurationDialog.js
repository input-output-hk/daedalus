// @flow
import React, { Component } from 'react';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './StepDialogStyles.scss';

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

export default class StepConfigurationDialog extends Component<Props> {
  render() {
    const { onContinue, onClose, onBack } = this.props;
    return (
      <WalletRestoreDialog
        stepNumber={2}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
      >
        <div className={commonStyles.component}>CONFIGURATION STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
