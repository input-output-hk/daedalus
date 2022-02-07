import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletCreateStyles.scss' or ... Remove this comment to see the full error message
import commonStyles from './WalletCreateStyles.scss';

type Props = {
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
};
export default class ValidateDialog extends Component<Props> {
  render() {
    const { onContinue, onClose } = this.props;
    return (
      <WalletCreateDialog
        stepNumber={3}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
      >
        <div className={commonStyles.component}>VALIDATE STEP CONTENT</div>
      </WalletCreateDialog>
    );
  }
}
