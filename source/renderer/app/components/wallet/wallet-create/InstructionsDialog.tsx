import React, { Component } from 'react';
import WalletCreateDialog from './WalletCreateDialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletCreateStyles.scss' or ... Remove this comment to see the full error message
import commonStyles from './WalletCreateStyles.scss';

type Props = {
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  isVideoWatched?: boolean;
};
export default class InstructionsDialog extends Component<Props> {
  render() {
    const { onContinue, onClose, isVideoWatched } = this.props;
    return (
      <WalletCreateDialog
        stepNumber={0}
        actions={[
          {
            primary: true,
            label: !isVideoWatched ? 'Skip video and continue' : 'Continue',
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
