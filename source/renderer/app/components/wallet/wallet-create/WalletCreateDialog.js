// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import WalletCreateSteps from './WalletCreateSteps';
import styles from './WalletCreateDialog.scss';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
});

type Props = {
  stepNumber: number,
  actions: Array<DialogAction>,
  primaryButtonAutoFocus?: boolean,
  children: Node,
};

class WalletCreateHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { actions, children, stepNumber } = this.props;

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={() => {}}
        closeButton={<DialogCloseButton />}
      >
        <WalletCreateSteps stepNumber={stepNumber} />
        {children}
      </Dialog>
    );
  }
}

export default WalletCreateHeader;
