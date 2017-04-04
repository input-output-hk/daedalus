// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import styles from './ChangeWalletPasswordDialog.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.changePassword.dialog.title',
    defaultMessage: '!!!Change password',
    description: 'Title for the "Change wallet password" dialog.',
  },
  currentPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordLabel',
    defaultMessage: '!!!Current password',
    description: 'Label for the "Current password" input in the change wallet password dialog.',
  },
  newPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.newPasswordLabel',
    defaultMessage: '!!!New password',
    description: 'Label for the "New password" input in the change wallet password dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the change wallet password dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the change wallet password dialog.',
  },
});

@observer
export default class ChangeWalletPasswordDialog extends Component {

  static propTypes = {
    hasWalletPassword: PropTypes.bool.isRequired,
    onSave: PropTypes.func,
    onCancel: PropTypes.func,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      hasWalletPassword,
      onSave,
      onCancel,
    } = this.props;

    const actions = [
      {
        label: intl.formatMessage(globalMessages.save),
        onClick: onSave,
        primary: true,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        active
        className={styles.dialog}
      >

        {hasWalletPassword ? (
          <Input
            type="password"
            className={styles.topInput}
            label={intl.formatMessage(messages.currentPasswordLabel)}
            placeholder={intl.formatMessage(messages.passwordFieldPlaceholder)}
          />
        ) : null}

        <Input
          type="password"
          className={!hasWalletPassword ? styles.topInput : null}
          label={intl.formatMessage(messages.newPasswordLabel)}
          placeholder={intl.formatMessage(messages.passwordFieldPlaceholder)}
        />

        <Input
          type="password"
          label={intl.formatMessage(messages.repeatPasswordLabel)}
          placeholder={intl.formatMessage(messages.passwordFieldPlaceholder)}
        />

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
