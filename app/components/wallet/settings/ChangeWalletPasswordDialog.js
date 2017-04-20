// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import Input from 'react-toolbox/lib/input/Input';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import styles from './ChangeWalletPasswordDialog.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  dialogTitleSetPassword: {
    id: 'wallet.settings.changePassword.dialog.title.setPassword',
    defaultMessage: '!!!Password',
    description: 'Title for the "Change wallet password" dialog when there is no password set.',
  },
  dialogTitleChangePassword: {
    id: 'wallet.settings.changePassword.dialog.title.changePassword',
    defaultMessage: '!!!Change password',
    description: 'Title for the "Change wallet password" dialog when there is already password set.',
  },
  walletPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the change wallet password dialog.',
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

  props: {
    hasWalletPassword: boolean,
    currentPasswordValue: string,
    newPasswordValue: string,
    repeatedPasswordValue: string,
    onSave: Function,
    onCancel: Function,
    onDataChange: Function,
  };

  static defaultProps = {
    currentPasswordValue: '',
    newPasswordValue: '',
    repeatedPasswordValue: '',
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleDataChange = (key: string, value: string) => {
    this.props.onDataChange({ [key]: value });
  };

  render() {
    const { intl } = this.context;
    const {
      hasWalletPassword,
      onSave,
      onCancel,
      currentPasswordValue,
      newPasswordValue,
      repeatedPasswordValue,
    } = this.props;

    const isCurrentPasswordBlank = currentPasswordValue.length === 0;
    const isNewPasswordBlank = newPasswordValue.length === 0;
    const isRepeatedPasswordCorrect = repeatedPasswordValue === newPasswordValue;

    const actions = [
      {
        label: intl.formatMessage(globalMessages.save),
        onClick: () => {
          onSave({ oldPassword: currentPasswordValue, newPassword: newPasswordValue });
        },
        disabled: (
          (hasWalletPassword && isCurrentPasswordBlank) ||
          isNewPasswordBlank || !isRepeatedPasswordCorrect
        ),
        primary: true,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(
          !hasWalletPassword ? messages.dialogTitleSetPassword : messages.dialogTitleChangePassword
        )}
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
            value={currentPasswordValue}
            onChange={(value) => this.handleDataChange('currentPasswordValue', value)}
          />
        ) : null}

        <Input
          type="password"
          className={!hasWalletPassword ? styles.topInput : null}
          label={intl.formatMessage(
            !hasWalletPassword ? messages.walletPasswordLabel : messages.newPasswordLabel
          )}
          placeholder={intl.formatMessage(messages.passwordFieldPlaceholder)}
          value={newPasswordValue}
          onChange={(value) => this.handleDataChange('newPasswordValue', value)}
        />

        <Input
          type="password"
          label={intl.formatMessage(messages.repeatPasswordLabel)}
          placeholder={intl.formatMessage(messages.passwordFieldPlaceholder)}
          value={repeatedPasswordValue}
          onChange={(value) => this.handleDataChange('repeatedPasswordValue', value)}
        />

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
