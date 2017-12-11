// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import { isValidWalletPassword, isValidRepeatPassword } from '../../../utils/validations';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './ChangeWalletPasswordDialog.scss';

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
  currentPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordFieldPlaceholder',
    defaultMessage: '!!!Type current password',
    description: 'Placeholder for the "Current password" inputs in the change wallet password dialog.',
  },
  newPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.newPasswordFieldPlaceholder',
    defaultMessage: '!!!Type new password',
    description: 'Placeholder for the "New password" inputs in the change wallet password dialog.',
  },
  repeatPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordFieldPlaceholder',
    defaultMessage: '!!!Repeat new password',
    description: 'Placeholder for the "Repeat password" inputs in the change wallet password dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.settings.changePassword.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Remove password',
    description: 'Label for the "Check to deactivate password" switch in the change wallet password dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Check to deactivate password',
    description: 'Text for the "Check to deactivate password" switch in the change wallet password dialog.',
  },
});

type Props = {
  isWalletPasswordSet: boolean,
  currentPasswordValue: string,
  newPasswordValue: string,
  repeatedPasswordValue: string,
  onSave: Function,
  onCancel: Function,
  onDataChange: Function,
  onPasswordSwitchToggle: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
};

type State = {
  removePassword: boolean,
};

@observer
export default class ChangeWalletPasswordDialog extends Component<Props, State> {

  static defaultProps = {
    currentPasswordValue: '',
    newPasswordValue: '',
    repeatedPasswordValue: '',
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    removePassword: false,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      currentPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.currentPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.currentPasswordFieldPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (!this.props.isWalletPasswordSet) return [true];
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
        }],
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages[
          this.props.isWalletPasswordSet ? 'newPasswordLabel' : 'walletPasswordLabel'
        ]),
        placeholder: this.context.intl.formatMessage(messages.newPasswordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (this.state.removePassword) return [true];
          const repeatPasswordField = form.$('repeatPassword');
          if (repeatPasswordField.value.length > 0) repeatPasswordField.validate(form);
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
        }],
      },
      repeatPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.repeatPasswordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (this.state.removePassword) return [true];
          const walletPassword = form.$('walletPassword').value;
          if (walletPassword.length === 0) return [true];
          return [
            isValidRepeatPassword(walletPassword, field.value),
            this.context.intl.formatMessage(globalMessages.invalidRepeatPassword)
          ];
        }],
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { removePassword } = this.state;
        const { currentPassword, walletPassword } = form.values();
        const passwordData = {
          oldPassword: currentPassword || null,
          newPassword: removePassword ? null : walletPassword,
        };
        this.props.onSave(passwordData);
      },
      onError: () => {},
    });
  };

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ removePassword: value });
    this.props.onPasswordSwitchToggle();
  };

  handleDataChange = (key: string, value: string) => {
    this.props.onDataChange({ [key]: value });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      isWalletPasswordSet,
      onCancel,
      currentPasswordValue,
      newPasswordValue,
      repeatedPasswordValue,
      isSubmitting,
      error,
    } = this.props;
    const { removePassword } = this.state;

    const dialogClasses = classnames([
      isWalletPasswordSet ? 'changePasswordDialog' : 'createPasswordDialog',
      styles.dialog,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      removePassword ? styles.hidden : null
    ]);

    const confirmButtonClasses = classnames([
      'confirmButton',
      removePassword ? styles.removeButton : null,
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const newPasswordClasses = classnames([
      'newPassword',
      styles.newPassword,
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages[removePassword ? 'remove' : 'save']),
        onClick: this.submit,
        primary: true,
        className: confirmButtonClasses,
      },
    ];

    const currentPasswordField = form.$('currentPassword');
    const newPasswordField = form.$('walletPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    return (
      <Dialog
        title={intl.formatMessage(
          messages[!isWalletPasswordSet ? 'dialogTitleSetPassword' : 'dialogTitleChangePassword']
        )}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : null}
        className={dialogClasses}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >

        {isWalletPasswordSet ? (
          <div className={styles.walletPassword}>
            <div className={styles.walletPasswordSwitch}>
              <div className={styles.passwordLabel}>
                {intl.formatMessage(messages.passwordSwitchLabel)}
              </div>
              <Checkbox
                onChange={this.handlePasswordSwitchToggle}
                label={intl.formatMessage(messages.passwordSwitchPlaceholder)}
                checked={removePassword}
                skin={<SimpleSwitchSkin />}
              />
            </div>

            <Input
              type="password"
              className="currentPassword"
              value={currentPasswordValue}
              onChange={(value) => this.handleDataChange('currentPasswordValue', value)}
              {...currentPasswordField.bind()}
              error={currentPasswordField.error}
              skin={<SimpleInputSkin />}
            />
          </div>
        ) : null}

        <div className={walletPasswordFieldsClasses}>
          <Input
            type="password"
            className={newPasswordClasses}
            value={newPasswordValue}
            onChange={(value) => this.handleDataChange('newPasswordValue', value)}
            {...newPasswordField.bind()}
            error={newPasswordField.error}
            skin={<SimpleInputSkin />}
          />

          <Input
            type="password"
            className="repeatedPassword"
            value={repeatedPasswordValue}
            onChange={(value) => this.handleDataChange('repeatedPasswordValue', value)}
            {...repeatedPasswordField.bind()}
            error={repeatedPasswordField.error}
            skin={<SimpleInputSkin />}
          />

          <p className={styles.passwordInstructions}>
            {intl.formatMessage(globalMessages.passwordInstructions)}
          </p>
        </div>

        {error ? <p className={styles.error}>{intl.formatMessage(error)}</p> : null}

      </Dialog>
    );
  }

}
