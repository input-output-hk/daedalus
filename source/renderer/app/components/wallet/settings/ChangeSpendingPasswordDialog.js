// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import {
  isValidSpendingPassword,
  isValidRepeatPassword,
} from '../../../utils/validations';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './ChangeSpendingPasswordDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';

const messages = defineMessages({
  dialogTitleSetPassword: {
    id: 'wallet.settings.changePassword.dialog.title.setPassword',
    defaultMessage: '!!!Password',
    description:
      'Title for the "Change wallet password" dialog when there is no password set.',
  },
  dialogTitleChangePassword: {
    id: 'wallet.settings.changePassword.dialog.title.changePassword',
    defaultMessage: '!!!Change password',
    description:
      'Title for the "Change wallet password" dialog when there is already password set.',
  },
  spendingPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description:
      'Label for the "Spending password" input in the change wallet password dialog.',
  },
  currentPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordLabel',
    defaultMessage: '!!!Current password',
    description:
      'Label for the "Current password" input in the change wallet password dialog.',
  },
  newPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.newPasswordLabel',
    defaultMessage: '!!!New password',
    description:
      'Label for the "New password" input in the change wallet password dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the change wallet password dialog.',
  },
  currentPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordFieldPlaceholder',
    defaultMessage: '!!!Type current password',
    description:
      'Placeholder for the "Current password" inputs in the change wallet password dialog.',
  },
  newPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.newPasswordFieldPlaceholder',
    defaultMessage: '!!!Type new password',
    description:
      'Placeholder for the "New password" inputs in the change wallet password dialog.',
  },
  repeatPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordFieldPlaceholder',
    defaultMessage: '!!!Repeat new password',
    description:
      'Placeholder for the "Repeat password" inputs in the change wallet password dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.settings.changePassword.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Remove password',
    description:
      'Label for the "Check to deactivate password" switch in the change wallet password dialog.',
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Check to deactivate password',
    description:
      'Text for the "Check to deactivate password" switch in the change wallet password dialog.',
  },
});

type Props = {
  isSpendingPasswordSet: boolean,
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
export default class ChangeSpendingPasswordDialog extends Component<
  Props,
  State
> {
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

  form = new ReactToolboxMobxForm(
    {
      fields: {
        currentPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.currentPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.currentPasswordFieldPlaceholder
          ),
          value: '',
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages[
              this.props.isSpendingPasswordSet
                ? 'newPasswordLabel'
                : 'spendingPasswordLabel'
            ]
          ),
          placeholder: this.context.intl.formatMessage(
            messages.newPasswordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              if (this.state.removePassword) return [true];
              const repeatPasswordField = form.$('repeatPassword');
              if (repeatPasswordField.value.length > 0) {
                repeatPasswordField.validate({ showErrors: true });
              }
              return [
                isValidSpendingPassword(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidSpendingPassword
                ),
              ];
            },
          ],
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.repeatPasswordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              if (this.state.removePassword) return [true];
              const spendingPassword = form.$('spendingPassword').value;
              if (spendingPassword.length === 0) return [true];
              return [
                isValidRepeatPassword(spendingPassword, field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidRepeatPassword
                ),
              ];
            },
          ],
        },
      },
    },
    {
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { removePassword } = this.state;
        const { currentPassword, spendingPassword } = form.values();
        const passwordData = {
          oldPassword: currentPassword || null,
          newPassword: removePassword ? null : spendingPassword,
        };
        this.props.onSave(passwordData);
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

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
      isSpendingPasswordSet,
      onCancel,
      currentPasswordValue,
      newPasswordValue,
      repeatedPasswordValue,
      isSubmitting,
      error,
    } = this.props;
    const { removePassword } = this.state;

    const dialogClasses = classnames([
      isSpendingPasswordSet ? 'changePasswordDialog' : 'createPasswordDialog',
      styles.dialog,
    ]);

    const spendingPasswordFieldsClasses = classnames([
      styles.spendingPasswordFields,
      removePassword ? styles.hidden : null,
    ]);

    const confirmButtonClasses = classnames([
      'confirmButton',
      removePassword ? 'attention' : null,
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const newPasswordClasses = classnames(['newPassword', styles.newPassword]);

    const actions = [
      {
        label: intl.formatMessage(
          globalMessages[removePassword ? 'remove' : 'save']
        ),
        onClick: this.submit,
        primary: true,
        className: confirmButtonClasses,
      },
    ];

    const currentPasswordField = form.$('currentPassword');
    const newPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    return (
      <Dialog
        title={intl.formatMessage(
          messages[
            !isSpendingPasswordSet
              ? 'dialogTitleSetPassword'
              : 'dialogTitleChangePassword'
          ]
        )}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => {}}
        className={dialogClasses}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >
        {isSpendingPasswordSet ? (
          <div className={styles.spendingPassword}>
            <div className={styles.spendingPasswordSwitch}>
              <div className={styles.passwordLabel}>
                {intl.formatMessage(messages.passwordSwitchLabel)}
              </div>
              <Checkbox
                onChange={this.handlePasswordSwitchToggle}
                label={intl.formatMessage(messages.passwordSwitchPlaceholder)}
                checked={removePassword}
                themeId={IDENTIFIERS.SWITCH}
                skin={SwitchSkin}
              />
            </div>

            <Input
              type="password"
              className="currentPassword"
              label={currentPasswordField.label}
              value={currentPasswordValue}
              onKeyPress={this.handleSubmitOnEnter}
              onChange={value =>
                this.handleDataChange('currentPasswordValue', value)
              }
              {...currentPasswordField.bind()}
              error={currentPasswordField.error}
              skin={InputSkin}
            />
          </div>
        ) : null}

        <div className={spendingPasswordFieldsClasses}>
          <Input
            type="password"
            className={newPasswordClasses}
            label={newPasswordField.label}
            value={newPasswordValue}
            onKeyPress={this.handleSubmitOnEnter}
            onChange={value => this.handleDataChange('newPasswordValue', value)}
            {...newPasswordField.bind()}
            error={newPasswordField.error}
            skin={InputSkin}
          />

          <Input
            type="password"
            className="repeatedPassword"
            label={repeatedPasswordField.label}
            value={repeatedPasswordValue}
            onKeyPress={this.handleSubmitOnEnter}
            onChange={value =>
              this.handleDataChange('repeatedPasswordValue', value)
            }
            {...repeatedPasswordField.bind()}
            error={repeatedPasswordField.error}
            skin={InputSkin}
          />

          <p className={styles.passwordInstructions}>
            <FormattedHTMLMessage {...globalMessages.passwordInstructions} />
          </p>
        </div>

        {error ? (
          <p className={styles.error}>{intl.formatMessage(error)}</p>
        ) : null}
      </Dialog>
    );
  }
}
