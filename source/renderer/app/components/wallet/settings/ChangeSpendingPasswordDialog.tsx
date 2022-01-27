import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import {
  isValidSpendingPassword,
  isValidRepeatPassword,
} from '../../../utils/validations';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { PasswordInput } from '../../widgets/forms/PasswordInput';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ChangeSpendingPasswordDialog... Remove this comment to see the full error message
import styles from './ChangeSpendingPasswordDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/info-ic... Remove this comment to see the full error message
import infoIconInline from '../../../assets/images/info-icon.inline.svg';

const messages = defineMessages({
  dialogTitleSetPassword: {
    id: 'wallet.settings.changePassword.dialog.title.setPassword',
    defaultMessage: '!!!Set a password for {walletName} wallet',
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
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
type Props = {
  currentPasswordValue: string;
  newPasswordValue: string;
  repeatedPasswordValue: string;
  onSave: (...args: Array<any>) => any;
  onCancel: (...args: Array<any>) => any;
  onDataChange: (...args: Array<any>) => any;
  isSubmitting: boolean;
  error: LocalizableError | null | undefined;
  isSpendingPasswordSet: boolean;
  walletName: string;
  currentLocale: string;
};

@observer
class ChangeSpendingPasswordDialog extends Component<Props> {
  static defaultProps = {
    currentPasswordValue: '',
    newPasswordValue: '',
    repeatedPasswordValue: '',
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        currentPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.currentPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.currentPasswordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ form }) => {
              if (this.props.isSpendingPasswordSet) {
                const currentPasswordField = form.$('currentPassword');
                return [
                  currentPasswordField.value.length > 0,
                  this.context.intl.formatMessage(
                    globalMessages.invalidSpendingPassword
                  ),
                ];
              }

              return [true];
            },
          ],
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
              const repeatPasswordField = form.$('repeatPassword');
              const isRepeatPasswordFieldSet =
                repeatPasswordField.value.length > 0;
              repeatPasswordField.validate({
                showErrors: isRepeatPasswordFieldSet,
              });
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
              const spendingPassword = form.$('spendingPassword').value;
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
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
        showErrorsOnClear: true,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { currentPassword, spendingPassword } = form.values();
        const passwordData = {
          oldPassword: currentPassword,
          newPassword: spendingPassword,
        };
        this.props.onSave(passwordData);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);
  handleDataChange = (key: string, value: string) => {
    this.props.onDataChange({
      [key]: value,
    });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      onCancel,
      isSubmitting,
      error,
      isSpendingPasswordSet,
      walletName,
      currentLocale,
    } = this.props;
    const dialogClasses = classnames([
      styles.dialog,
      isSpendingPasswordSet ? 'changePasswordDialog' : 'createPasswordDialog',
    ]);
    const confirmButtonClasses = classnames([
      'confirmButton',
      isSubmitting ? styles.isSubmitting : null,
    ]);
    const spendingPasswordClasses = classnames([
      styles.spendingPasswordField,
      currentLocale === 'ja-JP' ? styles.jpLangTooltipIcon : '',
    ]);
    const newPasswordClasses = classnames(['newPassword', styles.newPassword]);
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const currentPasswordField = form.$('currentPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const newPasswordField = form.$('spendingPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const repeatedPasswordField = form.$('repeatPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
    const canSubmit = !isSubmitting && form.isValid;
    const currentPasswordError =
      // @ts-ignore ts-migrate(2339) FIXME: Property 'code' does not exist on type 'Localizabl... Remove this comment to see the full error message
      canSubmit && error && error.code === 'wrong_encryption_passphrase';
    const actions = [
      {
        className: confirmButtonClasses,
        disabled: !canSubmit,
        label: intl.formatMessage(globalMessages.save),
        onClick: this.submit,
        primary: true,
      },
    ];
    return (
      <Dialog
        title={intl.formatMessage(
          messages[
            !isSpendingPasswordSet
              ? 'dialogTitleSetPassword'
              : 'dialogTitleChangePassword'
          ],
          {
            walletName,
          }
        )}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => {}}
        className={dialogClasses}
        closeButton={<DialogCloseButton onClose={onCancel} />}
      >
        <div className={styles.spendingPasswordFields}>
          {isSpendingPasswordSet && (
            <div className={spendingPasswordClasses}>
              <Input
                className={styles.currentPassword}
                error={currentPasswordField.error || currentPasswordError}
                {...currentPasswordField.bind()}
                onKeyPress={this.handleSubmitOnEnter}
              />
              <PopOver
                content={<FormattedHTMLMessage {...messages.passwordTooltip} />}
                key="tooltip"
              >
                <SVGInline svg={infoIconInline} className={styles.infoIcon} />
              </PopOver>
            </div>
          )}

          <div className={newPasswordClasses}>
            <div className={spendingPasswordClasses}>
              <PasswordInput
                {...newPasswordField.bind()}
                onKeyPress={this.handleSubmitOnEnter}
              />
              {!isSpendingPasswordSet && (
                <PopOver
                  content={
                    <FormattedHTMLMessage {...messages.passwordTooltip} />
                  }
                  key="tooltip"
                >
                  <SVGInline svg={infoIconInline} className={styles.infoIcon} />
                </PopOver>
              )}
            </div>
          </div>

          <div className={styles.repeatedPassword}>
            <PasswordInput
              {...repeatedPasswordField.bind()}
              onKeyPress={this.handleSubmitOnEnter}
              repeatPassword={newPasswordField.value}
              isPasswordRepeat
            />
          </div>
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

export default ChangeSpendingPasswordDialog;
