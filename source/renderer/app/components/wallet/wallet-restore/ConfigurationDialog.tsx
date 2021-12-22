// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { PasswordInput } from '../../widgets/forms/PasswordInput';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import styles from './ConfigurationDialog.scss';
import ReactToolboxMobxForm, {
  handleFormErrors,
} from '../../../utils/ReactToolboxMobxForm';
import {
  isValidWalletName,
  isValidSpendingPassword,
  isValidRepeatPassword,
} from '../../../utils/validations';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import infoIconInline from '../../../assets/images/info-icon.inline.svg';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  description1: {
    id: 'wallet.restore.dialog.step.configuration.description1',
    defaultMessage:
      '!!!Name your restored wallet and set a spending password to keep your wallet secure.',
    description: 'Description1 for Configuration Step',
  },
  description2: {
    id: 'wallet.restore.dialog.step.configuration.description2',
    defaultMessage:
      '!!!Wallet names and spending passwords are only stored locally and are not stored on the blockchain. You can give your restored wallet a new name and set a new spending password, you don’t need to match the wallet name and spending password you were using before. <b>Only the recovery phrase from your original wallet is needed to restore a wallet.</b>',
    description: 'Description2 for Configuration Step',
  },
  walletNameLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.walletName.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for Wallet Name Input',
  },
  walletNamePlaceholder: {
    id: 'wallet.restore.dialog.step.configuration.input.walletName.placeholder',
    defaultMessage: '!!!Name the wallet you are restoring',
    description: 'Placeholder for Wallet Name Input',
  },
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.spendingPassword.label',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.repeatPassword.label',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldsPlaceholder: {
    id:
      'wallet.restore.dialog.step.configuration.input.passwordFields.placeholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
  continueButtonLabel: {
    id: 'wallet.restore.dialog.step.configuration.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description: 'Placeholder for the dialog "Continue" button',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      '!!!It is really good to use Password Manager apps to improve security. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris mattis diam non nulla sollicitudin, ac ultrices purus luctus.',
    description: 'Tooltip for the password input in the create wallet dialog.',
  },
});

type Props = {
  isSubmitting: boolean,
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  onChange: Function,
  walletName: string,
  spendingPassword: string,
  repeatPassword: string,
  error?: ?LocalizableError,
  currentLocale: string,
};

@observer
export default class ConfigurationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  componentDidUpdate() {
    if (this.props.error) {
      handleFormErrors('.ConfigurationDialog_error');
    }
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNamePlaceholder
          ),
          value: this.props.walletName,
          validators: [
            ({ field }) => [
              isValidWalletName(field.value),
              this.context.intl.formatMessage(globalMessages.invalidWalletName),
            ],
          ],
          hooks: {
            onChange: this.props.onChange.bind(this, 'walletName'),
          },
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldsPlaceholder
          ),
          value: this.props.spendingPassword,
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
          hooks: {
            onChange: this.props.onChange.bind(this, 'spendingPassword'),
          },
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldsPlaceholder
          ),
          value: this.props.repeatPassword,
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
          hooks: {
            onChange: this.props.onChange.bind(this, 'repeatPassword'),
          },
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { onContinue } = this.props;
        const { walletName, spendingPassword } = form.values();
        onContinue(walletName, spendingPassword);
      },
      onError: () =>
        handleFormErrors('.ConfigurationDialog_error', { focusElement: true }),
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack, error, isSubmitting, currentLocale } = this.props;
    const { form } = this;

    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatPasswordField = form.$('repeatPassword');

    const walletNameFieldClasses = classnames([
      styles.walletName,
      'walletName',
    ]);

    const spendingPasswordClasses = classnames([
      styles.spendingPasswordField,
      currentLocale === 'ja-JP' ? styles.jpLangTooltipIcon : '',
    ]);

    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.continueButtonLabel)
    ) : (
      <LoadingSpinner />
    );

    const canSubmit = !isSubmitting && form.isValid;

    return (
      <WalletRestoreDialog
        className={styles.dialogComponent}
        stepNumber={2}
        actions={[
          {
            className: isSubmitting ? styles.isSubmitting : null,
            disabled: !canSubmit,
            primary: true,
            label: buttonLabel,
            onClick: this.submit,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
      >
        <div className={styles.component}>
          <p>{intl.formatMessage(messages.description1)}</p>
          <p>
            <FormattedHTMLMessage {...messages.description2} />
          </p>
          <Input
            className={walletNameFieldClasses}
            onKeyPress={this.handleSubmitOnEnter}
            {...walletNameField.bind()}
            error={walletNameField.error}
          />

          <div className={styles.spendingPasswordWrapper}>
            <div className={styles.spendingPasswordFields}>
              <div className={spendingPasswordClasses}>
                <PasswordInput
                  className="spendingPassword"
                  onKeyPress={this.handleSubmitOnEnter}
                  {...spendingPasswordField.bind()}
                />
                <PopOver
                  content={
                    <FormattedHTMLMessage {...messages.passwordTooltip} />
                  }
                  key="tooltip"
                >
                  <SVGInline svg={infoIconInline} className={styles.infoIcon} />
                </PopOver>
              </div>
              <div className={styles.spendingPasswordField}>
                <PasswordInput
                  className="repeatPassword"
                  onKeyPress={this.handleSubmitOnEnter}
                  {...repeatPasswordField.bind()}
                  repeatPassword={spendingPasswordField.value}
                  isPasswordRepeat
                />
              </div>
            </div>
            <div className={styles.passwordInstructions}>
              <FormattedHTMLMessage {...globalMessages.passwordInstructions} />
            </div>
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </div>
      </WalletRestoreDialog>
    );
  }
}
