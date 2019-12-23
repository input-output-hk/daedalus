// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
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
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the wallet name input on the wallet restore dialog.',
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Name the wallet you are restoring',
    description:
      'Hint "Name the wallet you are restoring" for the wallet name input on the wallet restore dialog.',
  },
  recoveryPhraseTypeLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.type.options.label',
    defaultMessage: '!!!Number of words in the recovery phrase',
    description:
      'Label for the recovery phrase type options on the wallet restore dialog.',
  },
  recoveryPhraseTypeOptionWord: {
    id: 'wallet.restore.dialog.recovery.phrase.type.word',
    defaultMessage: '!!! words',
    description:
      'Word for the recovery phrase type on the wallet restore dialog.',
  },
  recoveryPhraseType15WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.15word.option',
    defaultMessage: '!!!Rewards wallet',
    description:
      'Label for the recovery phrase type 15-word option on the wallet restore dialog.',
  },
  recoveryPhraseType12WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.12word.option',
    defaultMessage: '!!!Balance wallet',
    description:
      'Label for the recovery phrase type 12-word option on the wallet restore dialog.',
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recovery phrase input on the wallet restore dialog.',
  },
  recoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.',
  },
  recoveryPhraseNoResults: {
    id: 'wallet.restore.dialog.recovery.phrase.input.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  invalidRecoveryPhrase: {
    id: 'wallet.restore.dialog.form.errors.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  passwordSectionLabel: {
    id: 'wallet.restore.dialog.passwordSectionLabel',
    defaultMessage: '!!!Spending password',
    description: 'Password creation label.',
  },
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
  recoveryPhraseTabTitle: {
    id: 'wallet.restore.dialog.tab.title.recoveryPhrase',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Tab title "Daedalus wallet" in the wallet restore dialog.',
  },
  certificateTabTitle: {
    id: 'wallet.restore.dialog.tab.title.certificate',
    defaultMessage: '!!!Daedalus paper wallet',
    description:
      'Tab title "Daedalus paper wallet" in the wallet restore dialog.',
  },
  yoroiTabTitle: {
    id: 'wallet.restore.dialog.tab.title.yoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Tab title "Yoroi wallet" in the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.label',
    defaultMessage: '!!!27-word paper wallet recovery phrase',
    description:
      'Label for the shielded recovery phrase input on the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.hint',
    defaultMessage:
      '!!!Enter your {numberOfWords}-word paper wallet recovery phrase',
    description:
      'Hint "Enter your 27-word paper wallet recovery phrase." for the recovery phrase input on the wallet restore dialog.',
  },
  restorePaperWalletButtonLabel: {
    id: 'wallet.restore.dialog.paper.wallet.button.label',
    defaultMessage: '!!!Restore paper wallet',
    description:
      'Label for the "Restore paper wallet" button on the wallet restore dialog.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  error?: ?LocalizableError,
};

@observer
export default class ConfigurationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  componentWillReceiveProps(newProps: Props) {
    if (newProps.error) {
      handleFormErrors('.WalletRestoreDialog_error');
    }
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameInputLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNameInputHint
          ),
          value: '',
          validators: [
            ({ field }) => [
              isValidWalletName(field.value),
              this.context.intl.formatMessage(globalMessages.invalidWalletName),
            ],
          ],
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
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
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
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
        const { onContinue } = this.props;
        const { walletName, spendingPassword } = form.values();
        const walletData: Object = {
          // recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          spendingPassword,
        };

        // walletData.type = this.state.walletType;

        onContinue(walletData);
      },
      onError: () =>
        handleFormErrors('.SimpleFormField_error', { focusElement: true }),
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each(field => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack, error } = this.props;
    const { form } = this;

    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    const walletNameFieldClasses = classnames([styles.input, 'walletName']);
    const spendingPasswordFieldClasses = classnames([
      styles.input,
      styles.spendingPasswordField,
      'spendingPassword',
    ]);
    const repeatedPasswordFieldClasses = classnames([
      styles.input,
      styles.spendingPasswordField,
      'repeatedPassword',
    ]);

    return (
      <WalletRestoreDialog
        stepNumber={2}
        actions={[
          {
            primary: true,
            label: 'Continue',
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
            skin={InputSkin}
          />

          <div className={styles.spendingPasswordWrapper}>
            <div className={styles.spendingPasswordFields}>
              <Input
                className={spendingPasswordFieldClasses}
                onKeyPress={this.handleSubmitOnEnter}
                {...spendingPasswordField.bind()}
                error={spendingPasswordField.error}
                skin={InputSkin}
              />
              <Input
                className={repeatedPasswordFieldClasses}
                onKeyPress={this.handleSubmitOnEnter}
                {...repeatedPasswordField.bind()}
                error={repeatedPasswordField.error}
                skin={InputSkin}
              />
              <p className={styles.passwordInstructions}>
                <FormattedHTMLMessage
                  {...globalMessages.passwordInstructions}
                />
              </p>
            </div>
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </div>
      </WalletRestoreDialog>
    );
  }
}
