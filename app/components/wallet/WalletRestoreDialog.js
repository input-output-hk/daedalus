// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import Autocomplete from 'react-polymorph/lib/components/Autocomplete';
import SimpleAutocompleteSkin from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import { isValidWalletName, isValidWalletPassword, isValidRepeatPassword } from '../../utils/validations';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletRestoreDialog.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.restore.dialog.title.label',
    defaultMessage: '!!!Restore wallet',
    description: 'Label "Restore wallet" on the wallet restore dialog.'
  },
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for the wallet name input on the wallet restore dialog.'
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Enter wallet name',
    description: 'Hint "Enter wallet name" for the wallet name input on the wallet restore dialog.'
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description: 'Label for the recovery phrase input on the wallet restore dialog.'
  },
  recoveryPhraseInputHint: {
    id: 'wallet.restore.dialog.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description: 'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.'
  },
  recoveryPhraseNoResults: {
    id: 'wallet.restore.dialog.recovery.phrase.input.noResults',
    defaultMessage: '!!!No results',
    description: '"No results" message for the recovery phrase input search results.'
  },
  importButtonLabel: {
    id: 'wallet.restore.dialog.restore.wallet.button.label',
    defaultMessage: '!!!Restore wallet',
    description: 'Label for the "Restore wallet" button on the wallet restore dialog.'
  },
  invalidRecoveryPhrase: {
    id: 'wallet.restore.dialog.form.errors.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description: 'Error message shown when invalid recovery phrase was entered.'
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.restore.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Activate to create password',
    description: 'Text for the "Activate to create password" switch in the wallet restore dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.restore.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Password',
    description: 'Label for the "Activate to create password" switch in the wallet restore dialog.',
  },
  walletPasswordLabel: {
    id: 'wallet.restore.dialog.walletPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description: 'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onSubmit: Function,
  onCancel: Function,
  isSubmitting: boolean,
  mnemonicValidator: Function,
  error?: ?LocalizableError,
  suggestedMnemonics: Array<string>,
};

type State = {
  createPassword: boolean,
};

@observer
export default class WalletRestoreDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  state = {
    createPassword: false,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      walletName: {
        label: this.context.intl.formatMessage(messages.walletNameInputLabel),
        placeholder: this.context.intl.formatMessage(messages.walletNameInputHint),
        value: '',
        validators: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )],
      },
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseInputLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseInputHint),
        value: '',
        validators: ({ field }) => {
          const value = join(field.value, ' ');
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.mnemonicValidator(value),
            this.context.intl.formatMessage(messages.invalidRecoveryPhrase)
          ];
        },
      },
      walletPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.walletPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
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
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const walletPassword = form.$('walletPassword').value;
          if (walletPassword.length === 0) return [true];
          return [
            isValidRepeatPassword(walletPassword, field.value),
            this.context.intl.formatMessage(globalMessages.invalidRepeatPassword)
          ];
        }],
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { createPassword } = this.state;
        const { recoveryPhrase, walletName, walletPassword } = form.values();
        const walletData = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          walletPassword: createPassword ? walletPassword : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {}
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { suggestedMnemonics, isSubmitting, error, onCancel } = this.props;
    const { createPassword } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'WalletRestoreDialog',
    ]);

    const walletNameFieldClasses = classnames([
      'walletName',
      styles.walletName,
    ]);

    const walletPasswordFieldsClasses = classnames([
      styles.walletPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const walletNameField = form.$('walletName');
    const recoveryPhraseField = form.$('recoveryPhrase');
    const walletPasswordField = form.$('walletPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: this.context.intl.formatMessage(messages.importButtonLabel),
        primary: true,
        disabled: isSubmitting,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onCancel}
        closeButton={<DialogCloseButton />}
      >

        <Input
          className={walletNameFieldClasses}
          {...walletNameField.bind()}
          error={walletNameField.error}
          skin={<SimpleInputSkin />}
        />

        <Autocomplete
          options={suggestedMnemonics}
          maxSelections={12}
          {...recoveryPhraseField.bind()}
          error={recoveryPhraseField.error}
          maxVisibleOptions={5}
          noResultsMessage={intl.formatMessage(messages.recoveryPhraseNoResults)}
          skin={<SimpleAutocompleteSkin />}
        />

        <div className={styles.walletPassword}>
          <div className={styles.walletPasswordSwitch}>
            <div className={styles.passwordLabel}>
              {intl.formatMessage(messages.passwordSwitchLabel)}
            </div>
            <Checkbox
              onChange={this.handlePasswordSwitchToggle}
              label={intl.formatMessage(messages.passwordSwitchPlaceholder)}
              checked={createPassword}
              skin={<SimpleSwitchSkin />}
            />
          </div>

          <div className={walletPasswordFieldsClasses}>
            <Input
              className="walletPassword"
              {...walletPasswordField.bind()}
              error={walletPasswordField.error}
              skin={<SimpleInputSkin />}
            />
            <Input
              className="repeatedPassword"
              {...repeatedPasswordField.bind()}
              error={repeatedPasswordField.error}
              skin={<SimpleInputSkin />}
            />
            <p className={styles.passwordInstructions}>
              {intl.formatMessage(globalMessages.passwordInstructions)}
            </p>
          </div>
        </div>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

      </Dialog>
    );
  }

}
