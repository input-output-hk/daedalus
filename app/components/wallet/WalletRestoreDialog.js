// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-toolbox/lib/input/Input';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import { isValidWalletName } from '../../lib/validations';
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
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

@observer
export default class WalletRestoreDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  props: {
    onSubmit: Function,
    onCancel: Function,
    mnemonicValidator: Function,
    error?: ?LocalizableError,
  };

  state = {
    isSubmitting: false,
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
        bindings: 'ReactToolbox',
      },
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseInputLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseInputHint),
        value: '',
        validators: ({ field }) => {
          const value = field.value;
          if (value === '') return [false, this.context.intl.formatMessage(messages.fieldIsRequired)];
          return [
            this.props.mnemonicValidator(field.value),
            this.context.intl.formatMessage(messages.invalidRecoveryPhrase)
          ];
        },
        bindings: 'ReactToolbox',
      },
    },
  }, {
    options: {
      showErrorsOnChange: true,
      validateOnChange: true,
      validationDebounceWait: 250,
      validationDebounceOptions: { trailing: true, },
    },
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.importButtonLabel),
      primary: true,
      onClick: () => this.submit()
    }
  ];

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        this.props.onSubmit(form.values());
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      }
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { error, onCancel } = this.props;
    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        actions={this.actions}
        active
      >
        <Input className="walletName" {...form.$('walletName').bind()} />

        <Input
          className="recoveryPhrase"
          multiline
          rows={3}
          {...form.$('recoveryPhrase').bind()}
        />

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
