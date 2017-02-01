// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import MobxReactForm from 'mobx-react-form';
import Input from 'react-toolbox/lib/input/Input';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import { validateMnemonic } from 'bip39';
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

@observer
export default class WalletRestoreDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    onCancel: PropTypes.func.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  state = {
    isSubmitting: false
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      walletName: {
        value: '',
        validate: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )]
      },
      recoveryPhrase: {
        value: '',
        validate: [({ field }) => (
          [
            validateMnemonic(field.value),
            this.context.intl.formatMessage(globalMessages.invalidMnemonic)
          ]
        )]
      },
    }
  });

  actions = [
    {
      label: this.context.intl.formatMessage(messages.importButtonLabel),
      primary: true,
      onClick: () => this.submit()
    }
  ];

  submit = () => {
    this.validator.submit({
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
    const { validator } = this;
    const { error, onCancel } = this.props;
    const walletName = validator.$('walletName');
    const recoveryPhrase = validator.$('recoveryPhrase');
    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.title)}
        actions={this.actions}
        active
      >
        <Input
          type="text"
          className="walletName"
          label={intl.formatMessage(messages.walletNameInputLabel)}
          hint={intl.formatMessage(messages.walletNameInputHint)}
          value={walletName.value}
          error={walletName.error}
          onChange={walletName.onChange}
          onFocus={walletName.onFocus}
          onBlur={walletName.onBlur}
        />

        <Input
          type="text"
          className="recoveryPhrase"
          label={intl.formatMessage(messages.recoveryPhraseInputLabel)}
          hint={intl.formatMessage(messages.recoveryPhraseInputHint)}
          value={recoveryPhrase.value}
          error={recoveryPhrase.error}
          onChange={recoveryPhrase.onChange}
          onFocus={recoveryPhrase.onFocus}
          onBlur={recoveryPhrase.onBlur}
          multiline
          rows={3}
        />

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <DialogCloseButton onClose={onCancel} />

      </Dialog>
    );
  }

}
