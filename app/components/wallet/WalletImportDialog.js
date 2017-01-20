// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import MobxReactForm from 'mobx-react-form';
import Input from 'react-toolbox/lib/input/Input';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../widgets/DialogCloseButton';
import styles from './WalletAddDialog.scss';

const messages = defineMessages({
  title: {
    id: 'wallet.import.dialog.title.label',
    defaultMessage: '!!!Import wallet',
    description: 'Label "Import wallet" on the wallet import dialog.'
  },
  walletNameInputLabel: {
    id: 'wallet.import.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for the wallet name input on the wallet import dialog.'
  },
  walletNameInputHint: {
    id: 'wallet.import.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Enter wallet name',
    description: 'Hint "Enter wallet name" for the wallet name input on the wallet import dialog.'
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.import.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description: 'Label for the recovery phrase input on the wallet import dialog.'
  },
  recoveryPhraseInputHint: {
    id: 'wallet.import.dialog.recovery.phrase.input.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description: 'Hint "Enter recovery phrase" for the recovery phrase input on the wallet import dialog.'
  },
  importButtonLabel: {
    id: 'wallet.import.dialog.import.wallet.button.label',
    defaultMessage: '!!!Import wallet',
    description: 'Label for the "Import wallet" button on the wallet import dialog.'
  },
});

const fields = {
  walletName: {
    value: '',
  },
  recoveryPhrase: {
    value: '',
  },
};

const options = {
  validateOnChange: false
};

@observer
export default class WalletImportDialog extends Component {

  static contextTypes = {
    intl: intlShape.isRequired
  };

  static propTypes = {
  };

  validator = new MobxReactForm({ options, fields }, {});

  actions = [
    {
      label: this.context.intl.formatMessage(messages.importButtonLabel),
      primary: true,
      onClick: () => {}
    }
  ];

  render() {
    const { intl } = this.context;
    const { validator } = this;
    const walletName = validator.$('walletName');
    const recoveryPhrase = validator.$('recoveryPhrase');
    const errors = {
    };
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
          error={errors.walletName}
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
          error={errors.recoveryPhrase}
          onChange={recoveryPhrase.onChange}
          onFocus={recoveryPhrase.onFocus}
          onBlur={recoveryPhrase.onBlur}
          multiline
          rows={3}
        />

        <DialogCloseButton onClose={this.props.onCancel} />

      </Dialog>
    );
  }

}
