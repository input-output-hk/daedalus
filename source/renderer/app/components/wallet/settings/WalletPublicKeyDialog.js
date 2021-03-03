// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import vjf from 'mobx-react-form/lib/validators/VJF';
import styles from './WalletPublicKeyDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import { isValidSpendingPassword } from '../../../utils/validations';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';

const messages = defineMessages({
  title: {
    id: 'wallet.settings.walletPublicKeyDialog.title',
    defaultMessage: '!!!Reveal wallet public key',
    description: 'Title "Choose a stake pool" on the reveal Wallet Id dialog.',
  },
  description: {
    id: 'wallet.settings.walletPublicKeyDialog.description',
    defaultMessage:
      '!!!Please enter your spending password to reveal your wallet’s public key.',
    description: 'Description on the reveal Wallet Id dialog.',
  },
  buttonLabel: {
    id: 'wallet.settings.walletPublicKeyDialog.button',
    defaultMessage: '!!!Reveal wallet public key',
    description: 'Description on the reveal Wallet Id dialog.',
  },
});

type Props = {
  onRevealPublicKey: Function,
  onClose: Function,
  error: ?LocalizableError,
  hasReceivedWalletPublicKey: boolean,
};

@observer
export default class WalletPublicKeyDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidUpdate() {
    const { hasReceivedWalletPublicKey, onClose } = this.props;
    if (hasReceivedWalletPublicKey) {
      onClose();
    }
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            globalMessages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            globalMessages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              return [
                isValidSpendingPassword(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidSpendingPassword
                ),
              ];
            },
          ],
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
        const { spendingPassword } = form.values();
        const { onRevealPublicKey } = this.props;
        onRevealPublicKey({ spendingPassword });
      },
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { intl } = this.context;
    const { onClose, error } = this.props;
    const { form } = this;
    const spendingPasswordField = form.$('spendingPassword');
    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabel),
        onClick: this.submit,
        primary: true,
      },
    ];
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.description}>
          {intl.formatMessage(messages.description)}
        </div>
        <Input
          className={styles.spendingPassword}
          {...spendingPasswordField.bind()}
          error={spendingPasswordField.error}
          onKeyPress={this.handleSubmitOnEnter}
        />
        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
      </Dialog>
    );
  }
}
