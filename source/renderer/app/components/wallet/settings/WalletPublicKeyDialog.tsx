import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import vjf from 'mobx-react-form/lib/validators/VJF';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PublicKeyDialog.scss' or its... Remove this comment to see the full error message
import styles from './PublicKeyDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
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
  onRevealPublicKey: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  error: LocalizableError | null | undefined;
  hasReceivedWalletPublicKey: boolean;
  walletName: string;
};

@observer
class WalletPublicKeyDialog extends Component<Props> {
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
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
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
              if (field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(
                    globalMessages.fieldIsRequired
                  ),
                ];
              }

              return [true];
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
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        const { onRevealPublicKey } = this.props;
        onRevealPublicKey({
          spendingPassword,
        });
      },
    });
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { intl } = this.context;
    const { onClose, error, walletName } = this.props;
    const { form } = this;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const spendingPasswordField = form.$('spendingPassword');
    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabel),
        onClick: this.submit,
        primary: true,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
        disabled: !this.form.isValid,
      },
    ];
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={walletName}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.dialog}
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
          autoFocus
        />
        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
      </Dialog>
    );
  }
}

export default WalletPublicKeyDialog;
