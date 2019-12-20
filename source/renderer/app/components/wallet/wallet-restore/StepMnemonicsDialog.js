// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './StepDialogStyles.scss';
import ReactToolboxMobxForm, {
  handleFormErrors,
} from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import { isValidMnemonic } from '../../../../../common/crypto/decrypt';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_KINDS,
  WALLET_YOROI_KINDS,
  WALLET_HARDWARE_KINDS,
  WALLET_DAEDALUS_WORD_COUNT,
  WALLET_YOROI_WORD_COUNT,
  WALLET_HARDWARE_WORD_COUNT,
} from '../../../config/walletRestoreConfig';
import {
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
} from '../../../types/walletRestoreTypes';

const messages = defineMessages({
  labelWalletKind: {
    id: 'wallet.restore.dialog.step.WalletKind.label.walletKind',
    defaultMessage: '!!!What kind of wallet would you like to restore?',
    description: 'Label for the "labelwalletKind" checkbox.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  walletKind?: WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
  walletKindHardware: ?WalletHardwareKind,
};

export default class StepMnemonicsDialog extends Component<Props> {
  get expectedWordCount() {
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    let expectedWordCount = 0;
    if (walletKind === WALLET_KINDS.DAEDALUS) {
      expectedWordCount = WALLET_DAEDALUS_WORD_COUNT[walletKindDaedalus];
    } else if (walletKind === WALLET_KINDS.YOROI) {
      expectedWordCount = WALLET_YOROI_WORD_COUNT[walletKindYoroi];
    } else {
      expectedWordCount = WALLET_HARDWARE_WORD_COUNT[walletKindHardware];
    }
    return expectedWordCount;
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const { intl } = this.context;
            const enteredWords = field.value;
            const wordCount = enteredWords.length;
            const value = join(enteredWords, ' ');
            const isPhraseComplete = wordCount === this.expectedWordCount;
            if (!isPhraseComplete) {
              return [
                false,
                intl.formatMessage(globalMessages.incompleteMnemonic, {
                  expected: this.expectedWordCount,
                }),
              ];
            }
            return [
              isValidMnemonic(value, this.expectedWordCount),
              this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
            ];
          },
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
        const { recoveryPhrase, walletName, spendingPassword } = form.values();
        const walletData: Object = {
          recoveryPhrase: join(recoveryPhrase, ' '),
          walletName,
          spendingPassword,
        };

        walletData.type = this.state.walletType;

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

  resetMnemonics = () => {
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    recoveryPhraseField.debouncedValidation.cancel();
    recoveryPhraseField.reset();
    recoveryPhraseField.showErrors(false);

    // Autocomplete has to be reset manually
    this.recoveryPhraseAutocomplete.clear();
  };

  render() {
    const { onContinue, onClose, onBack } = this.props;
    return (
      <WalletRestoreDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: onContinue,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
      >
        <div className={commonStyles.component}>MNEMONICS STEP CONTENT</div>
      </WalletRestoreDialog>
    );
  }
}
