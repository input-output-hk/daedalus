import React, { Component } from 'react';
import _ from 'lodash';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { validateMnemonics } from '../../../utils/validations';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import validWords from '../../../../../common/config/crypto/valid-words.en';
import type {
  WalletDaedalusKind,
  WalletHardwareKind,
  WalletKind,
  WalletYoroiKind,
} from '../../../types/walletRestoreTypes';
import { MnemonicInput } from '../mnemonic-input';

const messages = defineMessages({
  autocompletePlaceholder: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.placeholder',
    defaultMessage: '!!!Enter word #{wordNumber}',
    description: 'Placeholder for the mnemonics autocomplete.',
  },
  autocompleteMultiLengthPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.multiLengthPhrase.placeholder',
    defaultMessage: '!!!Enter your 12, 18 or 24-word recovery phrase',
    description: 'Placeholder for the multi-length mnemonics autocomplete.',
  },
  autocompleteNoResults: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the mnemonics autocomplete search results.',
  },
  continueButtonLabel: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.continueButtonLabel',
    defaultMessage: '!!!Check recovery phrase',
    description: 'Label for the mnemonics Continue button.',
  },
  invalidRecoveryPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description: 'Label for invalid recovery phrase',
  },
});
type Props = {
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onBack: (...args: Array<any>) => any;
  onSetWalletMnemonics: (...args: Array<any>) => any;
  onValidateMnemonics: (...args: Array<any>) => any;
  mnemonics: Array<string>;
  walletKind: WalletKind | null | undefined;
  walletKindDaedalus: WalletDaedalusKind | null | undefined;
  walletKindYoroi: WalletYoroiKind | null | undefined;
  walletKindHardware: WalletHardwareKind | null | undefined;
  expectedWordCount: Array<number> | number;
  maxWordCount: number;
};

interface FormFields {
  recoveryPhrase: string;
}

@observer
class MnemonicsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm<FormFields>(
    {
      fields: {
        recoveryPhrase: {
          value: [...this.props.mnemonics],
          validators: ({ field }) =>
            validateMnemonics({
              requiredWords: this.props.expectedWordCount,
              providedWords: field.value,
              validator: (enteredWords) => [
                this.props.onValidateMnemonics(enteredWords),
                this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
              ],
            }),
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        showErrorsOnChange: false,
        validateOnChange: true,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: this.props.onContinue,
      onError: () => {},
    });
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack, onSetWalletMnemonics, maxWordCount } = this.props;
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    const canSubmit =
      !recoveryPhraseField.error &&
      recoveryPhraseField.value.length === maxWordCount &&
      recoveryPhraseField.value.every((word) => word);
    const { reset, ...mnemonicInputProps } = recoveryPhraseField.bind();

    return (
      <WalletRestoreDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
            disabled: !canSubmit,
            label: intl.formatMessage(messages.continueButtonLabel),
            onClick: this.submit,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
      >
        <MnemonicInput
          {...mnemonicInputProps}
          label={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
          onChange={(enteredMnemonics) => {
            recoveryPhraseField.set(enteredMnemonics);
            onSetWalletMnemonics(enteredMnemonics);
          }}
          availableWords={validWords}
          wordCount={maxWordCount}
          error={recoveryPhraseField.error}
          reset={this.form.resetting}
        />
      </WalletRestoreDialog>
    );
  }
}

export default MnemonicsDialog;
