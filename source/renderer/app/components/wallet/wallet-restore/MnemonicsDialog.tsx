import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import {
  errorOrIncompleteMarker,
  validateMnemonics,
} from '../../../utils/validations';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import validWords from '../../../../../common/config/crypto/valid-words.en';
import type {
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
} from '../../../types/walletRestoreTypes';

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

@observer
class MnemonicsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
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
        validateOnChange: true,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: this.props.onContinue,
      onError: () => {},
    });
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onBack,
      mnemonics,
      onSetWalletMnemonics,
      maxWordCount,
      expectedWordCount,
    } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    const canSubmit = recoveryPhraseField.isValid && !recoveryPhraseField.error;
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
        <div>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            label={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
            placeholder={
              Array.isArray(expectedWordCount)
                ? intl.formatMessage(messages.autocompleteMultiLengthPhrase)
                : intl.formatMessage(messages.autocompletePlaceholder, {
                    wordNumber: mnemonics.length + 1,
                  })
            }
            options={validWords}
            requiredSelections={
              Array.isArray(expectedWordCount)
                ? expectedWordCount
                : [expectedWordCount]
            }
            requiredSelectionsInfo={(required, actual) =>
              intl.formatMessage(globalMessages.knownMnemonicWordCount, {
                actual,
                required,
              })
            }
            maxSelections={maxWordCount}
            error={errorOrIncompleteMarker(recoveryPhraseField.error)}
            maxVisibleOptions={5}
            noResultsMessage={intl.formatMessage(
              messages.autocompleteNoResults
            )}
            skin={AutocompleteSkin}
            onChange={(enteredMnemonics) => {
              recoveryPhraseField.set(enteredMnemonics);
              onSetWalletMnemonics(enteredMnemonics);
            }}
            preselectedOptions={[...mnemonics]}
            optionHeight={50}
          />
          <div className="error" />
        </div>
      </WalletRestoreDialog>
    );
  }
}

export default MnemonicsDialog;
