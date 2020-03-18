// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import validWords from '../../../../../common/crypto/valid-words.en';
import type {
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
} from '../../../types/walletRestoreTypes';

const messages = defineMessages({
  autocompletePlaceholder: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.placeholder',
    defaultMessage: '!!!Enter your {numberOfWords}-word recovery phrase',
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
  incompleteMultiLengthPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.incompleteMultiLengthPhrase',
    defaultMessage: '!!!12, 18 or 24 words',
    description: 'Error message for incomplete multi-length recovery phrase',
  },
  invalidRecoveryPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description: 'Label for invalid recovery phrase',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  onSetWalletMnemonics: Function,
  onValidateMnemonics: Function,
  mnemonics: Array<string>,
  walletKind: ?WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
  walletKindHardware: ?WalletHardwareKind,
  expectedWordCount: Array<number> | number,
  maxWordCount: number,
};

@observer
export default class MnemonicsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  recoveryPhraseAutocomplete: Autocomplete;

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: this.props.mnemonics,
          validators: () => {
            const { intl } = this.context;
            const {
              mnemonics,
              onValidateMnemonics,
              expectedWordCount,
            } = this.props;
            const wordCount = mnemonics.length;
            const isPhraseComplete = Array.isArray(expectedWordCount)
              ? expectedWordCount.includes(wordCount)
              : wordCount === expectedWordCount;
            if (!isPhraseComplete) {
              return [
                false,
                Array.isArray(expectedWordCount)
                  ? intl.formatMessage(messages.incompleteMultiLengthPhrase)
                  : intl.formatMessage(globalMessages.incompleteMnemonic, {
                      expected: expectedWordCount,
                    }),
              ];
            }
            return [
              onValidateMnemonics(mnemonics, wordCount),
              intl.formatMessage(messages.invalidRecoveryPhrase),
            ];
          },
        },
      },
    },
    {
      options: {
        validateOnChange: false,
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
    const {
      onClose,
      onBack,
      mnemonics,
      onSetWalletMnemonics,
      maxWordCount,
      expectedWordCount,
    } = this.props;
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    return (
      <WalletRestoreDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
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
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
            label={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
            placeholder={
              Array.isArray(expectedWordCount)
                ? intl.formatMessage(messages.autocompleteMultiLengthPhrase)
                : intl.formatMessage(messages.autocompletePlaceholder, {
                    numberOfWords: expectedWordCount,
                  })
            }
            options={validWords}
            maxSelections={maxWordCount}
            error={recoveryPhraseField.error}
            maxVisibleOptions={5}
            noResultsMessage={intl.formatMessage(
              messages.autocompleteNoResults
            )}
            skin={AutocompleteSkin}
            onChange={onSetWalletMnemonics}
            preselectedOptions={[...mnemonics]}
            optionHeight={50}
          />
          <div className="error" />
        </div>
      </WalletRestoreDialog>
    );
  }
}
