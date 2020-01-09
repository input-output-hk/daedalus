// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import WalletRestoreDialog from './widgets/WalletRestoreDialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import { isValidMnemonic } from '../../../../../common/crypto/decrypt';
import validWords from '../../../../../common/crypto/valid-words.en';
import {
  WALLET_KINDS,
  WALLET_DAEDALUS_WORD_COUNT,
  WALLET_YOROI_WORD_COUNT,
  WALLET_HARDWARE_WORD_COUNT,
} from '../../../config/walletRestoreConfig';
import type {
  WalletKind,
  WalletDaedalusKind,
  WalletYoroiKind,
  WalletHardwareKind,
} from '../../../types/walletRestoreTypes';
import { PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';

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
  mnemonics: Array<string>,
  walletKind: ?WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
  walletKindHardware: ?WalletHardwareKind,
};

@observer
export default class MnemonicsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  recoveryPhraseAutocomplete: Autocomplete;

  get expectedWordCount() {
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = this.props;
    let expectedWordCount = 0;
    if (walletKindDaedalus && walletKind === WALLET_KINDS.DAEDALUS) {
      expectedWordCount = WALLET_DAEDALUS_WORD_COUNT[walletKindDaedalus];
    } else if (walletKindYoroi && walletKind === WALLET_KINDS.YOROI) {
      expectedWordCount = WALLET_YOROI_WORD_COUNT[walletKindYoroi];
    } else if (walletKindHardware) {
      expectedWordCount = WALLET_HARDWARE_WORD_COUNT[walletKindHardware];
    }
    return expectedWordCount;
  }

  get maxWordCount() {
    const { expectedWordCount } = this;
    return Array.isArray(expectedWordCount)
      ? Math.max(...expectedWordCount)
      : expectedWordCount;
  }

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: this.props.mnemonics,
          validators: () => {
            const { intl } = this.context;
            const { mnemonics: enteredWords } = this.props;
            const { expectedWordCount } = this;
            const wordCount = enteredWords.length;
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
            const value = join(enteredWords, ' ');
            return [
              this.expectedWordCount === PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT
                ? true
                : isValidMnemonic(value, wordCount),
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
    const { onClose, onBack, mnemonics, onSetWalletMnemonics } = this.props;
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    const { expectedWordCount, maxWordCount } = this;
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
          />
          <div className="error" />
        </div>
      </WalletRestoreDialog>
    );
  }
}
