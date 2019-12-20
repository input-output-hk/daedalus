// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import WalletRestoreDialog from './WalletRestoreDialog';
import commonStyles from './StepDialogStyles.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import { isValidMnemonic } from '../../../../../common/crypto/decrypt';
import validWords from '../../../../../common/crypto/valid-words.en';
import {
  WALLET_KINDS,
  // WALLET_DAEDALUS_KINDS,
  // WALLET_YOROI_KINDS,
  // WALLET_HARDWARE_KINDS,
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

const messages = defineMessages({
  REPLACE: {
    id: 'REPLACE',
    defaultMessage: '!!!REPLACE-ME',
    description: 'TODO MSGS',
  },
  autocompletePlaceholder: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.placeholder',
    defaultMessage: '!!!Enter your {numberOfWords}-word recovery phrase',
    description: 'Placeholder for the mnemonics autocomplete.',
  },
  autocompleteNoResults: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the mnemonics autocomplete search results.',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  walletKind: ?WalletKind,
  walletKindDaedalus: ?WalletDaedalusKind,
  walletKindYoroi: ?WalletYoroiKind,
  walletKindHardware: ?WalletHardwareKind,
};

export default class StepMnemonicsDialog extends Component<Props> {
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

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validatorsFIX: ({ field }) => {
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
              intl.formatMessage(messages.REPLACE),
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
      onSuccess: form => {
        const { onContinue } = this.props;
        const { recoveryPhrase } = form.values();
        onContinue(recoveryPhrase);
      },
      onError: () => {},
    });
  };

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
    const { intl } = this.context;
    const { onClose, onBack } = this.props;
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    const { expectedWordCount: numberOfWords } = this;
    return (
      <WalletRestoreDialog
        stepNumber={1}
        actions={[
          {
            primary: true,
            label: 'Continue',
            onClick: this.submit,
          },
        ]}
        onClose={onClose}
        onBack={onBack}
      >
        <div className={commonStyles.component}>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
            label={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
            placeholder={intl.formatMessage(messages.autocompletePlaceholder, {
              numberOfWords,
            })}
            options={validWords}
            maxSelections={this.expectedWordCount}
            error={recoveryPhraseField.error}
            maxVisibleOptions={5}
            noResultsMessage={intl.formatMessage(
              messages.autocompleteNoResults
            )}
            skin={AutocompleteSkin}
          />
          <div className="error" />
        </div>
      </WalletRestoreDialog>
    );
  }
}
