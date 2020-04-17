// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { join } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletRecoveryPhraseStepDialogs.scss';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../../config/cryptoConfig';
import globalMessages from '../../../i18n/global-messages';

export const messages = defineMessages({
  recoveryPhraseStep2Title: {
    id: 'wallet.settings.recoveryPhraseStep2Title',
    defaultMessage: '!!!Wallet recovery phrase verification',
    description: 'Label for the recoveryPhraseStep2Title on wallet settings.',
  },
  recoveryPhraseStep2Description: {
    id: 'wallet.settings.recoveryPhraseStep2Description',
    defaultMessage:
      '!!!Please enter your 12-word or 15-word wallet recovery phrase. Make sure you enter the words in the correct order.',
    description:
      'Label for the recoveryPhraseStep2Description on wallet settings.',
  },
  recoveryPhraseStep2Subtitle: {
    id: 'wallet.settings.recoveryPhraseStep2Subtitle',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recoveryPhraseStep2Subtitle on wallet settings.',
  },
  recoveryPhraseStep2Button: {
    id: 'wallet.settings.recoveryPhraseStep2Button',
    defaultMessage: '!!!Verify',
    description: 'Label for the recoveryPhraseStep2Button on wallet settings.',
  },
  recoveryPhraseInputHint: {
    id: 'wallet.settings.recoveryPhraseInputHint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.',
  },
  recoveryPhraseNoResults: {
    id: 'wallet.settings.recoveryPhraseInputNoResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  recoveryPhraseStep2InvalidMnemonics: {
    id: 'wallet.settings.recoveryPhraseStep2InvalidMnemonics',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
});

type Props = {
  // mnemonicValidator: Function,
  suggestedMnemonics: Array<string>,
  isVerifying: boolean,
  onVerify: Function,
  onClose: Function,
};

@observer
export default class WalletRecoveryPhraseStep2 extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const { intl } = this.context;
            const enteredWords = field.value;
            const wordCount = enteredWords.length;
            // const value = join(enteredWords, ' ');

            // Check if recovery phrase contains 12 words
            const isPhraseComplete =
              wordCount === LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT ||
              wordCount === WALLET_RECOVERY_PHRASE_WORD_COUNT;
            if (!isPhraseComplete) {
              return [
                false,
                intl.formatMessage(globalMessages.incompleteMnemonic, {
                  expected: `${LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT} or ${WALLET_RECOVERY_PHRASE_WORD_COUNT}`,
                }),
              ];
            }
            return true;
            // return [
            //   this.props.mnemonicValidator(value),
            //   this.context.intl.formatMessage(
            //     messages.recoveryPhraseStep2InvalidMnemonics
            //   ),
            // ];
          },
        },
      },
    },
    {
      options: {
        validateOnChange: true,
      },
    }
  );

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onClose, onVerify, suggestedMnemonics, isVerifying } = this.props;

    const recoveryPhraseField = form.$('recoveryPhrase');
    const canSubmit =
      !recoveryPhraseField.error &&
      !isVerifying &&
      recoveryPhraseField.value.length ===
        LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT;
    const actions = [
      {
        className: isVerifying ? styles.isVerifying : null,
        label: intl.formatMessage(messages.recoveryPhraseStep2Button),
        primary: true,
        onClick: () => onVerify(recoveryPhraseField.value),
        disabled: !canSubmit,
      },
    ];

    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.recoveryPhraseStep2Title)}
        actions={actions}
        closeOnOverlayClick={false}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.subtitle}>
          <p>{intl.formatMessage(messages.recoveryPhraseStep2Description)}</p>
        </div>

        <Autocomplete
          {...recoveryPhraseField.bind()}
          label={intl.formatMessage(messages.recoveryPhraseStep2Subtitle)}
          placeholder={intl.formatMessage(messages.recoveryPhraseInputHint)}
          options={suggestedMnemonics}
          maxSelections={LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT}
          error={recoveryPhraseField.error}
          maxVisibleOptions={5}
          noResultsMessage={intl.formatMessage(
            messages.recoveryPhraseNoResults
          )}
          skin={AutocompleteSkin}
          optionHeight={50}
        />
      </Dialog>
    );
  }
}
