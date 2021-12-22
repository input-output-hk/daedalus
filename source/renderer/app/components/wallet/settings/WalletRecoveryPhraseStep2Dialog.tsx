import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import suggestedMnemonics from '../../../../../common/config/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../common/config/crypto/decrypt';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  errorOrIncompleteMarker,
  validateMnemonics,
} from '../../../utils/validations';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryPhraseStepDial... Remove this comment to see the full error message
import styles from './WalletRecoveryPhraseStepDialogs.scss';
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
      '!!!Please enter your wallet recovery phrase. Make sure you enter the words in the correct order.',
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
  recoveryPhraseInputPlaceholder: {
    id: 'wallet.settings.recoveryPhraseInputPlaceholder',
    defaultMessage: '!!!Enter word #{wordNumber}',
    description:
      'Placeholder "Enter word #{wordNumber}" for the recovery phrase input on the verification dialog.',
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
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  expectedWordCount: number | Array<number>;
  walletName: string;
};
type State = {
  isVerifying: boolean;
};

@observer
class WalletRecoveryPhraseStep2Dialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isVerifying: false,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) =>
            validateMnemonics({
              requiredWords: this.props.expectedWordCount,
              providedWords: field.value,
              validator: (enteredWords) => [
                isValidMnemonic(enteredWords.join(' '), enteredWords.length),
                this.context.intl.formatMessage(
                  messages.recoveryPhraseStep2InvalidMnemonics
                ),
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

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onClose, onContinue, expectedWordCount, walletName } = this.props;
    const { isVerifying } = this.state;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const recoveryPhraseField = form.$('recoveryPhrase');
    const { length: enteredWordCount } = recoveryPhraseField.value;
    const canSubmit =
      !recoveryPhraseField.error &&
      !isVerifying &&
      (Array.isArray(expectedWordCount)
        ? expectedWordCount.includes(enteredWordCount)
        : enteredWordCount === expectedWordCount);
    const recoveryPhrase = recoveryPhraseField.value;
    const actions = [
      {
        className: isVerifying ? styles.isVerifying : null,
        label: intl.formatMessage(messages.recoveryPhraseStep2Button),
        primary: true,
        onClick: () => {
          this.setState({
            isVerifying: true,
          });
          onContinue({
            recoveryPhrase,
          });
        },
        disabled: !canSubmit,
      },
    ];
    const maxSelections = Array.isArray(expectedWordCount)
      ? Math.max(...expectedWordCount)
      : expectedWordCount;
    return (
      <Dialog
        className={styles.dialog}
        title={intl.formatMessage(messages.recoveryPhraseStep2Title)}
        subtitle={walletName}
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
          placeholder={intl.formatMessage(
            messages.recoveryPhraseInputPlaceholder,
            {
              wordNumber: enteredWordCount + 1,
            }
          )}
          options={suggestedMnemonics}
          requiredSelections={
            Array.isArray(expectedWordCount)
              ? expectedWordCount
              : [expectedWordCount]
          }
          requiredSelectionsInfo={(required, actual) =>
            Array.isArray(expectedWordCount)
              ? intl.formatMessage(globalMessages.unknownMnemonicWordCount, {
                  actual,
                })
              : intl.formatMessage(globalMessages.knownMnemonicWordCount, {
                  actual,
                  required,
                })
          }
          maxSelections={maxSelections}
          error={errorOrIncompleteMarker(recoveryPhraseField.error)}
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

export default WalletRecoveryPhraseStep2Dialog;
