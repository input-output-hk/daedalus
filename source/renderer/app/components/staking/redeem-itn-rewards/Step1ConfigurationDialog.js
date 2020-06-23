// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import {
  defineMessages,
  intlShape /* FormattedHTMLMessage */,
} from 'react-intl';
import Wallet from '../../../domains/Wallet';
// import vjf from 'mobx-react-form/lib/validators/VJF';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Dialog from '../../widgets/Dialog';
import styles from './Step1ConfigurationDialog.scss';
import ReactToolboxMobxForm /* ,  {handleFormErrors } */ from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step1.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description: 'Title for Redeem Incentivized Testnet - Step 1',
  },
});

type Props = {
  wallets: Array<Wallet>,
  isSubmitting: boolean,
  isWalletValid: boolean,
  onContinue: Function,
  onClose: Function,
  isWalletValid?: boolean,
  suggestedMnemonics: Array<string>,
  error?: ?LocalizableError,
};

@observer
export default class Step1ConfigurationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
  };

  recoveryPhraseAutocomplete: Autocomplete;

  // componentDidUpdate() {
  //   if (this.props.error) {
  //     handleFormErrors('.ConfigurationDialog_error');
  //   }
  // }

  form = new ReactToolboxMobxForm({
    fields: {
      recoveryPhrase: {
        value: [],
        // validators: ({ field }) => {
        //   console.log('field', field);
        //   // @REDEEM TODO: validation
        //   // const { intl } = this.context;
        //   // const { walletType } = this.state;
        //   // const enteredWords = field.value;
        //   // const wordCount = enteredWords.length;
        //   // const expectedWordCount =
        //   //   RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType];
        //   // const value = join(enteredWords, ' ');
        //   // // Regular mnemonics have 12 and paper wallet recovery needs 27 words
        //   // const isPhraseComplete = wordCount === expectedWordCount;
        //   // if (!isPhraseComplete) {
        //   //   return [
        //   //     false,
        //   //     intl.formatMessage(globalMessages.incompleteMnemonic, {
        //   //       expected: expectedWordCount,
        //   //     }),
        //   //   ];
        //   // }
        //   // return [
        //   //   // TODO: we should also validate paper wallets mnemonics here!
        //   //   !this.isCertificate()
        //   //     ? this.props.mnemonicValidator(value, expectedWordCount)
        //   //     : true,
        //   //   this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
        //   // ];
        // },
      },
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { onContinue } = this.props;
        const { recoveryPhrase /* walletName */ } = form.values();
        onContinue({
          wallet: this.props.wallets[0],
          recoveryPhrase,
        });
      },
      onError: (a, b, c) => {
        console.log('ERROR');
        console.log('a', a);
        console.log('b', b);
        console.log('c', c);
      },
      //   handleFormErrors('.ConfigurationDialog_error', { focusElement: true }),
    });
  };

  // handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  // resetForm = () => {
  //   const { form } = this;
  //   // Cancel all debounced field validations
  //   form.each(field => {
  //     field.debouncedValidation.cancel();
  //   });
  //   form.reset();
  //   form.showErrors(false);
  // };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      wallets,
      onContinue,
      onClose,
      isSubmitting,
      isWalletValid,
      suggestedMnemonics,
    } = this.props;
    const canSubmit = !isSubmitting && isWalletValid; // && form.isValid;
    const recoveryPhraseField = form.$('recoveryPhrase');

    const walletsSelectClasses = classnames([styles.walletSselect]);
    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={[
          {
            className: isSubmitting ? styles.isSubmitting : null,
            disabled: !canSubmit,
            primary: true,
            label: '!!!Continue ->',
            // label: intl.formatMessage(messages.continueButtonLabel),
            onClick: () =>
              onContinue({
                wallet: wallets[0],
                recoveryPhrase: ['one', 'two'],
              }),
          },
        ]}
        onContinue={onContinue}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.component}>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
            label={
              'Autocomplete LABEL'
              // !this.isCertificate()
              //   ? intl.formatMessage(messages.recoveryPhraseInputLabel)
              //   : intl.formatMessage(messages.shieldedRecoveryPhraseInputLabel)
            }
            placeholder={
              'Autocomplete PLACEHOLDER'

              // !this.isCertificate()
              //   ? intl.formatMessage(messages.recoveryPhraseInputHint)
              //   : intl.formatMessage(messages.shieldedRecoveryPhraseInputHint, {
              //       numberOfWords: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
              //     })
            }
            options={suggestedMnemonics}
            maxSelections={
              3
              /* RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType] */
            }
            error={recoveryPhraseField.error}
            maxVisibleOptions={5}
            noResultsMessage={
              'NO RESULTS LABEL'
              // intl.formatMessage(messages.recoveryPhraseNoResults)
            }
            skin={AutocompleteSkin}
            optionHeight={50}
          />

          <WalletsDropdown
            className={walletsSelectClasses}
            label={
              'WalletsDropdown Label'
              /* intl.formatMessage(messages.selectWalletInputLabel) */
            }
            numberOfStakePools={4}
            wallets={wallets}
            onChange={(a, b, c) => {
              console.log('a', a);
              console.log('b', b);
              console.log('c', c);
            }}
            placeholder={
              'WalletsDropdown Placeholder'
              /* intl.formatMessage(
              messages.selectWalletInputPlaceholder
            ) */
            }
            value={wallets[0].name}
            getStakePoolById={(a, b, c) => {
              console.log('a', a);
              console.log('b', b);
              console.log('c', c);
            }}
          />
          {/* error && <p className={styles.error}>{intl.formatMessage(error)}</p> */}
        </div>
      </Dialog>
    );
  }

  // render() {
  //   const { intl } = this.context;
  //   const { onClose, onBack, error, isSubmitting } = this.props;
  //   const { form } = this;
  //   const canSubmit = !isSubmitting && form.isValid;
  //   return (
  //     <Dialog
  //       title={'Redeem Incentivized Testnet rewards'}
  //       actions={[
  //         {
  //           className: isSubmitting ? styles.isSubmitting : null,
  //           disabled: !canSubmit,
  //           primary: true,
  //           label: intl.formatMessage(messages.continueButtonLabel),
  //           onClick: this.submit,
  //         },
  //       ]}
  //       onClose={onClose}
  //       onBack={onBack}
  //     >
  //       <div className={styles.component}>
  //         {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
  //       </div>
  //     </Dialog>
  //   );
  // }
}
