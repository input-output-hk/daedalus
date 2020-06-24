// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { get } from 'lodash';
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
// @REDEEM TODO - color variable
import styles from './Step1ConfigurationDialog.scss';
import ReactToolboxMobxForm /* ,  {handleFormErrors } */ from '../../../utils/ReactToolboxMobxForm';
// import { submitOnEnter } from '../../../utils/form';
// import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';
// import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step1.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description: 'Title for Redeem Incentivized Testnet - Step 1',
  },
  description: {
    id: 'staking.redeemItnRewards.step1.description',
    defaultMessage:
      '!!!If you participated in the Incentivized Testnet and earned rewards by running a stake pool or delegating your stake, you can use this feature to redeem your rewards as ada on the Cardano mainnet. You will need the wallet recovery phrase for the Incentivized Testnet wallet used to earn rewards and an existing mainnet wallet in Daedalus to which the rewards will be transferred. This wallet will also be used to pay any applicable transaction fees.',
    description: 'description for Redeem Incentivized Testnet - Step 1',
  },
  recoveryPhraseLabel: {
    id: 'staking.redeemItnRewards.step1.recoveryPhraseLabel',
    defaultMessage:
      '!!!Wallet recovery phrase for your Incentivized Testnet Rewards wallet:',
    description: 'recoveryPhraseLabel for Redeem Incentivized Testnet - Step 1',
  },
  walletsDropdownLabel: {
    id: 'staking.redeemItnRewards.step1.walletsDropdownLabel',
    defaultMessage: '!!!Redeem rewards to:',
    description:
      'walletsDropdownLabel for Redeem Incentivized Testnet - Step 1',
  },
  walletsDropdownError: {
    id: 'staking.redeemItnRewards.step1.walletsDropdownError',
    defaultMessage:
      '!!!The selected wallet does not have sufficient ada to cover the necessary transaction fees. Please choose another wallet or add more funds to this one.',
    description: 'Title for Redeem Incentivized Testnet - Step 1',
  },
});

type Props = {
  wallets: Array<Wallet>,
  redeemWallet?: Wallet,
  isSubmitting: boolean,
  isWalletValid: boolean,
  onContinue: Function,
  onClose: Function,
  onSelectWallet: Function,
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
      redeemWallet,
      onContinue,
      onClose,
      isSubmitting,
      isWalletValid,
      suggestedMnemonics,
      onSelectWallet,
    } = this.props;
    const canSubmit = !isSubmitting && isWalletValid; // && form.isValid;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const redeemWalletId = get(redeemWallet, 'id', null);
    let walletsDropdownError;
    if (redeemWallet && redeemWallet.amount.isZero())
      walletsDropdownError = intl.formatMessage(messages.walletsDropdownError);

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
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
            label={intl.formatMessage(messages.recoveryPhraseLabel)}
            options={suggestedMnemonics}
            maxSelections={WALLET_RECOVERY_PHRASE_WORD_COUNT}
            error={recoveryPhraseField.error}
            maxVisibleOptions={5}
            noResultsMessage={
              'NO RESULTS LABEL'
              // intl.formatMessage(messages.recoveryPhraseNoResults)
            }
            className={styles.recoveryPhrase}
            skin={AutocompleteSkin}
            optionHeight={50}
          />

          <WalletsDropdown
            className={styles.walletsDropdown}
            label={intl.formatMessage(messages.walletsDropdownLabel)}
            numberOfStakePools={4}
            wallets={wallets}
            onChange={onSelectWallet}
            placeholder={
              'WalletsDropdown Placeholder'
              /* intl.formatMessage(
              messages.selectWalletInputPlaceholder
            ) */
            }
            value={redeemWalletId}
            getStakePoolById={() => {}}
            error={walletsDropdownError}
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
