// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape } from 'react-intl';
import Wallet from '../../../domains/Wallet';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Dialog from '../../widgets/Dialog';
// @REDEEM TODO - color variable
import styles from './Step1ConfigurationDialog.scss';
import redeemDialogOverride from './RedeemDialogOverride.scss';
import ReactToolboxMobxForm /* ,  {handleFormErrors } */ from '../../../utils/ReactToolboxMobxForm';
// import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

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
    description:
      'walletsDropdownError for Redeem Incentivized Testnet - Step 1',
  },
  checkbox1Label: {
    id: 'staking.redeemItnRewards.step1.checkbox1Label',
    defaultMessage:
      '!!!I understand that redeeming rewards from the Incentivized Testnet requires paying transaction fees.',
    description: 'checkbox1Label for Redeem Incentivized Testnet - Step 1',
  },
  checkbox2Label: {
    id: 'staking.redeemItnRewards.step1.checkbox2Label',
    defaultMessage:
      '!!!I understand that fees will be paid from the wallet I am redeeming my rewards to.',
    description: 'checkbox2Label for Redeem Incentivized Testnet - Step 1',
  },
  continueButtonLabel: {
    id: 'staking.redeemItnRewards.step1.continueButton.label',
    defaultMessage: '!!!Continue',
    description: 'continueButtonLabel for Redeem Incentivized Testnet - Step 1',
  },
  learnMoreLinkLabel: {
    id: 'staking.redeemItnRewards.step1.learnMoreLink.label',
    defaultMessage: '!!!Learn More',
    description: 'learnMoreLinkLabel for Redeem Incentivized Testnet - Step 1',
  },
  learnMoreLinkUrl: {
    id: 'staking.redeemItnRewards.step1.learnMoreLink.url',
    defaultMessage: '!!!url',
    description: 'learnMoreLinkUrl for Redeem Incentivized Testnet - Step 1',
  },
});

type Props = {
  error?: ?string,
  isSubmitting: boolean,
  isWalletValid?: boolean,
  mnemonicValidator: Function,
  onClose: Function,
  onContinue: Function,
  onSelectWallet: Function,
  redeemWallet?: ?Wallet,
  suggestedMnemonics: Array<string>,
  wallets: Array<Wallet>,
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

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          validators: ({ field }) => {
            // const { intl } = this.context;
            const enteredWords = field.value;
            const wordCount = enteredWords.length;
            const expectedWordCount = WALLET_RECOVERY_PHRASE_WORD_COUNT;
            const value = enteredWords.join(' ');
            const isPhraseComplete = wordCount === expectedWordCount;
            if (!isPhraseComplete) {
              return [false, 'INCOMPLETE'];
            }
            return [
              this.props.mnemonicValidator(value, expectedWordCount),
              'this.context.intl.formatMessage(messages.invalidRecoveryPhrase)',
            ];
          },
        },
        walletsDropdown: {
          type: 'select',
          label: this.context.intl.formatMessage(messages.walletsDropdownLabel),
        },
        checkboxAcceptance1: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(messages.checkbox1Label),
        },
        checkboxAcceptance2: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(messages.checkbox2Label),
        },
      },
    },
    {
      plugins: { vjf: vjf() },
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

  get walletsDropdownError() {
    const { intl } = this.context;
    const { redeemWallet } = this.props;
    let walletsDropdownError;
    if (redeemWallet && redeemWallet.amount.isZero())
      walletsDropdownError = intl.formatMessage(messages.walletsDropdownError);
    return walletsDropdownError;
  }

  get canSubmit() {
    return true;
    // @REDEEM TODO:
    // const { isSubmitting, isWalletValid } = this.props;
    // const { form } = this;
    // const { checked: checkboxAcceptance1isChecked } = form.$(
    //   'checkboxAcceptance1'
    // );
    // const { checked: checkboxAcceptance2isChecked } = form.$(
    //   'checkboxAcceptance2'
    // );
    // return (
    //   !isSubmitting &&
    //   isWalletValid &&
    //   checkboxAcceptance1isChecked &&
    //   checkboxAcceptance2isChecked &&
    //   !this.walletsDropdownError &&
    //   form.isValid
    // );
  }

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      isSubmitting,
      onClose,
      onContinue,
      onSelectWallet,
      redeemWallet,
      suggestedMnemonics,
      wallets,
    } = this.props;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const walletsDropdownField = form.$('walletsDropdown');
    const checkboxAcceptance1Field = form.$('checkboxAcceptance1');
    const checkboxAcceptance2Field = form.$('checkboxAcceptance2');
    const redeemWalletId = get(redeemWallet, 'id', null);

    return (
      <>
        <DialogCloseButton className={redeemDialogOverride.closeButton} />
        <Dialog
          title={intl.formatMessage(messages.title)}
          actions={[
            {
              className: isSubmitting ? styles.isSubmitting : null,
              disabled: !this.canSubmit,
              primary: true,
              label: intl.formatMessage(messages.continueButtonLabel),
              onClick: () =>
                onContinue({
                  wallet: wallets[0],
                  recoveryPhrase: ['one', 'two'],
                }),
            },
          ]}
          onContinue={onContinue}
          onClose={onClose}
          closeButton1={<DialogCloseButton />}
          customThemeOverrides={redeemDialogOverride}
          closeOnOverlayClick={false}
        >
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
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
            {...walletsDropdownField.bind()}
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
            error={this.walletsDropdownError}
          />

          <hr />

          <Checkbox
            {...checkboxAcceptance1Field.bind()}
            className={styles.checkbox1}
            skin={CheckboxSkin}
            error={checkboxAcceptance1Field.error}
          />
          <Checkbox
            {...checkboxAcceptance2Field.bind()}
            skin={CheckboxSkin}
            error={checkboxAcceptance2Field.error}
          />

          {/* error && <p className={styles.error}>{intl.formatMessage(error)}</p> */}
        </Dialog>
      </>
    );
  }
}
