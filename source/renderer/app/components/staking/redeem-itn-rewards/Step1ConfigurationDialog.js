// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import classnames from 'classnames';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import BigNumber from 'bignumber.js';
import Wallet from '../../../domains/Wallet';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Dialog from '../../widgets/Dialog';
import styles from './Step1ConfigurationDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import { ITN_WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { MIN_DELEGATION_FUNDS } from '../../../config/stakingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.redeemItnRewards.step1.title',
    defaultMessage: '!!!Redeem Incentivized Testnet rewards',
    description: 'Title for Redeem Incentivized Testnet - Step 1',
  },
  description1: {
    id: 'staking.redeemItnRewards.step1.description1',
    defaultMessage:
      '!!!If you participated in the { itnLink } and earned rewards by running a stake pool or delegating your stake, you can use this feature to redeem your rewards as ada on the Cardano mainnet.',
    description: 'description for Redeem Incentivized Testnet - Step 1',
  },
  description2: {
    id: 'staking.redeemItnRewards.step1.description2',
    defaultMessage:
      '!!!You will need the <b>wallet recovery phrase for the Incentivized Testnet wallet</b> used to earn rewards and an existing mainnet wallet in Daedalus to which the rewards will be transferred. This wallet will also be used to pay any applicable transaction fees.',
    description: 'description for Redeem Incentivized Testnet - Step 1',
  },
  descriptionItnLinkLabel: {
    id: 'staking.redeemItnRewards.step1.descriptionItnLinkLabel',
    defaultMessage: '!!!Incentivized Testnet',
    description:
      'descriptionItnLinkLabel for Redeem Incentivized Testnet - Step 1',
  },
  descriptionItnLinkUrl: {
    id: 'staking.redeemItnRewards.step1.descriptionItnLinkUrl',
    defaultMessage: '!!!https://staking.cardano.org/',
    description:
      'descriptionItnLinkUrl for Redeem Incentivized Testnet - Step 1',
  },
  recoveryPhraseLabel: {
    id: 'staking.redeemItnRewards.step1.recoveryPhraseLabel',
    defaultMessage: '!!!Wallet recovery phrase:',
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
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900001656586',
    description: 'learnMoreLinkUrl for Redeem Incentivized Testnet - Step 1',
  },
  recoveryPhraseInputHint: {
    id: 'staking.redeemItnRewards.step1.recoveryPhraseInputHint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      'Hint "Enter recovery phrase" for the recovery phrase input on the wallet restore dialog.',
  },
  selectWalletInputPlaceholder: {
    id: 'staking.redeemItnRewards.step1.selectWalletInputPlaceholder',
    defaultMessage: '!!!Select Wallet',
    description:
      'Placeholder "Select Wallet" for select input on the delegation setup "choose wallet" step dialog.',
  },
  noResults: {
    id: 'staking.redeemItnRewards.step1.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  invalidRecoveryPhrase: {
    id: 'staking.redeemItnRewards.step1.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  errorMinDelegationFunds: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFunds',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {minDelegationFunds} ADA which is required for delegation to be available. Please select a wallet with <span>a minimum amount of {minDelegationFunds} ADA</span> and click continue.',
    description:
      'errorMinDelegationFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFundsRewardsOnly: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet contains only rewards balances so it cannot be delegated.',
    description:
      'errorMinDelegationFundsRewardsOnly Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for delegation while it’s being synced.',
    description:
      'RestoringWallet Error Label on the delegation setup "choose wallet" step dialog.',
  },
  syncingSavingsWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.syncingSavingsWallet',
    defaultMessage: '!!!Savings',
    description:
      'Syncing wallet savings label on the delegation setup "choose wallet" step dialog.',
  },
  syncingWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.syncingWallet',
    defaultMessage: '!!!in sync',
    description:
      'Syncing wallet label on the delegation setup "choose wallet" step dialog.',
  },
});

type Props = {
  error?: ?LocalizableError,
  isSubmitting: boolean,
  mnemonicValidator: Function,
  onClose: Function,
  onContinue: Function,
  onSelectWallet: Function,
  openExternalLink: Function,
  wallet: ?Wallet,
  suggestedMnemonics: Array<string>,
  recoveryPhrase?: ?Array<string>,
  wallets: Array<Wallet>,
};

@observer
export default class Step1ConfigurationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    error: null,
    recoveryPhrase: [],
  };

  recoveryPhraseAutocomplete: Autocomplete;

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [...(this.props.recoveryPhrase || [])],
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          validators: ({ field }) => {
            const { intl } = this.context;
            const enteredWords = field.value;
            const wordCount = enteredWords.length;
            const expectedWordCount = ITN_WALLET_RECOVERY_PHRASE_WORD_COUNT;
            const value = enteredWords.join(' ');
            const isPhraseComplete = wordCount === expectedWordCount;
            if (!isPhraseComplete) {
              return [
                false,
                intl.formatMessage(globalMessages.incompleteMnemonic, {
                  expected: expectedWordCount,
                }),
              ];
            }
            return [
              this.props.mnemonicValidator(value, expectedWordCount),
              this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
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

  isWalletAcceptable = (
    walletAmount?: BigNumber,
    walletReward?: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_DELEGATION_FUNDS)) &&
    !walletAmount.equals(walletReward);

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { onContinue } = this.props;
        const { recoveryPhrase } = form.values();
        onContinue({ recoveryPhrase });
      },
    });
  };

  get canSubmit() {
    const { isSubmitting, wallet } = this.props;
    const { form } = this;
    const { checked: checkboxAcceptance1isChecked } = form.$(
      'checkboxAcceptance1'
    );
    const { checked: checkboxAcceptance2isChecked } = form.$(
      'checkboxAcceptance2'
    );
    return (
      !isSubmitting &&
      wallet &&
      checkboxAcceptance1isChecked &&
      checkboxAcceptance2isChecked &&
      form.isValid
    );
  }

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      isSubmitting,
      onClose,
      onContinue,
      onSelectWallet,
      wallet,
      suggestedMnemonics,
      openExternalLink,
      wallets,
      recoveryPhrase,
    } = this.props;
    let { error } = this.props;
    if (
      error &&
      (error.id === 'api.errors.NotEnoughFundsForTransactionFeesError' ||
        error.id === 'api.errors.NotEnoughMoneyToSendError')
    )
      error = messages.walletsDropdownError;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const walletsDropdownField = form.$('walletsDropdown');
    const checkboxAcceptance1Field = form.$('checkboxAcceptance1');
    const checkboxAcceptance2Field = form.$('checkboxAcceptance2');
    const walletId = get(wallet, 'id', null);

    const buttonClasses = classnames([
      'primary',
      isSubmitting ? styles.isSubmitting : null,
    ]);

    const actions = {
      direction: 'column',
      items: [
        {
          className: buttonClasses,
          disabled: !this.canSubmit,
          primary: true,
          label: intl.formatMessage(messages.continueButtonLabel),
          onClick: this.submit,
        },
        {
          onClick: (event: MouseEvent) =>
            openExternalLink(
              intl.formatMessage(messages.learnMoreLinkUrl, event)
            ),
          label: intl.formatMessage(messages.learnMoreLinkLabel),
          isLink: true,
        },
      ],
    };

    const itnLink = (
      <Link
        className={styles.itnLink}
        onClick={(event: MouseEvent) =>
          openExternalLink(
            intl.formatMessage(messages.descriptionItnLinkUrl, event)
          )
        }
        label={intl.formatMessage(messages.descriptionItnLinkLabel)}
        skin={LinkSkin}
      />
    );

    const closeButton = <DialogCloseButton onClose={onClose} />;

    const selectedWallet: ?Wallet = wallets.find(
      (current: Wallet) => current && current.id === walletId
    );

    const { amount, reward, isRestoring } = selectedWallet || {};

    let errorMessage;
    if (selectedWallet && !this.isWalletAcceptable(amount, reward)) {
      // Wallet is restoring
      if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.equals(reward))
        errorMessage = messages.errorMinDelegationFundsRewardsOnly;
      // Wallet balance < min delegation funds
      else errorMessage = messages.errorMinDelegationFunds;
    }

    const minDelegationFunds = MIN_DELEGATION_FUNDS;

    const dropdownError = errorMessage && (
      <p className={styles.errorMessage}>
        <FormattedHTMLMessage
          {...errorMessage}
          values={{ minDelegationFunds }}
        />
      </p>
    );

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        onContinue={onContinue}
        onClose={onClose}
        closeButton={closeButton}
        closeOnOverlayClick={false}
        fullSize
      >
        <div className={styles.component}>
          <p className={styles.description}>
            <FormattedMessage
              {...messages.description1}
              values={{
                itnLink,
              }}
            />{' '}
            <FormattedHTMLMessage {...messages.description2} />
          </p>
          <Autocomplete
            {...recoveryPhraseField.bind()}
            ref={autocomplete => {
              this.recoveryPhraseAutocomplete = autocomplete;
            }}
            options={suggestedMnemonics}
            maxSelections={ITN_WALLET_RECOVERY_PHRASE_WORD_COUNT}
            error={recoveryPhraseField.error}
            maxVisibleOptions={5}
            noResultsMessage={intl.formatMessage(messages.noResults)}
            className={styles.recoveryPhrase}
            skin={AutocompleteSkin}
            optionHeight={50}
            preselectedOptions={[...(recoveryPhrase || [])]}
          />
          <div className={styles.walletsDropdownWrapper}>
            <WalletsDropdown
              className={styles.walletsDropdown}
              {...walletsDropdownField.bind()}
              numberOfStakePools={4}
              wallets={wallets}
              onChange={onSelectWallet}
              placeholder={intl.formatMessage(
                messages.selectWalletInputPlaceholder
              )}
              syncingSavingsLabel={intl.formatMessage(
                messages.syncingSavingsWallet
              )}
              syncingLabel={intl.formatMessage(messages.syncingWallet)}
              value={walletId}
              getStakePoolById={() => {}}
              errorPosition="bottom"
            />
          </div>
          {dropdownError}
          <Checkbox
            {...checkboxAcceptance1Field.bind()}
            className={styles.checkbox}
            skin={CheckboxSkin}
            error={checkboxAcceptance1Field.error}
          />
          <Checkbox
            {...checkboxAcceptance2Field.bind()}
            className={styles.checkbox}
            skin={CheckboxSkin}
            error={checkboxAcceptance2Field.error}
          />
          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </div>
      </Dialog>
    );
  }
}
