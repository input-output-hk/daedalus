// @flow
import { defineMessages } from 'react-intl';

export default defineMessages({
  fieldIsRequired: {
    id: 'global.errors.fieldIsRequired',
    defaultMessage: '!!!This field is required.',
    description: 'Error message when required fields are left empty.',
  },
  incompleteMnemonic: {
    id: 'global.errors.incompleteMnemonic',
    defaultMessage: '!!!Please enter all {expected} words.',
    description:
      'Error message shown when incomplete bip39 mnemonic was entered.',
  },
  invalidEmail: {
    id: 'global.errors.invalidEmail',
    defaultMessage: '!!!Invalid email entered, please check.',
    description: 'Error message shown when invalid email was entered.',
  },
  invalidWalletName: {
    id: 'global.errors.invalidWalletName',
    defaultMessage: '!!!Wallet name requires at least 3 and at most 40 letters',
    description:
      'Error message shown when invalid wallet name was entered in create wallet dialog.',
  },
  invalidSpendingPassword: {
    id: 'global.errors.invalidSpendingPassword',
    defaultMessage: '!!!Insecure',
    description:
      'Error message shown when insecure wallet password was entered in a password input.',
  },
  weakSpendingPassword: {
    id: 'global.errors.weakSpendingPassword',
    defaultMessage: '!!!Weak',
    description:
      'Error message shown when weak wallet password was entered in a password input.',
  },
  strongSpendingPassword: {
    id: 'global.errors.strongSpendingPassword',
    defaultMessage: '!!!Strong',
    description:
      'Error message shown when strong wallet password was entered in a password input.',
  },
  invalidRepeatPassword: {
    id: 'global.errors.invalidRepeatPassword',
    defaultMessage: "!!!Doesn't match.",
    description:
      "Error message shown when wallet password and repeat passwords don't match in create wallet dialog.",
  },
  paperWalletOpenPdfError: {
    id: 'global.errors.paperWalletOpenPdfError',
    defaultMessage:
      '!!!The file you are trying to replace is open. Please close it and try again.',
    description:
      'Error message shown when the file the user tries to replace is open.',
  },
  rewardsOpenCsvError: {
    id: 'global.errors.rewardsOpenCsvError',
    defaultMessage:
      '!!!The file you are trying to replace is open. Please close it and try again.',
    description:
      'Error message shown when the file the user tries to replace is open.',
  },
  passwordInstructions: {
    id: 'global.passwordInstructions',
    defaultMessage:
      '!!!Note that password needs to be at least 10 characters long.',
    description: 'Password instructions note.',
  },
  cancel: {
    id: 'global.labels.cancel',
    defaultMessage: '!!!Cancel',
    description:
      'The word "cancel" reused at several places (like cancel buttons)',
  },
  close: {
    id: 'global.labels.close',
    defaultMessage: '!!!Close',
    description:
      'The word "close" reused at several places (like cancel buttons)',
  },
  change: {
    id: 'global.labels.change',
    defaultMessage: '!!!Change',
    description:
      'The word "change" reused at several places (like change buttons)',
  },
  create: {
    id: 'global.labels.create',
    defaultMessage: '!!!Create',
    description:
      'The word "create" reused at several places (like create buttons)',
  },
  remove: {
    id: 'global.labels.remove',
    defaultMessage: '!!!Remove',
    description:
      'The word "remove" reused at several places (like remove buttons)',
  },
  save: {
    id: 'global.labels.save',
    defaultMessage: '!!!Save',
    description: 'The word "save" reused at several places (like save buttons)',
  },
  languageEnglish: {
    id: 'global.language.english',
    defaultMessage: '!!!English',
    description: 'Language name for "English" language.',
  },
  languageJapanese: {
    id: 'global.language.japanese',
    defaultMessage: '!!!Japanese',
    description: 'Language name for "Japanese" language.',
  },
  languageChinese: {
    id: 'global.language.chinese',
    defaultMessage: '!!!Chinese',
    description: 'Language name for "Chinese" language.',
  },
  languageKorean: {
    id: 'global.language.korean',
    defaultMessage: '!!!Korean',
    description: 'Language name for "Korean" language.',
  },
  languageGerman: {
    id: 'global.language.german',
    defaultMessage: '!!!German',
    description: 'Language name for "German" language.',
  },
  languageCroatian: {
    id: 'global.language.croatian',
    defaultMessage: '!!!Croatian',
    description: 'Language name for "Croatian" language.',
  },
  punctuationColon: {
    id: 'global.punctuation.colon',
    defaultMessage: '!!!:',
    description: 'Colon punctuation.',
  },
  punctuationDot: {
    id: 'global.punctuation.dot',
    defaultMessage: '!!!.',
    description: 'Final dot punctuation.',
  },
  unitAda: {
    id: 'global.unit.ada',
    defaultMessage: '!!!ADA',
    description: 'Name for "ADA" unit.',
  },
  recoveryPhraseDialogTitle: {
    id: 'wallet.backup.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.',
  },
  spendingPasswordLabel: {
    id: 'global.spendingPasswordLabel',
    defaultMessage: '!!!Spending Password',
    description:
      'Label for the "Wallet password" input in the create wallet dialog.',
  },
  spendingPasswordPlaceholder: {
    id: 'global.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
  dialogButtonContinueLabel: {
    id: 'global.dialog.button.continue',
    defaultMessage: '!!!Continue',
    description: 'Label "Continue" in dialogs.',
  },
  faqLinkUrl: {
    id: 'settings.support.faq.faqLinkURL',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011451693',
    description:
      'URL for the "Known Issues" link in the "Help and support" section on the support settings page',
  },
  currency: {
    id: 'environment.currency.ada',
    defaultMessage: '!!!ADA',
    description: 'Name for "Ada" unit.',
  },
  apiName: {
    id: 'environment.apiName.cardano',
    defaultMessage: '!!!Cardano',
    description: 'Name for "Cardano" client.',
  },
  network_mainnet: {
    id: 'environment.network.mainnet',
    defaultMessage: '!!!Mainnet',
    description: '"mainnet" Cardano network',
  },
  network_staging: {
    id: 'environment.network.staging',
    defaultMessage: '!!!Staging',
    description: '"staging" Cardano network',
  },
  network_testnet: {
    id: 'environment.network.testnet',
    defaultMessage: '!!!Testnet',
    description: '"testnet" Cardano network',
  },
  network_shelley_testnet: {
    id: 'environment.network.shelley_testnet',
    defaultMessage: '!!!Shelley Testnet',
    description: '"Shelley Testnet" Cardano network',
  },
  network_shelley_qa: {
    id: 'environment.network.shelley_qa',
    defaultMessage: '!!!Shelley QA',
    description: '"Shelley QA" Cardano network',
  },
  network_development: {
    id: 'environment.network.development',
    defaultMessage: '!!!Development',
    description: '"development" Cardano network',
  },
  network_itn_rewards_v1: {
    id: 'environment.network.itn_rewards_v1',
    defaultMessage: '!!!Incentivized Testnet v1 - Rewards',
    description: '"itn_rewards_v1" Cardano network',
  },
  network_itn_selfnode: {
    id: 'environment.network.itn_selfnode',
    defaultMessage: '!!!ITN Selfnode',
    description: '"itn_selfnode" Cardano network',
  },
  network_selfnode: {
    id: 'environment.network.selfnode',
    defaultMessage: '!!!Selfnode',
    description: '"selfnode" Cardano network',
  },
  network_qa: {
    id: 'environment.network.qa',
    defaultMessage: '!!!QA',
    description: '"qa" Cardano network',
  },
  network_nightly: {
    id: 'environment.network.nightly',
    defaultMessage: '!!!Nightly',
    description: '"nightly" Cardano network',
  },
  years: {
    id: 'global.duration.years',
    defaultMessage: '!!!years',
    description: 'Label for years value in duration.',
  },
  months: {
    id: 'global.duration.months',
    defaultMessage: '!!!months',
    description: 'Label for months value in duration.',
  },
  days: {
    id: 'global.duration.days',
    defaultMessage: '!!!days',
    description: 'Label for days value in duration.',
  },
  hours: {
    id: 'global.duration.hours',
    defaultMessage: '!!!hours',
    description: 'Label for hours value in duration.',
  },
  minutes: {
    id: 'global.duration.minutes',
    defaultMessage: '!!!minutes',
    description: 'Label for minutes value in duration.',
  },
  seconds: {
    id: 'global.duration.seconds',
    defaultMessage: '!!!seconds',
    description: 'Label for seconds value in duration.',
  },
  rangeFrom: {
    id: 'global.range.from',
    defaultMessage: '!!!from',
    description: 'From label of range.',
  },
  rangeTo: {
    id: 'global.range.to',
    defaultMessage: '!!!to',
    description: 'To label of range.',
  },
  filter: {
    id: 'global.labels.filter',
    defaultMessage: '!!!Filter',
    description: 'Filter label.',
  },
  all: {
    id: 'global.labels.all',
    defaultMessage: '!!!All',
    description: 'All label.',
  },
  reset: {
    id: 'global.labels.reset',
    defaultMessage: '!!!Reset',
    description: 'Reset label.',
  },
  token: {
    id: 'global.labels.token',
    defaultMessage: '!!!Token',
    description: 'Token description.',
  },
});
