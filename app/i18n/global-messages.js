import { defineMessages } from 'react-intl';

export default defineMessages({
  fieldIsRequired: {
    id: 'global.errors.fieldIsRequired',
    defaultMessage: '!!!This field is required.',
    description: 'Error message when required fields are left empty.'
  },
  invalidWalletName: {
    id: 'global.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
  invalidWalletPassword: {
    id: 'global.errors.invalidWalletPassword',
    defaultMessage: '!!!Minimum 6 letters.',
    description: 'Error message shown when invalid wallet password was entered in create wallet dialog.'
  },
  invalidRepeatPassword: {
    id: 'global.errors.invalidRepeatPassword',
    defaultMessage: '!!!Doesn\'t match.',
    description: 'Error message shown when wallet password and repeat passwords don\'t match in create wallet dialog.'
  },
  cancel: {
    id: 'global.labels.cancel',
    defaultMessage: '!!!Cancel',
    description: 'The word "cancel" reused at several places (like cancel buttons)',
  },
  change: {
    id: 'global.labels.change',
    defaultMessage: '!!!Change',
    description: 'The word "change" reused at several places (like change buttons)',
  },
  save: {
    id: 'global.labels.save',
    defaultMessage: '!!!Save',
    description: 'The word "save" reused at several places (like save buttons)',
  },
  invalidMnemonic: {
    id: 'global.errors.invalidMnemonic',
    defaultMessage: '!!!Invalid phrase entered, please check.',
    description: 'Error message shown when invalid bip39 mnemonic was entered.'
  },
  invalidAdaRedemptionCertificate: {
    id: 'global.errors.AdaRedemptionCertificateParseError',
    defaultMessage: '!!!The ADA redemption code could not be parsed from the given document.',
    description: 'Error message shown when invalid Ada redemption certificate was uploaded.',
  },
  invalidEmail: {
    id: 'global.errors.invalidEmail',
    defaultMessage: '!!!Invalid email entered, please check.',
    description: 'Error message shown when invalid email was entered.'
  },
  languageEnglish: {
    id: 'global.language.english',
    defaultMessage: '!!!English',
    description: 'Language name for "English" language.'
  },
  languageJapanese: {
    id: 'global.language.japanese',
    defaultMessage: '!!!Japanese',
    description: 'Language name for "Japanese" language.'
  },
  languageChinese: {
    id: 'global.language.chinese',
    defaultMessage: '!!!Chinese',
    description: 'Language name for "Chinese" language.'
  },
  languageKorean: {
    id: 'global.language.korean',
    defaultMessage: '!!!Korean',
    description: 'Language name for "Korean" language.'
  },
  languageGerman: {
    id: 'global.language.german',
    defaultMessage: '!!!German',
    description: 'Language name for "German" language.'
  },
  languageCroatian: {
    id: 'global.language.croatian',
    defaultMessage: '!!!Croatian',
    description: 'Language name for "Croatian" language.'
  },
  assuranceLevelNormal: {
    id: 'global.assuranceLevel.normal',
    defaultMessage: '!!!Normal',
    description: 'Name for "Normal" transaction assurance security level.'
  },
  assuranceLevelStrict: {
    id: 'global.assuranceLevel.strict',
    defaultMessage: '!!!Strict',
    description: 'Name for "Strict" transaction assurance security level.'
  },
  unitAda: {
    id: 'global.unit.ada',
    defaultMessage: '!!!Ada',
    description: 'Name for "Ada" unit.'
  },
  transactionAssuranceLevelLow: {
    id: 'wallet.transaction.assuranceLevel.low',
    defaultMessage: '!!!low',
    description: 'Transaction assurance level "low".',
  },
  transactionAssuranceLevelMedium: {
    id: 'wallet.transaction.assuranceLevel.medium',
    defaultMessage: '!!!medium',
    description: 'Transaction assurance level "medium".',
  },
  transactionAssuranceLevelHigh: {
    id: 'wallet.transaction.assuranceLevel.high',
    defaultMessage: '!!!high',
    description: 'Transaction assurance level "high".',
  },
});
