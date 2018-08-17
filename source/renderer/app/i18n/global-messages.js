import { defineMessages } from 'react-intl';

export default defineMessages({
  fieldIsRequired: {
    id: 'global.errors.fieldIsRequired',
    defaultMessage: '!!!This field is required.',
    description: 'Error message when required fields are left empty.'
  },
  incompleteMnemonic: {
    id: 'global.errors.incompleteMnemonic',
    defaultMessage: '!!!Please enter all {expected} words.',
    description: 'Error message shown when incomplete bip39 mnemonic was entered.'
  },
  invalidMnemonic: {
    id: 'global.errors.invalidMnemonic',
    defaultMessage: '!!!Invalid phrase entered, please check.',
    description: 'Error message shown when invalid bip39 mnemonic was entered.'
  },
  invalidEmail: {
    id: 'global.errors.invalidEmail',
    defaultMessage: '!!!Invalid email entered, please check.',
    description: 'Error message shown when invalid email was entered.'
  },
  invalidAdaRedemptionCertificate: {
    id: 'global.errors.AdaRedemptionCertificateParseError',
    defaultMessage: '!!!The ADA redemption code could not be parsed from the given document.',
    description: 'Error message shown when invalid Ada redemption certificate was uploaded.',
  },
  invalidAdaRedemptionEncryptedCertificate: {
    id: 'global.errors.AdaRedemptionEncryptedCertificateParseError',
    defaultMessage: '!!!The ADA redemption code could not be parsed, please check your passphrase.',
    description: 'Error message shown when invalid Ada redemption encrypted certificate was uploaded.',
  },
  invalidWalletName: {
    id: 'global.errors.invalidWalletName',
    defaultMessage: '!!!Wallet name requires at least 3 and at most 40 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
  invalidWalletPassword: {
    id: 'global.errors.invalidWalletPassword',
    defaultMessage: '!!!Invalid password',
    description: 'Error message shown when invalid wallet password was entered in create wallet dialog.'
  },
  invalidRepeatPassword: {
    id: 'global.errors.invalidRepeatPassword',
    defaultMessage: '!!!Doesn\'t match.',
    description: 'Error message shown when wallet password and repeat passwords don\'t match in create wallet dialog.'
  },
  passwordInstructions: {
    id: 'global.passwordInstructions',
    defaultMessage: '!!!Note that password needs to be at least 7 characters long, and have at least 1 uppercase, 1 lowercase letter and 1 number.',
    description: 'Password instructions note.',
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
  create: {
    id: 'global.labels.create',
    defaultMessage: '!!!Create',
    description: 'The word "create" reused at several places (like create buttons)',
  },
  remove: {
    id: 'global.labels.remove',
    defaultMessage: '!!!Remove',
    description: 'The word "remove" reused at several places (like remove buttons)',
  },
  save: {
    id: 'global.labels.save',
    defaultMessage: '!!!Save',
    description: 'The word "save" reused at several places (like save buttons)',
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
  unitEtc: {
    id: 'global.unit.etc',
    defaultMessage: '!!!Etc',
    description: 'Name for "Etc" unit.'
  },
  recoveryPhraseDialogTitle: {
    id: 'wallet.backup.recovery.phrase.dialog.title',
    defaultMessage: '!!!Recovery phrase',
    description: 'Title for the "Recovery Phrase" dialog.'
  },
  spendingPasswordLabel: {
    id: 'global.spendingPasswordLabel',
    defaultMessage: '!!!Spending Password',
    description: 'Label for the "Wallet password" input in the create wallet dialog.',
  },
  spendingPasswordPlaceholder: {
    id: 'global.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
  dialogButtonContinueLabel: {
    id: 'global.dialog.button.continue',
    defaultMessage: '!!!Continue',
    description: 'Label "Continue" in dialogs.'
  },
  faqLinkUrl: {
    id: 'settings.support.faq.faqLinkURL',
    defaultMessage: '!!!https://daedaluswallet.io/faq/',
    description: 'URL for the "FAQ on Daedalus website" link in the FAQ section on the support settings page',
  },
});

export const environmentSpecificMessages = {
  ada: defineMessages({
    currency: {
      id: 'environment.currency.ada',
      defaultMessage: '!!!Ada',
      description: 'Name for "Ada" unit.'
    },
    apiName: {
      id: 'environment.apiName.cardano',
      defaultMessage: '!!!Cardano',
      description: 'Name for "Cardano" client.'
    },
  }),

  etc: defineMessages({
    currency: {
      id: 'environment.currency.etc',
      defaultMessage: '!!!Etc',
      description: 'Name for "Etc" unit.'
    },
    apiName: {
      id: 'environment.apiName.mantis',
      defaultMessage: '!!!Mantis',
      description: 'Name for "Mantis" client.'
    },
  }),
};
