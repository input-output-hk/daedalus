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
  invalidMnemonic: {
    id: 'global.errors.invalidMnemonic',
    defaultMessage: '!!!Invalid phrase entered, please check.',
    description: 'Error message shown when invalid bip39 mnemonic was entered.',
  },
  invalidEmail: {
    id: 'global.errors.invalidEmail',
    defaultMessage: '!!!Invalid email entered, please check.',
    description: 'Error message shown when invalid email was entered.',
  },
  invalidWalletName: {
    id: 'global.errors.invalidWalletName',
    defaultMessage:
      '!!!Wallet name requires at least 3 and at most 40 letters.',
    description:
      'Error message shown when invalid wallet name was entered in create wallet dialog.',
  },
  invalidSpendingPassword: {
    id: 'global.errors.invalidSpendingPassword',
    defaultMessage: '!!!Invalid password',
    description:
      'Error message shown when invalid wallet password was entered in create wallet dialog.',
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
  passwordInstructions: {
    id: 'global.passwordInstructions',
    defaultMessage:
      '!!!Note that password needs to be at least 10 characters long, and have at least 1 uppercase, 1 lowercase letter and 1 number.',
    description: 'Password instructions note.',
  },
  cancel: {
    id: 'global.labels.cancel',
    defaultMessage: '!!!Cancel',
    description:
      'The word "cancel" reused at several places (like cancel buttons)',
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
});
