import readline from "readline";
import { expect } from "chai";
import { Given, When, Then } from "cucumber";
import { range, concat } from "lodash";
import { generateAccountMnemonics, generateAdditionalMnemonics, scrambleMnemonics, unscrambleMnemonics } from "../../../../source/renderer/app/api/utils/mnemonics";
import { mnemonicToSeedHex, getScrambledInput, generateMnemonic } from "../../../../source/renderer/app/utils/crypto";
import { isValidMnemonic } from "../../../../source/common/config/crypto/decrypt";
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from "../../../../source/renderer/app/config/cryptoConfig";

const isValidWalletRecoveryPhrase = mnemonic => isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT);

Given('I generate {int} wallet recovery mnemonics', function (numberOfMnemonics) {
  this.context.mnemonics = range(numberOfMnemonics).map(() => generateAccountMnemonics(WALLET_RECOVERY_PHRASE_WORD_COUNT).join(' '));
});
Given('I generate {int} word mnemonic', function (numberOfWords) {
  this.context.mnemonic = generateMnemonic(numberOfWords);
});
Then('all generated wallet recovery mnemonics should be valid', function () {
  for (const mnemonic of this.context.mnemonics) {
    if (!isValidWalletRecoveryPhrase(mnemonic)) {
      throw new Error(`"${mnemonic}" is not valid`);
    }
  }
});
Given('I generate and validate an unbound number of wallet recovery mnemonics', function () {
  let numberOfTestsExecuted = 0;
  let generated = true;

  while (generated) {
    const mnemonic = generateAccountMnemonics(WALLET_RECOVERY_PHRASE_WORD_COUNT).join(' ');

    if (!isValidWalletRecoveryPhrase(mnemonic)) {
      generated = false;
      throw new Error(`"${mnemonic}" is not valid`);
    }

    numberOfTestsExecuted++;
    readline.clearLine(process.stdout, 0);
    readline.cursorTo(process.stdout, 0);
    process.stdout.write(`${numberOfTestsExecuted} mnemonics validated.`);
  }
});
When('I generate additional mnemonic words', function () {
  this.context.additionalMnemonicWords = generateAdditionalMnemonics().join(' ');
});
When('I generate spending password from 9-word mnemonic', function () {
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  this.context.spendingPassword = mnemonicToSeedHex(this.context.additionalMnemonicWords);
});
When('I generate 18-word scrambled mnemonic', function () {
  this.context.scrambledMnemonic = scrambleMnemonics({
    passphrase: this.context.spendingPassword,
    scrambledInput: this.context.mnemonic
  });
});
When('I generate 27-word paper wallet certificate recovery phrase', function () {
  this.context.paperWalletCertificateRecoveryPhrase = concat(this.context.scrambledMnemonic, this.context.additionalMnemonicWords.split(' '));
});
When('I unscramble mnemonics', function () {
  // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
  const {
    passphrase,
    scrambledInput
  } = getScrambledInput(this.context.paperWalletCertificateRecoveryPhrase);
  // Unscramble 18-word wallet certificate mnemonic to 12-word initial mnemonic
  this.context.unscrambledMnemonic = unscrambleMnemonics({
    passphrase,
    scrambledInput
  });
});
Then('Unscrambled mnemonic should be same as generated 12-word mnemonic', function () {
  const unscrambledMnemonic = this.context.unscrambledMnemonic.join(' ');
  expect(unscrambledMnemonic).to.equal(this.context.mnemonic);
});