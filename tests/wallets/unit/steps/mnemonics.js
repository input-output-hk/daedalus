// @flow
import readline from 'readline';
import { Given, When, Then } from 'cucumber';
import { range } from 'lodash';
import { generateAccountMnemonics, generateAdditionalMnemonics, scrambleMnemonics, unscrambleMnemonics } from '../../../../source/renderer/app/api/utils/mnemonics';
import { mnemonicToSeedHex, getScrambledInput } from '../../../../source/renderer/app/utils/crypto';
import { isValidMnemonic } from '../../../../source/common/crypto/decrypt';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../../source/renderer/app/config/cryptoConfig';

const isValidWalletRecoveryPhrase = mnemonic =>
  isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT);

Given('I generate {int} wallet recovery mnemonics', function(
  numberOfMnemonics
) {
  this.context.mnemonics = range(numberOfMnemonics).map(() =>
    generateAccountMnemonics().join(' ')
  );
});

Then('all generated wallet recovery mnemonics should be valid', function() {
  for (const mnemonic of this.context.mnemonics) {
    if (!isValidWalletRecoveryPhrase(mnemonic)) {
      throw new Error(`"${mnemonic}" is not valid`);
    }
  }
});

Given(
  'I generate and validate an unbound number of wallet recovery mnemonics',
  function() {
    let numberOfTestsExecuted = 0;
    let generated = true;
    while (generated) {
      const mnemonic = generateAccountMnemonics().join(' ');
      if (!isValidWalletRecoveryPhrase(mnemonic)) {
        generated = false;
        throw new Error(`"${mnemonic}" is not valid`);
      }
      numberOfTestsExecuted++;
      readline.clearLine(process.stdout, 0);
      readline.cursorTo(process.stdout, 0);
      process.stdout.write(`${numberOfTestsExecuted} mnemonics validated.`);
    }
  }
);

When('I generate additional mnemonic words', function() {
  this.context.additionalMnemonicWords = generateAdditionalMnemonics().join(' ');
});

When('I generate spending password from 9-word mnemonic', function() {
  this.context.spendingPassword = mnemonicToSeedHex(this.context.additionalMnemonicWords);
});

When('I scramble mnemonics', function() {
  const scrambledMnemonics = scrambleMnemonics({ passphrase: this.context.spendingPassword, scrambledInput: this.context.mnemonics[0] })
  // Split recovery phrase to 18 (scrambled mnemonics) + 9 (mnemonics seed) mnemonics
  const { passphrase, scrambledInput } = getScrambledInput(scrambledMnemonics);
  // Unscramble 18-word wallet certificate mnemonic to 12-word mnemonic
  const unscrambledMnemonics = unscrambleMnemonics({ passphrase, scrambledInput });
});
