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
  console.log('>>> 1. MNEMONICS: ', this.context.mnemonics); // 15-words
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
  this.context.additionalMnemonicWords = generateAdditionalMnemonics().join(' '); // 9-words
  console.log('>>> 2. additionalMnemonicWords: ', this.context.additionalMnemonicWords);
});

When('I generate spending password from 9-word mnemonic', function() {
  this.context.spendingPassword = mnemonicToSeedHex(this.context.additionalMnemonicWords); // spending password
  console.log('>>> 3. spendingPassword from 9-words mnemonic: ', this.context.spendingPassword);
});

When('I scramble mnemonics', function() {
  // this.context.spendingPassword = mnemonicToSeedHex(this.context.additionalMnemonicWords); // spending password
  // try {
  //   this.context.scrambledMnemonics = scramblePaperWalletMnemonic(this.context.spendingPassword, this.context.mnemonics[0]);
  // } catch(e) {
  //   console.log('ERROR: ', e)
  // }
  const scrambledMnemonics = scrambleMnemonics({ passphrase: this.context.spendingPassword, scrambledInput: this.context.mnemonics[0] })
  console.log('>>>>> scrambledMnemonics: ', scrambledMnemonics);
  // console.log('>>> 3. spendingPassword from 9-words mnemonic: ', this.context.scrambledMnemonics);

  console.log('>>>>> UNSCRAMBLE');

  const { passphrase, scrambledInput } = getScrambledInput(scrambledMnemonics);
//
  console.log('>>>>> UNSCRAMBLED passphrase: ', passphrase);
  console.log('>>>>> UNSCRAMBLED scrambledInput: ', scrambledInput);
//
  const unscrambledMnemonics = unscrambleMnemonics({ passphrase, scrambledInput });
  console.log('>>>>> DONE: ', unscrambledMnemonics);
});
