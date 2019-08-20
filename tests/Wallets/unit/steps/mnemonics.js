import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { range } from 'lodash';
import { generateAccountMnemonics } from '../../../../source/renderer/app/api/utils/mnemonics';
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
    while (true) {
      const mnemonic = generateAccountMnemonics().join(' ');
      if (!isValidWalletRecoveryPhrase(mnemonic)) {
        throw new Error(`"${mnemonic}" is not valid`);
      }
      numberOfTestsExecuted++;
      process.stdout.clearLine();
      process.stdout.cursorTo(0);
      process.stdout.write(numberOfTestsExecuted + ' mnemonics validated.');
    }
  }
);
