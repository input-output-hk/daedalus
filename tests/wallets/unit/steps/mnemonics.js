// @flow
// TODO: Merge review
// HEAD:features/tests/unit/steps/mnemonics-steps.js
// =======
// import readline from 'readline';
// >>>>>>> develop:tests/wallets/unit/steps/mnemonics.js
import { Given, Then } from 'cucumber';
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
    let generated = true;
    while (generated) {
      const mnemonic = generateAccountMnemonics().join(' ');
      if (!isValidWalletRecoveryPhrase(mnemonic)) {
        generated = false;
        throw new Error(`"${mnemonic}" is not valid`);
      }
      numberOfTestsExecuted++;
//  HEAD:features/tests/unit/steps/mnemonics-steps.js
      process.stdout.clearLine();
      process.stdout.cursorTo(0);
// =======
//       readline.clearLine(process.stdout, 0);
//       readline.cursorTo(process.stdout, 0);
// >>>>>>> develop:tests/wallets/unit/steps/mnemonics.js
      process.stdout.write(`${numberOfTestsExecuted} mnemonics validated.`);
    }
  }
);
