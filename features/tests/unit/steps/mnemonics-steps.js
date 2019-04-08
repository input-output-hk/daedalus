import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { range } from 'lodash';
import { generateAccountMnemonics } from '../../../../source/renderer/app/api/utils/mnemonics';
import { isValidMnemonic } from '../../../../source/common/crypto/decrypt';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../../source/renderer/app/config/cryptoConfig';

Given('I generate {int} wallet recovery mnemonics', function(
  numberOfMnemonics
) {
  this.context.mnemonics = range(numberOfMnemonics).map(() =>
    generateAccountMnemonics().join(' ')
  );
});

Then('all generated wallet recovery mnemonics should be valid', function() {
  for (const mnemonic of this.context.mnemonics) {
    if (!isValidMnemonic(mnemonic, WALLET_RECOVERY_PHRASE_WORD_COUNT)) {
      throw new Error(`"${mnemonic}" is not valid`);
    }
  }
});
