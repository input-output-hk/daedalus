import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { sampleSize } from "lodash";
import sinon from "sinon";
import validWords from "../../../../source/common/config/crypto/valid-words.en";
import { validateMnemonics, INCOMPLETE_MNEMONIC_MARKER } from "../../../../source/renderer/app/utils/validations";

const STUB_VALIDATOR_RETURN_VALUE = Symbol('validator return value');
Given('I require {int} mnemonic words', function (requiredWordsCount) {
  this.context.requiredWords = requiredWordsCount;
});
When('I provide {int} mnemonic words', function (providedWordsCount) {
  this.context.providedWords = sampleSize(validWords, providedWordsCount);
});
When('I provide an mnemonics validation function', async function () {
  this.context.validator = sinon.stub().returns(STUB_VALIDATOR_RETURN_VALUE);
});
When('I validate the mnemonics with these parameters', async function () {
  this.context.result = validateMnemonics(this.context);
});
Then('the result should be an incomplete mnemonic marker', async function () {
  expect(this.context.result).to.equal(INCOMPLETE_MNEMONIC_MARKER);
});
Then('the mnemonics validator should not have been called', async function () {
  expect(this.context.validator.called).to.equal(false);
});
Then('the result should be the return value of the mnemonics validator', async function () {
  expect(this.context.result).to.equal(STUB_VALIDATOR_RETURN_VALUE);
});
Then('the mnemonics validator should have been called with the provided words', async function () {
  expect(this.context.validator.calledWithExactly(this.context.providedWords)).to.equal(true);
});