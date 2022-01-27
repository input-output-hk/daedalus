import { Given, Then } from "cucumber";
import { expect } from "chai";
import { pickBy, identity } from "lodash";
import { generateFileNameWithTimestamp, defaultProps } from "../../../../source/common/utils/files";

const getDataFromFunction = props => {
  const filename = generateFileNameWithTimestamp(props);
  let prefix = filename.match(/^[^-]*[^ -]/i);
  let extension = filename.match(/\.[0-9a-z]+$/i);
  let isUTC = false;

  if (prefix) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'RegExpMat... Remove this comment to see the full error message
    prefix = prefix[0];
  }

  if (extension) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'RegExpMat... Remove this comment to see the full error message
    extension = extension[0].replace('.', '');
  }

  if (extension) {
    isUTC = !!filename.match(`Z.${extension}`);
  }

  return {
    filename,
    prefix,
    extension,
    isUTC
  };
};

Given('I dont pass any props to the function', function () {
  const {
    filename,
    extension,
    prefix,
    isUTC
  // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
  } = getDataFromFunction();
  this.context.filename = filename;
  this.context.extension = extension;
  this.context.prefix = prefix;
  this.context.isUTC = isUTC;
});
Given('I pass the following props to the function:', function (data) {
  let [expectedProps] = data.hashes();
  expectedProps = pickBy(expectedProps, identity);
  if (expectedProps.isUTC) expectedProps.isUTC = Boolean(expectedProps.isUTC === 'should');
  const {
    filename,
    extension,
    prefix,
    isUTC
  } = getDataFromFunction(expectedProps);
  this.context.filename = filename;
  this.context.extension = extension;
  this.context.prefix = prefix;
  this.context.isUTC = isUTC;
});
Then('the prefix should be {string}', function (prefix) {
  const expectedPrefix = prefix || defaultProps.prefix;
  expect(this.context.prefix).to.equal(expectedPrefix);
});
Then('the extension should be {string}', function (extension) {
  const expectedExtension = extension || defaultProps.extension;
  expect(this.context.extension).to.equal(expectedExtension);
});
Then(/^the time (should|shouldn't) be converted into UTC/, function (state) {
  const isUTC = state === 'should';
  expect(isUTC).to.equal(this.context.isUTC);
});