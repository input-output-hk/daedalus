import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { pickBy, identity } from 'lodash';
import {
  generateFileNameWithTimestamp,
  defaultProps,
} from '../../../../source/common/utils/files';

const getDataFromFunction = props => {
  const filename = generateFileNameWithTimestamp(props);
  const prefix = filename.match(/^[^-]*[^ -]/i)[0];
  const extention = filename.match(/\.[0-9a-z]+$/i)[0].replace('.', '');
  const isUTC = !!filename.match(`Z.${extention}`);
  return {
    filename,
    prefix,
    extention,
    isUTC,
  };
};

Given('I dont pass any props to the function', function() {
  const { filename, extention, prefix, isUTC } = getDataFromFunction();
  this.context.filename = filename;
  this.context.extention = extention;
  this.context.prefix = prefix;
  this.context.isUTC = isUTC;
});

Given('I pass the following props to the function:', function(data) {
  let [expectedProps] = data.hashes();
  expectedProps = pickBy(expectedProps, identity);
  if (expectedProps.isUTC)
    expectedProps.isUTC = Boolean(expectedProps.isUTC === 'should');
  const { filename, extention, prefix, isUTC } = getDataFromFunction(
    expectedProps
  );
  this.context.filename = filename;
  this.context.extention = extention;
  this.context.prefix = prefix;
  this.context.isUTC = isUTC;
});

Then('the prefix should be {string}', function(prefix) {
  const expectedPrefix = prefix || defaultProps.prefix;
  expect(this.context.prefix).to.equal(expectedPrefix);
});

Then('the extention should be {string}', function(extention) {
  const expectedExtention = extention || defaultProps.extention;
  expect(this.context.extention).to.equal(expectedExtention);
});

Then(/^the time (should|shouldn't) be converted into UTC/, function(state) {
  const isUTC = state === 'should';
  expect(isUTC).to.equal(this.context.isUTC);
});
