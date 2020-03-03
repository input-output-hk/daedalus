// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers, chooseCustomOptionsByValue, getSelectedCustomOptions, getValueFromSelector, screenElementSelectors } from './helpers';
import { timeout } from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I choose the following custom formats:$/, async function(formatsTable) {
  const chosenFormats = formatsTable.hashes();
  const [
    { value: numberValue },
    { value: dateValue },
    { value: timeValue },
  ] = chosenFormats;
  await chooseCustomOptionsByValue.call(this, numberValue, dateValue, timeValue);
});

Given(/^I have changed the following custom formats:$/, async function(formatsTable) {
  const chosenFormats = formatsTable.hashes();
  await this.client.executeAsync((chosenFormats, done) => {
    Promise.all(
      chosenFormats.map(({ param, value }) =>
        daedalus.stores.profile._updateUserLocalSetting({ param: `${param}Format`, value }))
    ).then(done);
  }, chosenFormats);
  await timeout(1500);
});

Then(/^I should see the following chosen options:$/, async function(expectedTable) {
  const expectedValues = expectedTable.hashes();
  const [
    { value: expectedNumber },
    { value: expectedDate },
    { value: expectedTime },
  ] = expectedValues;
  const {
    selectedNumber,
    selectedDate,
    selectedTime,
  } = await getSelectedCustomOptions.call(this);
  expect(selectedNumber).to.equal(expectedNumber);
  expect(selectedDate).to.equal(expectedDate);
  expect(selectedTime).to.equal(expectedTime);
});

const paramsMatchersValues = {
  date: (expectedValue: string) =>
    expectedValue
      .replace('MM', '(0[1-9]|1[0-2])')
      .replace('DD', '(0[1-9]|[12]\\d|3[01])')
      .replace('YYYY', '\\d{4}'),
  time: (expectedValue: string) => expectedValue === 'hh:mm:ss A'
    ? '[0-1]\\d:[0-5]\\d(:[0-5]\\d)? [AP]M'
    : '[0-2]\\d:[0-5]\\d:([0-5]\\d)?',
  number: (expectedValue: string) => {
    let [ thousandsSeparator, decimalSeparator ] = expectedValue.split('');
    if (!decimalSeparator) {
      decimalSeparator = thousandsSeparator;
      thousandsSeparator = ' ';
    }
    return `((${thousandsSeparator})?\\d+)+${decimalSeparator}\\d{6}$`;
  }
}

Then(/^the "([^"]*)" should display the following custom formats:$/, async function(screenElement, expectedTable) {
  const expectedValues = expectedTable.hashes();
  for (let i = 0; i < expectedValues.length; i++) {
    const { param: expectedParam, value: expectedValue } = expectedValues[i];
    const currentValue = await getValueFromSelector.call(this, screenElement, expectedParam)
    const expectedMatcher = new RegExp(paramsMatchersValues[expectedParam](expectedValue));
    const matcher = expectedMatcher.test(currentValue)
    expect(matcher).to.be.true;
  }
});

Then(/^the "([^"]*)" should display the "([^"]*)" of value "([^"]*)"$/, async function(screenElement, expectedParam, expectedValue) {
  const currentValue = await getValueFromSelector.call(this, screenElement, expectedParam)
  expect(currentValue).to.equal(expectedValue);
});

When(/^the "([^"]*)" wallet has received the transaction amount$/, async function(walletName) {
  await this.client.waitUntil(async () => {
    const currentAmount = await getValueFromSelector.call(this, walletName, 'number')
    return currentAmount !== '0 ADA';
  });
});

