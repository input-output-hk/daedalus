// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers, chooseCustomOptionsByValue, getSelectedCustomOptions } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I have chosen the following custom formats:$/, async function(formatsTable) {
  const chosenFormats = formatsTable.hashes();
  const [
    { value: numberValue },
    { value: dateValue },
    { value: timeValue },
  ] = chosenFormats;
  await chooseCustomOptionsByValue.call(this, numberValue, dateValue, timeValue);
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

const screenElementSelectors = {
  alert: {
    date: '.AlertsOverlay_date',
  },
  incident: {
    date: '.IncidentOverlay_date',
  },
  announcement: {
    date: '.NewsItem_newsItemDate',
  },
  info: {
    date: '.NewsItem_newsItemDate',
  },
  transaction: {
    date: '.WalletTransactionsList_groupDate',
    time: '.Transaction_type em',
  },
  'transaction filter': {
    date: '.FilterDialog_fromDateInput input',
  },
};

const paramsMatchersValues = {
  date: (expectedValue: string) =>
    expectedValue
      .replace('MM', '(0[1-9]|1[0-2])')
      .replace('DD', '(0[1-9]|[12]\\d|3[01])')
      .replace('YYYY', '\\d{4}'),
  time: (expectedValue: string) => expectedValue === 'hh:mm:ss A'
    ? '[0-2]\\d:[0-5]\\d [AP]M'
    : '[0-2]\\d:[0-5]\\d:[0-5]\\d',
}

Then(/^The "([^"]*)" should display the following custom formats:$/, async function(screenElement, expectedTable) {
  const expectedValues = expectedTable.hashes();
  for (let i = 0; i < expectedValues.length; i++) {
    const { param: expectedParam, value: expectedValue } = expectedValues[i];
    const selector = screenElementSelectors[screenElement][expectedParam];
    const tagName = await this.client.getTagName(selector);
    let currentValue;
    if (tagName === 'input') {
      currentValue = await this.client.getValue(selector);
    } else {
      currentValue = await this.client.getText(selector);
      if (Array.isArray(currentValue)) currentValue = currentValue[0];
    }
    const expectedMatcher = new RegExp(paramsMatchersValues[expectedParam](expectedValue));
    const matcher = expectedMatcher.test(currentValue)
    expect(matcher).to.be.true;
  }
});
