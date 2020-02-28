// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers, chooseCustomOptionsByValue, getSelectedCustomOptions } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;


Given(/^I have chosen the following custom formats:$/, async function(formatsTable) {
  const chosenFormats = formatsTable.hashes();
  const [
    { VALUE: numberValue },
    { VALUE: dateValue },
    { VALUE: timeValue },
  ] = chosenFormats;
  await chooseCustomOptionsByValue.call(this, numberValue, dateValue, timeValue);
});

Then (/^I should see the following chosen options:$/, async function(expectedTable) {
  const expectedValues = expectedTable.hashes();
  const [
    { VALUE: expectedNumber },
    { VALUE: expectedDate },
    { VALUE: expectedTime },
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
  }

}

const paramsMatchersValues = {
  date: (expectedValue: string) =>
    expectedValue
      .replace('MM', '(0[1-9]|1[0-2])')
      .replace('DD', '(0[1-9]|[12][0-9]|3[01])')
      .replace('YYYY', '[0-9]{4}'),
}

Then (/^The (\w+?)s? should display the following custom formats:$/, async function(screenElement, expectedTable) {
  const expectedValues = expectedTable.hashes();
  for (let i = 0; i < expectedValues.length; i++) {
    const { PARAM: expectedParam, VALUE: expectedValue } = expectedValues[i];
    const selector = screenElementSelectors[screenElement][expectedParam];
    const currentValue = await this.client.getText(selector);
    const expectedMatcher = new RegExp(paramsMatchersValues[expectedParam](expectedValue));
    const matcher = expectedMatcher.test(currentValue)
    expect(matcher).to.be.true;
  }
});
