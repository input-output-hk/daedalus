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
