// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers, chooseCustomOptions, getSelectedCustomOptions } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I have chosen custom number, date and time formats$/, async function() {
  await chooseCustomOptions.call(this, 1, 1, 1);
});

Given(/^I have chosen new custom number, date and time formats$/, async function() {
  await chooseCustomOptions.call(this, 2, 2, 0);
});

Then (/^I should see the correct chosen options$/, async function() {
  const {
    selectedNumber,
    selectedDate,
    selectedTime,
  } = await getSelectedCustomOptions.call(this);
  const expectedNumber = 'number-2';
  const expectedDate = 'DD/MM/YYYY';
  const expectedTime = 'HH:mm:ss';
  expect(selectedNumber).to.equal(expectedNumber);
  expect(selectedDate).to.equal(expectedDate);
  expect(selectedTime).to.equal(expectedTime);
})

Then (/^I should see the correct new chosen options$/, async function() {
  const {
    selectedNumber,
    selectedDate,
    selectedTime,
  } = await getSelectedCustomOptions.call(this);
  const expectedNumber = 'number-3';
  const expectedDate = 'YYYY/MM/DD';
  const expectedTime = 'hh:mm:ss A';
  expect(selectedNumber).to.equal(expectedNumber);
  expect(selectedDate).to.equal(expectedDate);
  expect(selectedTime).to.equal(expectedTime);
})


