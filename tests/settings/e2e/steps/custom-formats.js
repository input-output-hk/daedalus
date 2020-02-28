// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers } from './helpers';
import {
  clickInputByLabel,
  clickOptionByIndex,
  getInputValueByLabel,
} from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I have chosen custom number, date and time formats$/, async function() {
  await clickInputByLabel.call(this, 'Number format');
  await clickOptionByIndex.call(this, 1);
  await clickInputByLabel.call(this, 'Date format');
  await clickOptionByIndex.call(this, 1);
  await clickInputByLabel.call(this, 'Time format');
  await clickOptionByIndex.call(this, 1);
});

Then (/^I should see the correct chosen options$/, async function() {
  const selectedNumber = await getInputValueByLabel.call(this, 'Number format')
  console.log('selectedNumber', selectedNumber);
  const selectedDate = await getInputValueByLabel.call(this, 'Date format')
  console.log('selectedDate', selectedDate);
  const selectedTime = await getInputValueByLabel.call(this, 'Time format')
  console.log('selectedTime', selectedTime);
  return true;
})



