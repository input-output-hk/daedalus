// @flow
import { Given, When, Then } from 'cucumber';
import { expect } from 'chai';
import { termsOfUseHelpers } from './helpers';
import type { Daedalus } from '../../../types';

declare var daedalus: Daedalus;

Given(/^I have chosen custom number, date and time formats$/, async function() {
  await this.client.click('.DisplaySettings_component button:nth-child(2)');
});



