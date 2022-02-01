import { Given, When, Then } from "cucumber";
import { expect } from "chai";
import { chooseCustomOptionsByValue, getSelectedCustomOptions, getValueFromSelector, doesMatchExpectedValue } from "./helpers";

Given(/^I choose the following custom formats:$/, async function (formatsTable) {
  const chosenFormats = formatsTable.hashes();
  const [{
    value: numberValue
  }, {
    value: dateValue
  }, {
    value: timeValue
  }] = chosenFormats;
  await chooseCustomOptionsByValue.call(this, numberValue, dateValue, timeValue);
});
Given(/^I have changed the following custom formats:$/, async function (formatsTable) {
  const chosenFormats = formatsTable.hashes();
  await this.client.executeAsync((chosenFormats, done) => {
    Promise.all(chosenFormats.map(({
      param,
      value
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    }) => daedalus.stores.profile._updateUserLocalSetting({
      param: `${param}Format`,
      value
    }))).then(done);
  }, chosenFormats);
});
When(/^the "([^"]*)" wallet has received the transaction amount$/, async function (walletName) {
  await this.client.waitUntil(async () => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    const walletHasAmount = await this.client.execute(() => daedalus.stores.wallets.active && !daedalus.stores.wallets.active.amount.isZero());
    return walletHasAmount.value;
  });
});
Then(/^I should see the following chosen options:$/, async function (expectedTable) {
  const expectedValues = expectedTable.hashes();
  const [{
    value: expectedNumber
  }, {
    value: expectedDate
  }, {
    value: expectedTime
  }] = expectedValues;
  await this.client.waitUntil(async () => {
    const {
      selectedNumber,
      selectedDate,
      selectedTime
    } = await getSelectedCustomOptions.call(this);
    return selectedNumber === expectedNumber && selectedDate === expectedDate && selectedTime === expectedTime;
  });
  const {
    selectedNumber,
    selectedDate,
    selectedTime
  } = await getSelectedCustomOptions.call(this);
  expect(selectedNumber).to.equal(expectedNumber);
  expect(selectedDate).to.equal(expectedDate);
  expect(selectedTime).to.equal(expectedTime);
});
Then(/^the "([^"]*)" should display the following custom formats:$/, async function (screenElement, expectedTable) {
  const expectedValues = expectedTable.hashes();
  await this.client.waitUntil(async () => {
    const {
      param: expectedParam,
      value: expectedValue
    } = expectedValues[0];
    const matcher = await doesMatchExpectedValue.call(this, screenElement, expectedParam, expectedValue);
    return matcher;
  });

  for (let i = 0; i < expectedValues.length; i++) {
    const {
      param: expectedParam,
      value: expectedValue
    } = expectedValues[i];
    const matcher = await doesMatchExpectedValue.call(this, screenElement, expectedParam, expectedValue);
    expect(matcher).to.be.true;
  }
});
Then(/^the "([^"]*)" should display the "([^"]*)" of value "([^"]*)"$/, async function (screenElement, expectedParam, expectedValue) {
  const currentValue = await getValueFromSelector.call(this, screenElement, expectedParam);
  expect(currentValue).to.equal(expectedValue);
});