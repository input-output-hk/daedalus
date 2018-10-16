import { Given, When, Then } from 'cucumber';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';

Given(/^I haven't accepted the data layer migration$/, async function () {
  await this.client.execute(() => {
    daedalus.api.localStorage.unsetDataLayerMigrationAcceptance();
  });
});

Then(/^I should see the Data Layer Migration screen$/, function () {
  return this.client.waitForVisible(DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT);
});

Then(/^I should not see the Data Layer Migration screen$/, function () {
  return this.client.waitForVisible(DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT, null, true);
});

When(/^I click the migration button$/, function () {
  return this.waitAndClick(`${DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT} .DataLayerMigrationForm_submitButton`);
});

