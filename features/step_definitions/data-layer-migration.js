import { Given, When, Then } from 'cucumber';

const DATA_LAYER_MIGRATION_COMPONENT = '.DataLayerMigrationForm_component';

Given(/^I haven't clicked the migration button$/, async function () {
  await this.client.execute(() => {
    daedalus.api.localStorage.reset();
  });
});

Then(/^I should see the Data Layer Migration screen$/, function () {
  return this.client.waitForVisible(DATA_LAYER_MIGRATION_COMPONENT);
});

Then(/^I should not see the Data Layer Migration screen$/, function () {
  return this.client.waitForVisible(DATA_LAYER_MIGRATION_COMPONENT, null, true);
});

When(/^I click the migration button$/, function () {
  return this.waitAndClick(`${DATA_LAYER_MIGRATION_COMPONENT} .DataLayerMigrationForm_submitButton`);
});

When(/^I refresh the application$/, function () {
  this.client.keys(['command', 'r']);
  this.client.keys(['control', 'r']);
  this.client.keys(['super', 'r']);
});

Then(/^I should go to the initial screen$/, function () {
  return this.client.waitForVisible('.SidebarLayout_component');
});

When('I delete all wallets', async function () {
  await this.client.executeAsync(() => {
    daedalus.stores.sidebar.wallets.forEach(({ id: walletId }) =>
      daedalus.actions.ada.wallets.deleteWallet.trigger({ walletId })
    );
  });
});
