import { When, Then } from 'cucumber';

const CUSTOM_SERVER_DROPDOWN_OPTION = '//*[@class="SimpleInput_customValueWrapper"]//*[@value="Custom server"]';
const DAEDALUS_TOP_BAR_LOGO = '//*[@class="TopBar_topBar TopBar_withoutWallet"]';
const NOT_A_VALID_SMASH_SERVER_ERROR_MESSAGE_ACTIVATED = '//*[@class="InlineEditingInput_errorMessage" and text()="This URL is not a valid SMASH server"]';
const OFF_CHAIN_METADATA_SERVER_SMASH_LABEL = '//*[contains(text(), "Off-chain metadata server (SMASH)")]';
const SERVER_URL = "https://smash.cardano-testnet.iohkdev.io/";
const SMASH_SERVER_URL_INPUT_BOX = '//*[@label="SMASH server URL"]';
const STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_SUBMIT_BUTTON = '//*[@class="InlineEditingInput_button InlineEditingInput_okButton SimpleButton_root ButtonOverrides_root"]';
const STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_X_BUTTON = '//*[@class="InlineEditingInput_button InlineEditingInput_cancelButton SimpleButton_root ButtonOverrides_root"]';
const STAKE_POOL_SERVER_DROPDOWN = '//*[@class="SimpleFormField_inputWrapper"]';
const STAKE_POOL_SERVER_DROPDOWN_CUSTOM_OPTION = '//*[@class="ScrollbarsCustom-Content"]//span[text()="Custom server"]';
const STAKE_POOLS_SUBMENU_SETTINGS = '//*[@class="SettingsMenu_component"]//button[text()="Stake pools"]';

When(/^custom server is the default option$/, function() {
  return this.waitAndClick(CUSTOM_SERVER_DROPDOWN_OPTION);
});

Then(/^The custom server I chose earlier is visible on stake-pool screen above stake-pool list and is clickable$/, function() {
  return this.waitAndClick(`//*[@class="StakePools_smashSettings"]//span[text()="Moderated by ${SERVER_URL}"]`);
});

Then(/^I am brought back to the stake-pool server settings screen$/, function() {
  return this.client.waitForVisible(OFF_CHAIN_METADATA_SERVER_SMASH_LABEL);
});

When(/^I select custom server option$/, function() {
  return this.waitAndClick(STAKE_POOL_SERVER_DROPDOWN_CUSTOM_OPTION);
});

When(/^I open stake pool server dropdown$/, function() {
  return this.waitAndClick(STAKE_POOL_SERVER_DROPDOWN);
});

Then(/^The smash server input textBox is visible$/, function() {
  return this.client.waitForVisible(SMASH_SERVER_URL_INPUT_BOX);
});

When(/^And I enter a custom server URL as custom server option$/, function() {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, SERVER_URL);
});

When(/^I enter invalid url "([^"]*)" in to the custom server input-box$/, function(invalidUrl) {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, invalidUrl);
});

Then(/^Stake-pool custom input box error message "([^"]*)" is displayed$/, function(message) {
  return this.client.waitForVisible(`//div[contains(text(), "${message}")]`);
});

When(/^I enter invalid server "([^"]*)" containing https$/, function(value) {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, value);
});

When(/^I delete values in smash server custom url input box$/, function() {
  return this.waitAndClick(STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_X_BUTTON);
});

When(/^I focus the stake-pool custom server input box submit button and then click it$/, function() {
  // This step is necessary as otherwise the error message receives the click
  this.client.waitAndClick(DAEDALUS_TOP_BAR_LOGO);
  return this.waitAndClick(STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_SUBMIT_BUTTON);
});

Then(/^I see the "This url is not a valid SMASH server" error message displayed$/, function() {
  return this.client.waitForVisible(NOT_A_VALID_SMASH_SERVER_ERROR_MESSAGE_ACTIVATED);
});

When(/^I click on stakepool subtab$/, function() {
  return this.waitAndClick(STAKE_POOLS_SUBMENU_SETTINGS);
});

Then(/^I see the "([^"]*)" success message$/, function(message) {
  return this.client.waitForVisible(`//*[@class="StakePoolsSettings_savingResultLabel" and text()="${message}"]`);
});
