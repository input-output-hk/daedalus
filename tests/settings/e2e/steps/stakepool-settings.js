import { When, Then,} from 'cucumber';

const CUSTOM_SERVER_DROPDOWN_OPTION = '//*[@class="SimpleInput_customValueWrapper"]//*[@value="Custom server"]';
const DAEDALUS_TOP_BAR_LOGO = '//*[@class="TopBar_topBar TopBar_withoutWallet"]';
const IS_ERROR_MESSAGE_ACTIVATED = '//*[@class="SimpleInput_input SimpleInput_errored"]';
const NONE_FETCH_DATA_DIRECTLY_OPTION = '//*[@class="SimpleInput_customValueWrapper"]//*[@value="None - fetch the data directly"]';
const NOT_A_VALID_SMASH_SERVER_ERROR_MESSAGE_ACTIVATED = '//*[@class="InlineEditingInput_errorMessage" and text()="This URL is not a valid SMASH server"]';
const OFF_CHAIN_METADATA_SERVER_SMASH_LABEL = '//*[contains(text(), "Off-chain metadata server (SMASH)")]';
const STAKE_POOL_SERVER_DROPDOWN = '//*[@class="SimpleFormField_inputWrapper"]';
const STAKE_POOL_SERVER_DROPDOWN_CUSTOM_OPTION = '//*[@class="ScrollbarsCustom-Content"]//span[text()="Custom server"]';
const SMASH_SERVER_URL_INPUT_BOX = '//*[@label="SMASH server URL"]';
const STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_SUBMIT_BUTTON = '//*[@class="InlineEditingInput_button InlineEditingInput_okButton SimpleButton_root ButtonOverrides_root"]';
const STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_X_BUTTON = '//*[@class="InlineEditingInput_button InlineEditingInput_cancelButton SimpleButton_root ButtonOverrides_root"]';
const STAKE_POOLS_SUBMENU_SETTINGS = '//*[@class="SettingsMenu_component"]//button[text()="Stake pools"]';
const CHANGES_SAVED_SUCCESS_MESSAGE = '//*[@class="StakePoolsSettings_savingResultLabel"]';

When(/^none fetch the data directly is the default option$/, function() {
  return this.waitAndClick(NONE_FETCH_DATA_DIRECTLY_OPTION);
});

When(/^custom server is the default option$/, function() {
  return this.waitAndClick(CUSTOM_SERVER_DROPDOWN_OPTION);
});

When(/^I click on Daedalus logo to change focus$/, function() {
  // This step was necessary as sometimes if the focus is on text in the textbox the submit button could not recieve the click and i got a "Element is not clickable at point error"
  return this.waitAndClick(DAEDALUS_TOP_BAR_LOGO);
});

Then(/^"([^"]*)" is visible on stakepool screen above stake-pool list and is clickable$/, function(serverUrl) {
  return this.waitAndClick('//*[@class="StakePools_smashSettings"]//span[text()="Moderated by '+ serverUrl + '"]');

  // old element const MODERATED_BY_IOHK_DEVIO_HEADER_LINK = '//*[@class="StakePools_smashSettings"]//span[text()="Moderated by https://smash.cardano-testnet.iohkdev.io/"]';
  // return this.client.waitForVisible(`//*[@class="WalletSummaryCurrency_currencyRate" and text()="${ticker}"]`);
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
  const millisecondsToWait = 25000;
  setTimeout(function() {
  }, millisecondsToWait);

  return this.client.waitForVisible(SMASH_SERVER_URL_INPUT_BOX);
});

When(/^I enter custom server "([^"]*)" as the custom server option$/, function(customServer) {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, customServer);
});

When(/^I enter invalid url "([^"]*)" without https$/, function(invalidUrl) {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, invalidUrl);
});

Then(/^Stake-pool custom input box error message is displayed$/, function() {
  return this.client.waitForVisible(IS_ERROR_MESSAGE_ACTIVATED);
});

When(/^I enter invalid server "([^"]*)" containing https$/, function(value) {
  this.client.setValue(SMASH_SERVER_URL_INPUT_BOX, value);
});

When(/^I delete values in smash server custom url input box$/, function() {
  return this.waitAndClick(STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_X_BUTTON);
});

When(/^I click the stake-pool custom server inputbox submit button$/, function() {
  return this.waitAndClick(STAKE_POOL_CUSTOM_SERVER_INPUT_BOX_SUBMIT_BUTTON);
});

Then(/^I see the "This url is not a valid SMASH server" error message displayed$/, function() {
  return this.client.waitForVisible(NOT_A_VALID_SMASH_SERVER_ERROR_MESSAGE_ACTIVATED);
});

When(/^I click on stakepool subtab$/, function() {
  return this.waitAndClick(STAKE_POOLS_SUBMENU_SETTINGS);
});

Then(/^I see the your changes have been saved success message$/, function() {
  return this.client.waitForVisible(CHANGES_SAVED_SUCCESS_MESSAGE);
});
