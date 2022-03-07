import fs from "fs";
import path from "path";
import { When, Then, Given } from "cucumber";
import { expect } from "chai";
import { difference } from "lodash";
import { MAX_ADA_WALLETS_COUNT } from "../../../../source/renderer/app/config/numbersConfig";

const IMPORT_WALLETS_OVERLAY_SELECTOR = '.WalletImportFileDialog_overlay';
const IMPORT_WALLETS_OVERLAY_IMPORT_CHOICE_LABEL_SELECTOR = '.WalletImportFileDialog_stateFolderLabel';
const STATE_INPUT_FOLDER_SELECTOR = '.WalletImportFileDialog_stateFolderInput input';
const IMPORT_WALLETS_BUTTON_SELECTOR = '.WalletImportFileDialog_actionButton';
const WALLET_IMPORT_BIG_BUTTON_SELECTOR = '.WalletAdd .importWalletButton';
const NO_WALLETS_SELECTOR = '.WalletImportFileDialog_noWalletError';
const WALLET_SELECT_IMPORT_DIALOG_SELECTOR = '.WalletSelectImportDialog_component';
const NAMED_WALLET_INPUTS_SELECTOR = '.namedWalletsRow .SimpleInput_input';
const IMPORT_CHOICE_SELECTOR = '.RadioSet_radiosContainer.RadioSet_verticallyAligned .SimpleRadio_label';
const UNNAMED_WALLET_INPUTS_SELECTOR = '.unnamedWalletsRow .SimpleInput_input';
const WALLET_CHECKBOXES_SELECTOR = '.WalletSelectImportDialog_walletsRow .SimpleCheckbox_check';
const IS_CHECKBOX_CHECKDE_SELECTOR = 'SimpleCheckbox_checked';
const IS_CHECKBOX_DISABLED_SELECTOR = 'SimpleCheckbox_disabled';
const WALLET_SELECT_IMPORT_ACTION_BUTTON_SELECTOR = '.WalletSelectImportDialog_actionButton';
const WALLET_SELECT_IMPORT_ACTION_BUTTON_SPINNER_SELECTOR = '.WalletSelectImportDialog_actionButton .LoadingSpinner_component';
const WALLET_SELECT_IMPORT_ACTION_BUTTON_DISABLED_SELECTOR = '.WalletSelectImportDialog_actionButton.WalletSelectImportDialog_disabled';
const STATUS_ICON_CHECKMARK_SELECTOR = '.WalletSelectImportDialog_walletsStatusIconCheckmark';
const WALLET_STATUS_LABEL_SELECTOR = '.WalletSelectImportDialog_walletsStatus';
const WALLET_SELECT_IMPORT_DIALOG_CLOSE_BUTTON_SELECTOR = '.WalletSelectImportDialog_closeButton';
const WALLET_SELECT_IMPORT_DIALOG_CLOSE_LINK_SELECTOR = '.WalletSelectImportDialog_closeWindowLink';
const SIDEBAR_WALLETS_TITLE_SELECTOR = '.SidebarWalletMenuItem_title';
const NAMED_WALLET_ROW_SELECTOR = '.namedWalletsRow';
const NAMED_WALLET_CHECKBOXES_SELECTOR = '.namedWalletsRow .SimpleCheckbox_root';
const TOOLTIP_SELECTOR = '.WalletSelectImportDialog_maxWalletsReachedTooltip .SimpleBubble_root .SimpleBubble_bubble';
Given(/^I have wallet migration enabled/, async function () {
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  await this.client.execute(() => daedalus.stores.walletMigration._enableTestWalletMigration());
});
Given(/^I set (wrong )?import path$/, async function (isWrong) {
  const importDir = isWrong ? '../documents/import-files-wrong' : '../documents/import-files';
  const importPath = path.resolve(__dirname, importDir);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  await this.client.execute(path => daedalus.stores.walletMigration._setFakedImportPath(path), importPath);
});
Given(/^I set (wrong )?import secret key path$/, async function (isWrong) {
  const importDir = isWrong ? '../documents/import-files/Secrets-1.0/wrong-secret.key' : '../documents/import-files/Secrets-1.0/secret.key';
  const importPath = path.resolve(__dirname, importDir);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  await this.client.execute(path => daedalus.stores.walletMigration._setFakedImportPath(path), importPath);
});
When(/^I click on the import wallet button on the add wallet page/, async function () {
  this.waitAndClick(WALLET_IMPORT_BIG_BUTTON_SELECTOR);
});
When(/^I see the import wallets overlay$/, function () {
  return this.client.waitForVisible(IMPORT_WALLETS_OVERLAY_IMPORT_CHOICE_LABEL_SELECTOR);
});
When(/^I should see import selection label:$/, async function (dataTable) {
  const selectedImportOption = await this.waitAndGetText(IMPORT_WALLETS_OVERLAY_IMPORT_CHOICE_LABEL_SELECTOR);
  const labelId = dataTable.hashes()[0].label;
  const expectedLabel = await this.intl(labelId);
  expect(selectedImportOption).to.equal(expectedLabel);
});
When(/^I should see import file selection error message:$/, async function (dataTable) {
  const errorOnScreen = await this.waitAndGetText(NO_WALLETS_SELECTOR);
  const errorId = dataTable.hashes()[0].message;
  const expectedError = await this.intl(errorId);
  expect(errorOnScreen).to.equal(expectedError);
});
When(/^I should see Daedalus State directory as predefined import path$/, async function () {
  await this.client.waitForVisible(STATE_INPUT_FOLDER_SELECTOR);
  const selectedDirectory = await this.client.getValue(STATE_INPUT_FOLDER_SELECTOR);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  const predefinedStateDirectory = await this.client.execute(() => daedalus.stores.walletMigration.defaultExportSourcePath);
  expect(selectedDirectory).to.equal(predefinedStateDirectory.value);
});
When(/^I click "Import wallets" button$/, function () {
  this.waitAndClick(IMPORT_WALLETS_BUTTON_SELECTOR);
});
When(/^I click "Import selected wallets" button and wait until operation is finished$/, async function () {
  await this.waitAndClick(WALLET_SELECT_IMPORT_ACTION_BUTTON_SELECTOR);
  await this.client.waitForExist(WALLET_SELECT_IMPORT_ACTION_BUTTON_SPINNER_SELECTOR, 10000, true);
});
When(/^I should see wallet select import dialog$/, async function () {
  await this.client.waitForVisible(WALLET_SELECT_IMPORT_DIALOG_SELECTOR);
});
When(/^I should not see wallet select import dialog$/, async function () {
  return this.client.waitForVisible(WALLET_SELECT_IMPORT_DIALOG_SELECTOR, null, true);
});
When(/^I edit first wallet name$/, async function () {
  const namedWalletElements = await this.client.elements(NAMED_WALLET_INPUTS_SELECTOR);
  const firstNamedWallet = namedWalletElements.value[0].ELEMENT;
  await this.client.elementIdClick(firstNamedWallet);
  this.client.elementIdValue(firstNamedWallet, ' EDITED');
});
When(/^I select import from secret key$/, async function () {
  const importChoices = await this.client.elements(IMPORT_CHOICE_SELECTOR);
  const importFromKey = importChoices.value[1].ELEMENT;
  await this.client.elementIdClick(importFromKey);
});
When(/^I should see wallets properly listed$/, async function () {
  await this.client.waitForVisible(WALLET_SELECT_IMPORT_DIALOG_SELECTOR);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
  const exportedWallets = await this.client.execute(() => daedalus.stores.walletMigration.exportedWallets);
  this.exportedWallets = exportedWallets.value;
  const unnamedWallets = exportedWallets.value.filter(wallet => !wallet.name);
  const namedWalletsCount = Math.abs(exportedWallets.value.length - unnamedWallets.length);
  const unnamedWalletsCount = unnamedWallets.length;
  const namedWalletElements = await this.client.elements(NAMED_WALLET_INPUTS_SELECTOR);
  const unnamedWalletElements = await this.client.elements(UNNAMED_WALLET_INPUTS_SELECTOR);
  // All named wallets are listed
  expect(namedWalletsCount).to.equal(namedWalletElements.value.length);
  // All unnamed wallets are listed
  expect(unnamedWalletsCount).to.equal(unnamedWalletElements.value.length);

  // Named wallet inputs have value
  for (let i = 0; i < namedWalletElements.value.length; i++) {
    const namedWalletInputElement = namedWalletElements.value[i].ELEMENT;
    const namedWalletInputElementValue = await this.client.elementIdAttribute(namedWalletInputElement, 'value');
    expect(namedWalletInputElementValue.value).to.not.be.empty;
  }

  // Unnamed wallet inputs have NO value
  for (let i = 0; i < unnamedWalletElements.value.length; i++) {
    const unnamedWalletInputElement = unnamedWalletElements.value[i].ELEMENT;
    const unnamedWalletInputElementValue = await this.client.elementIdAttribute(unnamedWalletInputElement, 'value');
    expect(unnamedWalletInputElementValue.value).to.be.empty;
  }
});
When(/^I should see that all wallets are available for import$/, async function () {
  await this.client.waitForVisible(WALLET_SELECT_IMPORT_DIALOG_SELECTOR);
  const listedWalletElements = await this.client.elements(WALLET_CHECKBOXES_SELECTOR);

  for (let i = 0; i < this.exportedWallets.length; i++) {
    const listedWalletElement = listedWalletElements.value[i].ELEMENT;
    const checkboxClasses = await this.client.elementIdAttribute(listedWalletElement, 'class');
    const isChecked = checkboxClasses.value.indexOf(IS_CHECKBOX_CHECKDE_SELECTOR) >= 0;
    expect(isChecked).to.equal(false); // Checkbox presented and not checked
  }
});
When(/^I select all (named|unnamed) wallets for import$/, async function (_type) {
  const selector = `.${_type}WalletsRow .SimpleCheckbox_root`;
  const walletCheckboxes = await this.client.elements(selector);
  const selectedNamedWallets = this.exportedWallets.filter(wallet => wallet.hasName);

  for (let i = 0; i < walletCheckboxes.value.length; i++) {
    const walletCheckbox = walletCheckboxes.value[i].ELEMENT;
    await this.client.elementIdClick(walletCheckbox);
  }

  const walletNames = [];
  const walletInputElements = await this.client.elements(`.${_type}WalletsRow .SimpleInput_input`);

  for (let i = 0; i < walletInputElements.value.length; i++) {
    const walletInputElement = walletInputElements.value[i].ELEMENT;
    const walletName = await this.client.elementIdAttribute(walletInputElement, 'value');
    walletNames.push(walletName.value);
  }

  if (!this.selectedWallets) {
    this.selectedWallets = walletNames;
  } else {
    walletNames.map(walletName => {
      this.selectedWallets.push(walletName);
    });
  }
});
When('I select wallet with index {int} for import', async function (walletIndex) {
  const walletCheckboxes = await this.client.elements(NAMED_WALLET_CHECKBOXES_SELECTOR);
  const walletCheckbox = walletCheckboxes.value[walletIndex].ELEMENT;
  await this.client.elementIdClick(walletCheckbox);
});
When('I hover import selection checkbox for wallet with index {int}', async function (walletIndex) {
  const walletCheckboxes = await this.client.elements(NAMED_WALLET_CHECKBOXES_SELECTOR);
  const walletCheckbox = walletCheckboxes.value[walletIndex].ELEMENT;
  await this.client.elementIdClick(walletCheckbox);
});
Then('Import selection checkbox for wallet with index {int} is disabled', async function (walletIndex) {
  const walletCheckboxes = await this.client.elements(NAMED_WALLET_CHECKBOXES_SELECTOR);
  const walletCheckbox = walletCheckboxes.value[walletIndex].ELEMENT;
  const checkboxClasses = await this.client.elementIdAttribute(walletCheckbox, 'class');
  const isDisabled = checkboxClasses.value.indexOf(IS_CHECKBOX_DISABLED_SELECTOR) >= 0;
  expect(isDisabled).to.equal(true); // Checkbox presented and disabled
});
When('I should see maximum wallets reached tooltip for wallet with index {int}', async function (walletIndex) {
  const walletRows = await this.client.elements(NAMED_WALLET_ROW_SELECTOR);
  const walletRow = walletRows.value[walletIndex].ELEMENT;
  const walletTooltip = await this.client.elementIdElement(walletRow, TOOLTIP_SELECTOR);
  const tooltipText = await this.client.elementIdText(walletTooltip.value.ELEMENT);
  const expectedTooltipText = await this.intl('wallet.select.import.dialog.maxWalletsReachedTooltip', {
    maxWalletsCount: MAX_ADA_WALLETS_COUNT
  });
  expect(tooltipText.value).to.equal(expectedTooltipText); // Checkbox presented and disabled
});
When(/^"Import selected wallets" button is (enabled|disabled)$/, async function (state) {
  if (state === 'enabled') {
    await this.client.waitForEnabled(WALLET_SELECT_IMPORT_ACTION_BUTTON_SELECTOR);
  } else {
    await this.client.waitForVisible(WALLET_SELECT_IMPORT_ACTION_BUTTON_DISABLED_SELECTOR);
  }
});
When(/^I enter random names to all unnamed wallets$/, async function () {
  const listedWalletElements = await this.client.elements(UNNAMED_WALLET_INPUTS_SELECTOR);

  for (let i = 0; i < listedWalletElements.value.length; i++) {
    const walletName = `Unnamed Wallet ${i}`;
    const walletInputField = listedWalletElements.value[i].ELEMENT;
    await this.client.elementIdClick(walletInputField);
    await this.client.elementIdValue(walletInputField, walletName);
  }

  // Click outside to apply last entered values
  await this.waitAndClick('.WalletSelectImportDialog_title');
});
When(/^I close import wallets dialog by clicking on "([^"]*)" button$/, function (closeAction) {
  if (closeAction === 'Close window') {
    // Close link at the bottom
    this.waitAndClick(WALLET_SELECT_IMPORT_DIALOG_CLOSE_LINK_SELECTOR);
  } else {
    // Close (X) button in top right corner
    this.waitAndClick(WALLET_SELECT_IMPORT_DIALOG_CLOSE_BUTTON_SELECTOR);
  }
});
Then(/^I should see that all (named|unnamed) wallets are imported$/, async function (state) {
  const walletRows = await this.client.elements(`.${state}WalletsRow`);

  for (let i = 0; i < walletRows.value.length; i++) {
    const walletRow = walletRows.value[i].ELEMENT;
    // Check if imported wallet checkeckmark checked
    const checkmarkElement = await this.client.elementIdElement(walletRow, STATUS_ICON_CHECKMARK_SELECTOR);
    expect(checkmarkElement.value).to.not.be.empty; // Checkmark checked

    // Check if "Wallet imported" label is presented
    const statusLabel = await this.client.elementIdElement(walletRow, WALLET_STATUS_LABEL_SELECTOR);
    const statusLabelText = await this.client.elementIdText(statusLabel.value.ELEMENT);
    const expectedLabel = await this.intl('wallet.select.import.dialog.walletImported');
    expect(statusLabelText.value).to.equal(expectedLabel); // Wallet imported label
  }
});
Then(/^I should see that all (named|unnamed) already exists$/, async function (state) {
  const walletRows = await this.client.elements(`.${state}WalletsRow`);

  for (let i = 0; i < walletRows.value.length; i++) {
    const walletRow = walletRows.value[i].ELEMENT;
    // Check if imported wallet checkeckmark checked
    const checkmarkElement = await this.client.elementIdElement(walletRow, STATUS_ICON_CHECKMARK_SELECTOR);
    expect(checkmarkElement.value).to.not.be.empty; // Checkmark checked

    // Check if "Wallet already exist" label is presented
    const statusLabel = await this.client.elementIdElement(walletRow, WALLET_STATUS_LABEL_SELECTOR);
    const statusLabelText = await this.client.elementIdText(statusLabel.value.ELEMENT);
    const expectedLabel = await this.intl('wallet.select.import.dialog.walletExists');
    expect(statusLabelText.value).to.equal(expectedLabel); // Wallet imported label
  }
});
Then(/^I should not see the import wallet dialog anymore$/, function () {
  return this.client.waitForVisible(IMPORT_WALLETS_OVERLAY_IMPORT_CHOICE_LABEL_SELECTOR, null, true);
});
Then(/^I should see all imported wallets in left sidebar$/, async function () {
  const expectedWallets = await this.waitAndGetText(SIDEBAR_WALLETS_TITLE_SELECTOR);
  await this.client.waitUntil(async () => {
    const walletsMatch = difference(expectedWallets, this.selectedWallets).length === 0;
    return walletsMatch;
  });
});