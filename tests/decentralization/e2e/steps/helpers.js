// @flow
import { waitAndClick } from '../../../common/e2e/steps/helpers';
import type { Daedalus, WebdriverClient } from '../../../types';

declare var daedalus: Daedalus;
const SELECTORS = {
  // ACTIVE_CATEGORY: '.SidebarCategory_active',
  // ADD_WALLET_BTN: '.SidebarWalletsMenu_addWalletButton',
};

export const getCardanoEpochData = async function(epoch: 'current' | 'next', param: string) {
  const headerIndex = epoch === 'current' ? 1 : 2;
  await this.client.getText(`(//div[@class="DelegationCenterHeader_heading"])[${headerIndex}]//following-sibling::div//div[text()="${param}"]//following-sibling::div[@class="DelegationCenterHeader_fieldValue"]`);
}
