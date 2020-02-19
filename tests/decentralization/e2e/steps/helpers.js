// @flow
import { waitAndClick } from '../../../common/e2e/steps/helpers';
import type { Daedalus, WebdriverClient } from '../../../types';

declare var daedalus: Daedalus;

export const getCardanoEpochData = async function(epoch: 'current' | 'next', param: string) {
  const headerIndex = epoch === 'current' ? 1 : 2;
  return await this.client.getText(`(//div[@class="DelegationCenterHeader_heading"])[${headerIndex}]//following-sibling::div//div[text()="${param}"]//following-sibling::div[@class="DelegationCenterHeader_fieldValue"]`);
};

export const getCurrentEpoch = async function() {
  return await getCardanoEpochData.call(this, 'current', 'Epoch');
};

export const getNextEpoch = async function() {
  const headerText = await this.client.getText(`(//div[@class="DelegationCenterHeader_heading"])[2]`);
  try {
    return headerText.match(/[1-9]+/)[0];
  } catch(err) {
    return new Error(err);
  }
};
