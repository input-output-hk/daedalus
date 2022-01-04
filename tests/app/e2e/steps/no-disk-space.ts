import { Given, Then, When } from "cucumber";
import type { Daedalus } from "../../../types";

let diskSpaceRequired;
const HUNDRED_TB = 100 * 1e12; // 100 TB | unit: bytes

const NO_DISK_SPACE_COMPONENT = '.NoDiskSpaceError_component';
const ONE_KB = 1 * 1000; // 1 KB | unit: bytes

Given(/^I set the required space to 100 TB$/, () => {
  diskSpaceRequired = HUNDRED_TB;
  return true;
});
Given(/^I set the required space to 1 KB$/, () => {
  diskSpaceRequired = ONE_KB;
  return true;
});
When(/^I check the disk space$/, function () {
  this.client.execute(() => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.stores.networkStatus._checkDiskSpace();
  }, diskSpaceRequired);
});
Then(/^The No Disk Space overlay should be (hidden|visible)$/, function (state) {
  const waitForHidden = state === 'hidden';
  return this.client.waitForVisible(NO_DISK_SPACE_COMPONENT, null, waitForHidden);
});