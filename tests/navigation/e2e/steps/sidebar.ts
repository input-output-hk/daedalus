import { Given, When, Then } from "cucumber";
import { sidebarHelpers } from "./helpers";

const SELECTORS = {
  CATEGORY_ACTIVE: '.SidebarCategory_active',
  CATEGORY_COMPONENT: '.SidebarCategory_component',
  LAYOUT_COMPONENT: '.SidebarLayout_component',
  MENU_COMPONENT: '.SidebarMenu_component',
  MENU_VISIBLE: '.SidebarMenu_visible',
  SIDEBAR_COMPONENT: '.Sidebar_component',
  TOP_BAR: '.SidebarLayout_topbar',
  TOP_BAR_LEFT_ICON: '.TopBar_leftIcon'
};
Given(/^the sidebar submenu is (hidden|visible)/, async function (state) {
  const isVisible = state === 'visible';
  await this.client.waitForVisible(SELECTORS.SIDEBAR_COMPONENT);
  await this.client.executeAsync((visible, SELECTORS, done) => {
    const {
      isShowingSubMenus
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus.stores.sidebar;
    let sidebarWillAnimate = false;

    if (isShowingSubMenus !== visible) {
      sidebarWillAnimate = true;
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.sidebar.toggleSubMenus.trigger();
    }

    if (sidebarWillAnimate) {
      // Wait until the sidebar transition is finished -> otherwise webdriver click error!
      const sidebarElement = document.querySelectorAll(SELECTORS.SIDEBAR_COMPONENT)[0];

      const onTransitionFinished = () => {
        sidebarElement.removeEventListener('transitioned', onTransitionFinished);
        done();
      };

      sidebarElement.addEventListener('transitionend', onTransitionFinished);
    } else {
      done();
    }
  }, isVisible, SELECTORS);
  return this.client.waitForExist(SELECTORS.MENU_VISIBLE, null, !isVisible);
});
Given(/^The sidebar shows the "([^"]*)" category$/, function (category) {
  return sidebarHelpers.activateCategory(this.client, {
    category
  });
});
When(/^I click on the sidebar toggle button$/, function () {
  return this.waitAndClick(`${SELECTORS.TOP_BAR} ${SELECTORS.TOP_BAR_LEFT_ICON}`);
});
When(/^I click on the "([^"]*)" category in the sidebar$/, function (category) {
  return this.waitAndClick(`${SELECTORS.CATEGORY_COMPONENT}.${category}`);
});
When(/^I click on the add wallet button in the sidebar$/, function () {
  return sidebarHelpers.clickAddWalletButton(this.client);
});
When(/^I click on the "([^"]*)" wallet in the sidebar$/, function (walletName) {
  return this.waitAndClick(`//*[text()="${walletName}" and @class="SidebarWalletMenuItem_title"]`);
});
Then(/^the sidebar submenu should be (hidden|visible)/, function (state) {
  const waitForHidden = state === 'hidden';
  return this.client.waitForVisible(SELECTORS.MENU_COMPONENT, null, waitForHidden);
});
Then(/^The "([^"]*)" category should be active$/, function (category) {
  return this.client.waitForVisible(`${SELECTORS.CATEGORY_ACTIVE}.${category}`);
});
Then(/^I should see the initial screen$/, function () {
  return this.client.waitForVisible(SELECTORS.LAYOUT_COMPONENT);
});