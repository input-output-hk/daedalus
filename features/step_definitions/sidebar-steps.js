import { Given, When, Then } from 'cucumber';
import sidebar from '../support/helpers/sidebar-helpers';

Given(/^the sidebar submenu is (hidden|visible)/, async function (state) {
  const isVisible = state === 'visible';
  await this.client.waitForVisible('.Sidebar_component');
  await this.client.executeAsync((visible, done) => {
    const { isShowingSubMenus } = daedalus.stores.sidebar;
    let sidebarWillAnimate = false;
    if (isShowingSubMenus !== visible) {
      sidebarWillAnimate = true;
      daedalus.actions.sidebar.toggleSubMenus.trigger();
    }
    if (sidebarWillAnimate) {
      // Wait until the sidebar transition is finished -> otherwise webdriver click error!
      const sidebarElement = document.querySelectorAll('.Sidebar_component')[0];
      const onTransitionFinished = () => {
        sidebarElement.removeEventListener('transitioned', onTransitionFinished);
        done();
      };
      sidebarElement.addEventListener('transitionend', onTransitionFinished);
    } else {
      done();
    }
  }, isVisible);
  return this.client.waitForExist('.SidebarMenu_visible', null, !isVisible);
});

Given(/^The sidebar shows the "([^"]*)" category$/, function (category) {
  return sidebar.activateCategory(this.client, { category });
});

When(/^I click on the sidebar toggle button$/, function () {
  return this.waitAndClick('.SidebarLayout_topbar .TopBar_leftIcon');
});

When(/^I click on the "([^"]*)" category in the sidebar$/, function (category) {
  return this.waitAndClick(`.SidebarCategory_component.${category}`);
});

When(/^I click on the add wallet button in the sidebar$/, function () {
  return sidebar.clickAddWalletButton(this.client);
});

When(/^I click on the "([^"]*)" wallet in the sidebar$/, function (walletName) {
  return this.waitAndClick(`//*[contains(text(), "${walletName}") and @class="SidebarWalletMenuItem_title"]`);
});

Then(/^the sidebar submenu should be (hidden|visible)/, function (state) {
  const waitForHidden = state === 'hidden';
  return this.client.waitForVisible('.SidebarMenu_component', null, waitForHidden);
});

Then(/^The "([^"]*)" category should be active$/, function (category) {
  return this.client.waitForVisible(`.SidebarCategory_active.${category}`);
});
