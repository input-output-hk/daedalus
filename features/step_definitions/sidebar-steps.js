import { expect } from 'chai';

export default function () {
  this.Given(/^the sidebar submenu is (hidden|visible)/, async function (state) {
    const isHidden = state === 'hidden';
    await this.client.waitForVisible('.Sidebar_component');
    await this.client.executeAsync(function(hidden, done) {
      const sidebarState = daedalus.stores.sidebar;
      if (sidebarState.hidden !== hidden) {
        daedalus.actions.sidebar.toggleSubMenus();
      }
      done();
    }, isHidden);
    return this.client.waitForExist(`.Sidebar_hidden`, null, !isHidden);
  });

  this.Given(/^The sidebar shows the "([^"]*)" category$/, async function (category) {
    await this.client.execute(function(cat) {
      daedalus.stores.sidebar.showCategoryWithSubMenus(`/${cat}`);
    }, category);
    return this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });

  this.When(/^I click on the sidebar toggle button$/, function () {
    return this.client.click('.SidebarLayout_appbar .app-bar_leftIcon');
  });

  this.When(/^I click on the (.*) category in the sidebar$/, function (category) {
    return this.client.click(`.SidebarCategory_component.${category}`);
  });

  this.When(/^I click on the add wallet button in the sidebar$/, function () {
    return this.client.click('.SidebarWalletsMenu_addWalletButton');
  });

  this.Then(/^the sidebar submenu should be (hidden|visible)/, function (state) {
    const waitForHidden = state === 'hidden';
    return this.client.waitForVisible(`.SidebarMenu_component`, null, waitForHidden);
  });

  this.Then(/^The (.*) category should be active$/, function (category) {
    return this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });
}
