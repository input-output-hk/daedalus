import { expect } from 'chai';

export default function () {
  this.Given(/^the sidebar is (hidden|visible)/, async function (state) {
    const isHidden = state === 'hidden';
    await this.client.waitForVisible('.Sidebar_component');
    await this.client.executeAsync(function(hidden, done) {
      require('mobx').runInAction(() => {
        const sidebarState = daedalus.stores.sidebar;
        const sidebarWillAnimate = sidebarState.hidden !== hidden;
        let isDone = false;
        sidebarState.hidden = hidden;
        if (sidebarWillAnimate) {
          // Wait until the sidebar transition is finished -> otherwise webdriver click error!
          const sidebarElement = document.querySelectorAll('.Sidebar_component')[0];
          sidebarElement.addEventListener('transitionend', () => !isDone && done() && (isDone = true));
        } else {
          done();
        }
      });
    }, isHidden);
    return this.client.waitForExist(`.Sidebar_hidden`, null, !isHidden);
  });

  this.Given(/^The sidebar shows the (.*) category$/, async function (category) {
    await this.client.execute(function(route) {
      require('mobx').runInAction(() => daedalus.stores.sidebar.route = `/${route}`);
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

  this.Then(/^the sidebar should be (hidden|visible)/, function (state) {
    const isHidden = state === 'hidden';
    return this.client.waitForVisible(`.Sidebar_hidden`, null, !isHidden);
  });

  this.Then(/^The (.*) category should be active$/, function (category) {
    return this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });
}
