export default function () {
  this.Given(/^the sidebar submenu is (hidden|visible)/, async function (state) {
    const isVisible = state === 'visible';
    await this.client.waitForVisible('.Sidebar_component');
    await this.client.executeAsync(function(visible, done) {
      const { isShowingSubMenus } = daedalus.stores.sidebar;
      let sidebarWillAnimate = false;
      if (isShowingSubMenus !== visible) {
        sidebarWillAnimate = true;
        daedalus.actions.sidebar.toggleSubMenus();
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
    return this.client.waitForExist(`.SidebarMenu_visible`, null, !isVisible);
  });

  this.Given(/^The sidebar shows the "([^"]*)" category$/, async function (cat) {
    await this.client.execute(function(category) {
      daedalus.actions.sidebar.activateSidebarCategory({ category, showSubMenus: true });
    }, `/${cat}`);
    return this.client.waitForVisible(`.SidebarCategory_active.${cat}`);
  });

  this.When(/^I click on the sidebar toggle button$/, function () {
    return this.waitAndClick('.SidebarLayout_topbar .app-bar_leftIcon');
  });

  this.When(/^I click on the "([^"]*)" category in the sidebar$/, function (category) {
    return this.waitAndClick(`.SidebarCategory_component.${category}`);
  });

  this.When(/^I click on the add wallet button in the sidebar$/, function () {
    return this.waitAndClick('.SidebarWalletsMenu_addWalletButton');
  });

  this.When(/^I click on the "([^"]*)" wallet in the sidebar$/, function (walletName) {
    return this.waitAndClick(`//*[contains(text(), "${walletName}") and @class="SidebarWalletMenuItem_title"]`);
  });

  this.Then(/^the sidebar submenu should be (hidden|visible)/, function (state) {
    const waitForHidden = state === 'hidden';
    return this.client.waitForVisible(`.SidebarMenu_component`, null, waitForHidden);
  });

  this.Then(/^The "([^"]*)" category should be active$/, function (category) {
    return this.client.waitForVisible(`.SidebarCategory_active.${category}`);
  });
}
