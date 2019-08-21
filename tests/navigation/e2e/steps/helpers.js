import { waitAndClick } from './shared-helpers';

export const getCurrentAppRoute = async function() {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};

export const waitUntilUrlEquals = function(expectedUrl) {
  const context = this;
  return context.client.waitUntil(async () => {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};

export const navigateTo = function(requestedRoute) {
  return this.client.execute(route => {
    daedalus.actions.router.goToRoute.trigger({ route });
  }, requestedRoute);
};

export const sidebarHelpers =  {
  activateCategory: async (client, { category }) => {
    await client.execute(cat => {
      daedalus.actions.sidebar.activateSidebarCategory.trigger({
        category: cat,
        showSubMenu: true,
      });
    }, `/${category}`);
    return client.waitForVisible(`.SidebarCategory_active.${category}`);
  },
  clickAddWalletButton: client =>
    waitAndClick(client, '.SidebarWalletsMenu_addWalletButton'),
};