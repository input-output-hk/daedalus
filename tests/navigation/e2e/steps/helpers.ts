import { waitAndClick } from "../../../common/e2e/steps/helpers";

const SELECTORS = {
  ACTIVE_CATEGORY: '.SidebarCategory_active',
  ADD_WALLET_BTN: '.SidebarWalletsMenu_addWalletButton'
};
export const getCurrentAppRoute = async function () {
  const url = (await this.client.url()).value;
  return url.substring(url.indexOf('#/') + 1); // return without the hash
};
export const waitUntilUrlEquals = function (expectedUrl: string) {
  const context = this;
  return context.client.waitUntil(async () => {
    const url = await getCurrentAppRoute.call(context);
    return url === expectedUrl;
  });
};
export const navigateTo = function (requestedRoute: string) {
  return this.client.execute(route => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.router.goToRoute.trigger({
      route
    });
  }, requestedRoute);
};
export const sidebarHelpers = {
  activateCategory: async (client: Record<string, any>, {
    category
  }: {
    category: string;
  }) => {
    await client.execute(cat => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.sidebar.activateSidebarCategory.trigger({
        category: cat,
        showSubMenu: true
      });
    }, `/${category}`);
    return client.waitForVisible(`${SELECTORS.ACTIVE_CATEGORY}.${category}`);
  },
  clickAddWalletButton: (client: Record<string, any>) => waitAndClick.call({
    client
  }, SELECTORS.ADD_WALLET_BTN)
};