import { waitAndClick } from './shared-helpers';

export default {
  activateCategory: async (client, { category }) => {
    await client.execute((cat) => {
      daedalus.actions.sidebar.activateSidebarCategory.trigger({
        category: cat,
        showSubMenu: true,
      });
    }, `/${category}`);
    return client.waitForVisible(`.SidebarCategory_active.${category}`);
  },
  clickAddWalletButton: (client) => (
    waitAndClick(client, '.SidebarWalletsMenu_addWalletButton')
  ),
};
