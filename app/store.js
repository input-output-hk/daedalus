import { observable } from 'mobx';

export default observable({
  i18n: {
    locale: 'en-US',
  },
  wallet: null,
  router: null,
  sidebar: {
    hidden: false,
    showMenus: true
  }
});
