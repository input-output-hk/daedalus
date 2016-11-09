import { observable } from 'mobx';

export default observable({
  i18n: {
    locale: 'en-US',
  },
  wallet: null,
  router: null,
  sidebar: {
    route: '/wallets',
    hidden: false,
    showMenu: true
  }
});
