// @flow
import { ipcRenderer } from 'electron';
import Reaction from './Reaction';
import environment from '../environment';

export default class LoginReaction extends Reaction {
  reaction() {
    const { user } = this.stores;
    if (user.isLoggedIn && user.active) {
      this.appController.wallets.loadWallets();
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 1024, height: 768, animate: !environment.isTest() });
    }
    // TODO: environment is 'development' when tests start and than it changes to test
    // so this reaction fires in test mode without this timeout
    if (environment.isDev() && !user.isLoggedIn && this.appController.api.repository.user.profile) {
      setTimeout(() => {
        if (environment.isDev() && environment.AUTO_LOGIN) {
          if (environment.WITH_CARDANO_API) {
            user._login({ email: '', passwordHash: '' });
          } else {
            const { email, passwordHash } = this.appController.api.repository.user.profile;
            user._login({ email, passwordHash });
          }
        }
      }, 1000);
    }
  }
}
