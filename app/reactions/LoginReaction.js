// @flow
import { ipcRenderer } from 'electron';
import Reaction from './Reaction';
import environment from '../environment';

export default class LoginReaction extends Reaction {
  reaction() {
    const { isLoggedIn, isLoggingIn } = this.state.login;
    if (isLoggedIn && !isLoggingIn) {
      this.appController.user.loadUser();
      this.appController.wallets.loadWallets();
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 1024, height: 768, animate: !environment.isTest() });
    }
    // TODO: environment is 'development' when tests start and than it changes to test
    // so this reaction fires in test mode without this timeout
    if (environment.isDev() && !isLoggedIn && !isLoggingIn) {
      setTimeout(() => {
        if (environment.isDev()) {
          if (environment.WITH_CARDANO_API) {
            this.appController.user.login({ email: '', passwordHash: '' });
          } else {
            const { email, passwordHash } = this.appController.api.repository.user.profile;
            this.appController.user.login({ email, passwordHash });
          }
        }
      }, 1000);
    }
  }
}
