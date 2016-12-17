// @flow
import { ipcRenderer } from 'electron';
import BaseReaction from './BaseReaction';

export default class LoginReaction extends BaseReaction {
  reaction() {
    const { isLoggedIn, isLoggingIn } = this.state.login;
    if (isLoggedIn && !isLoggingIn) {
      this.appController.user.loadUser();
      this.appController.wallets.loadWallets();
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 1024, height: 768, animate: true });
    }
  }
}
