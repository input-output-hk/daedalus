// @flow
import BaseReaction from './BaseReaction';

export default class LoginReaction extends BaseReaction {
  reaction() {
    const { isLoggedIn, isLoggingIn } = this.state.login;
    if (isLoggedIn && !isLoggingIn) {
      this.appController.user.loadUser();
      this.appController.wallets.loadWallets();
    }
  }
}
