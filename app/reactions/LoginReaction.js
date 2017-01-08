// @flow
import { ipcRenderer } from 'electron';
import Reaction from './Reaction';
import environment from '../environment';

export default class LoginReaction extends Reaction {
  reaction() {
    const { user } = this.stores;
    if (user.isLoggedIn && user.active) {
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 1024, height: 768, animate: !environment.isTest() });
    }
    // TODO: environment is 'development' when tests start and than it changes to test
    // so this reaction fires in test mode without this timeout
    if (environment.isDev() && !user.isLoggedIn) {
      setTimeout(() => {
        if (environment.isDev() && environment.AUTO_LOGIN) {
          if (environment.WITH_CARDANO_API) {
            user._login({ email: '', passwordHash: '' });
          } else {
            user._login({
              email: 'satoshi@gmail.com',
              passwordHash: '5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8'
            });
          }
        }
      }, 1000);
    }
  }
}
