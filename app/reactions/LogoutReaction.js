// @flow
import { ipcRenderer } from 'electron';
import Reaction from './Reaction';
import environment from '../environment';

export default class LogoutReaction extends Reaction {
  reaction() {
    const { isLoggedIn } = this.stores.user;
    if (!isLoggedIn) {
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 480, height: 575, animate: !environment.isTest() });
    }
  }
}
