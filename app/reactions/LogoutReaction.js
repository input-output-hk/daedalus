// @flow
import { ipcRenderer } from 'electron';
import Reaction from './Reaction';
import environment from '../environment';

export default class LogoutReaction extends Reaction {
  reaction() {
    const { isLoggedIn, isLoggingIn } = this.state.login;
    if (!isLoggedIn || isLoggingIn) {
      // TODO: move window resizing to more appropriate place
      ipcRenderer.send('resize-window', { width: 480, height: 575, animate: !environment.isTest() });
    }
  }
}
