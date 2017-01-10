// @flow
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import environment from '../environment';

export default class WindowStore extends Store {

  constructor(...args) {
    super(...args);
    const { resizeWindow } = this.actions;
    this.mapActions([
      { action: resizeWindow, listener: this._resizeWindow },
    ]);
  }

  // PRIVATE

  _resizeWindow = ({ width, height }) => {
    ipcRenderer.send('resize-window', { width, height, animate: !environment.isTest() });
  };

}
