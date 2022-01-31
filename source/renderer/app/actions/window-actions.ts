import Action from './lib/Action';

export default class WindowActions {
  resizeWindow: Action<{
    width: number;
    height: number;
  }> = new Action();
  closeWindow: Action<{}> = new Action();
}
