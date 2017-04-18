// @flow
import { Action } from './lib/actions';

export default class WindowActions {
  resizeWindow: Action<{ width: number, height: number }> = new Action();
}
