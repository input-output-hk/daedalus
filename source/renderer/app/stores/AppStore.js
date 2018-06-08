// @flow
import { observable, computed } from 'mobx';
import { ipcRenderer, shell } from 'electron';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';
import { OPEN_ABOUT_DIALOG_CHANNEL } from '../../../common/ipc-api/open-about-dialog';
import AboutDialog from '../containers/static/AboutDialog';

export default class AppStore extends Store {

  @observable error: ?LocalizableError = null;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    ipcRenderer.on(OPEN_ABOUT_DIALOG_CHANNEL, this._triggerAboutDialog);
  }

  teardown() {
    ipcRenderer.removeListener(OPEN_ABOUT_DIALOG_CHANNEL, this._triggerAboutDialog);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  openExternalLink(link: string): void {
    shell.openExternal(link);
  }

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };

  _triggerAboutDialog = () => {
    this.actions.dialogs.open.trigger({ dialog: AboutDialog });
  }
}
