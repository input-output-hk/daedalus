// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import environment from '../environment'

export default class NodeUpdateStore extends Store {

  NODE_UPDATE_POLL_INTERVAL = 5000;

  @observable isUpdateAvailable = false;
  @observable isUpdatePostponed = false;
  @observable isNotificationExpanded = false;
  @observable isUpdateInstalled = false;
  @observable updateTitle = '';
  @observable updateVersion = null;
  @observable nextUpdateRequest = new Request(this.api, 'nextUpdate');
  @observable applyUpdateRequest = new Request(this.api, 'applyUpdate');

  constructor(...args) {
    super(...args);
    this.actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    this.actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    this.actions.toggleNodeUpdateNotificationExpanded.listen(this._toggleNodeUpdateNotificationExpanded);
    if (environment.CARDANO_API) {
      setInterval(this.refreshNextUpdate, this.NODE_UPDATE_POLL_INTERVAL);
    }
  }

  @action refreshNextUpdate = () => {
    this.nextUpdateRequest.execute();
    if (this.nextUpdateRequest.result && !this.isUpdatePostponed && !this.isUpdateInstalled) {
      this.isUpdateAvailable = true;
      this.isNotificationExpanded = true;
      this.updateVersion = this.nextUpdateRequest.result.version;
      this.updateTitle = `Cardano-Core update v${this.updateVersion} is available`;
    }
  };

  @action _postponeNodeUpdate = () => {
    this.isUpdatePostponed = true;
  };

  @action _acceptNodeUpdate = () => {
    this.applyUpdateRequest.execute();
    this.isUpdateAvailable = false;
    this.isUpdateInstalled = true;
  };

  @action _toggleNodeUpdateNotificationExpanded = () => {
    this.isNotificationExpanded = !this.isNotificationExpanded;
  };

}
