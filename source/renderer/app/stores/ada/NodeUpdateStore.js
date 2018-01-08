// @flow
import { observable, action } from 'mobx';
import Store from '../lib/Store';
import Request from '../lib/LocalizedRequest';
import type {
  NextUpdateResponse, PostponeUpdateResponse, ApplyUpdateResponse
} from '../../api/ada/index';

export default class NodeUpdateStore extends Store {

  NODE_UPDATE_POLL_INTERVAL = 5000;

  @observable isUpdateAvailable = false;
  @observable isUpdatePostponed = false;
  @observable isNotificationExpanded = false;
  @observable isUpdateInstalled = false;
  @observable updateVersion = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable nextUpdateRequest: Request<NextUpdateResponse> = new Request(this.api.ada.nextUpdate);
  @observable postponeUpdateRequest: Request<PostponeUpdateResponse> = new Request(this.api.ada.postponeUpdate);
  @observable applyUpdateRequest: Request<ApplyUpdateResponse> = new Request(this.api.ada.applyUpdate);
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.ada.nodeUpdate;
    actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    actions.toggleNodeUpdateNotificationExpanded.listen(this._toggleNotificationExpanded);
    setInterval(this.refreshNextUpdate, this.NODE_UPDATE_POLL_INTERVAL);
  }

  @action refreshNextUpdate = () => {
    if (this.stores.networkStatus.isSynced) {
      this.nextUpdateRequest.execute();
      if (this.nextUpdateRequest.result && !this.isUpdateAvailable &&
        !this.isUpdatePostponed && !this.isUpdateInstalled) {
        this.isUpdateAvailable = true;
        this.isNotificationExpanded = true;
        this.updateVersion = this.nextUpdateRequest.result.version;
      }
    }
  };

  @action _postponeNodeUpdate = () => {
    this.postponeUpdateRequest.execute();
    this.isUpdatePostponed = true;
  };

  @action _acceptNodeUpdate = () => {
    this.applyUpdateRequest.execute();
    this.isUpdateAvailable = false;
    this.isUpdateInstalled = true;
  };

  @action _toggleNotificationExpanded = () => {
    this.isNotificationExpanded = !this.isNotificationExpanded;
  };

}
