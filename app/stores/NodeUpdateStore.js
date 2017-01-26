// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';

export default class NodeUpdateStore extends Store {

  @observable isUpdateAvailable = true;
  @observable isUpdateInProgress = false;
  @observable isNotificationExpanded = false;
  @observable updateTitle = '';
  @observable updateDescription = '';
  @observable getCardanoNodeUpdateData = new CachedRequest(this.api, 'getCardanoNodeUpdateData');
  @observable updateCardanoNodeRequest = new Request(this.api, 'updateCaradnoNode');
  @observable postponeCaradnoNodeUpdate = new Request(this.api, 'postponeCaradnoNodeUpdate');

  constructor(...args) {
    super(...args);
    this.actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    this.actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    this.actions.toggleNodeUpdateNotificationExpanded.listen(this._toggleNodeUpdateNotificationExpanded);
  }

  @action _acceptNodeUpdate = () => {
  };

  @action _postponeNodeUpdate = () => {
  };

  @action _toggleNodeUpdateNotificationExpanded = () => {
    this.isNotificationExpanded = !this.isNotificationExpanded;
  };

}
