// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import NodeUpdateNotification from '../../components/notifications/NodeUpdateNotification';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class NodeUpdatePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const store = this.props.stores.ada.nodeUpdate;
    const actions = this.props.actions.ada.nodeUpdate;

    return (
      <NodeUpdateNotification
        version={store.updateVersion}
        onAccept={actions.acceptNodeUpdate.trigger}
        onPostpone={actions.postponeNodeUpdate.trigger}
        onToggleExpanded={actions.toggleNodeUpdateNotificationExpanded.trigger}
        isExpanded={store.isNotificationExpanded}
      />
    );
  }
}
