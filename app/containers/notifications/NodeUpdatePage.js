// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import NodeUpdateNotification from '../../components/notifications/NodeUpdateNotification';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class NodeUpdatePage extends Component {

  props: InjectedProps;

  render() {
    const {
      isNotificationExpanded,
      updateTitle,
    } = this.props.stores.nodeUpdate;
    const {
      acceptNodeUpdate,
      postponeNodeUpdate,
      toggleNodeUpdateNotificationExpanded
    } = this.props.actions.nodeUpdate;

    return (
      <NodeUpdateNotification
        title={updateTitle}
        onAccept={acceptNodeUpdate}
        onPostpone={postponeNodeUpdate}
        onToggleExpanded={toggleNodeUpdateNotificationExpanded}
        isExpanded={isNotificationExpanded}
      />
    );
  }
}
