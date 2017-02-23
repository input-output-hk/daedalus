// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import NodeUpdateNotification from '../../components/notifications/NodeUpdateNotification';

@inject('stores', 'actions') @observer
export default class NodeUpdatePage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      nodeUpdate: PropTypes.shape({
        isNotificationExpanded: PropTypes.bool.isRequired,
        updateTitle: PropTypes.string.isRequired,
      }),
    }).isRequired,
    actions: PropTypes.shape({
      nodeUpdate: PropTypes.shape({
        acceptNodeUpdate: PropTypes.func.isRequired,
        postponeNodeUpdate: PropTypes.func.isRequired,
        toggleNodeUpdateNotificationExpanded: PropTypes.func.isRequired,
      }),
    }).isRequired
  };

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
