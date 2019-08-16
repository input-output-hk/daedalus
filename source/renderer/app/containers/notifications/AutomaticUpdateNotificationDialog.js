// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import AutomaticUpdateNotification from '../../components/notifications/AutomaticUpdateNotification';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class AutomaticUpdateNotificationDialog extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { nextUpdateVersion } = stores.nodeUpdate;
    const { environment } = stores.app;
    const { version } = environment;
    const { acceptNodeUpdate, postponeNodeUpdate } = actions.nodeUpdate;

    return (
      <AutomaticUpdateNotification
        currentAppVersion={version}
        nextUpdateVersion={nextUpdateVersion}
        onAccept={acceptNodeUpdate.trigger}
        onPostpone={postponeNodeUpdate.trigger}
      />
    );
  }
}
