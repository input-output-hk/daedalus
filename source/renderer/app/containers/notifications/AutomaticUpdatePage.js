// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import AutomaticUpdate from '../../components/notifications/AutomaticUpdate';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class AutomaticUpdatePage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { availableAppVersion } = stores.nodeUpdate;
    const { environment } = stores.app;
    const { version } = environment;
    const { acceptNodeUpdate, postponeNodeUpdate } = actions.nodeUpdate;

    return (
      <AutomaticUpdate
        availableAppVersion={availableAppVersion}
        currentAppVersion={version}
        onAccept={acceptNodeUpdate.trigger}
        onPostpone={postponeNodeUpdate.trigger}
      />
    );
  }
}
