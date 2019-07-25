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
    const { updateVersion } = stores.nodeUpdate;
    const { environment } = stores.app;
    const { acceptNodeUpdate, postponeNodeUpdate } = actions.nodeUpdate;
    const { version } = environment;

    return (
      <AutomaticUpdate
        availableAppVersion={updateVersion}
        currentAppVersion={version}
        onAccept={acceptNodeUpdate.trigger}
        onPostpone={postponeNodeUpdate.trigger}
      />
    );
  }
}
