// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import ManualUpdate from '../../components/loading/manual-update/ManualUpdate';

type Props = InjectedProps;

@inject('stores')
@observer
export default class ManualUpdatePage extends Component<Props> {
  static defaultProps = { stores: null };

  render() {
    const { stores } = this.props;
    const { availableAppVersion } = stores.nodeUpdate;
    const { environment, openExternalLink } = stores.app;
    const { version } = environment;
    return (
      <ManualUpdate
        availableAppVersion={availableAppVersion}
        currentAppVersion={version}
        onExternalLinkClick={openExternalLink}
      />
    );
  }
}
