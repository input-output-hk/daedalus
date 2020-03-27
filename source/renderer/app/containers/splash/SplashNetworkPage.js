// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SplashNetworkITN from '../../components/splash/SplashNetworkITN';
import SplashNetworkByronReboot from '../../components/splash/SplashNetworkByronReboot';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class SplashNetworkPage extends Component<Props> {
  static defaultProps = { actions: null, stores: {} };

  render() {
    const { networkStatus: networkStatusActions } = this.props.actions;
    const { openExternalLink } = this.props.stores.app;
    const { isIncentivizedTestnet, isFlight } = global;
    if (isIncentivizedTestnet) {
      return (
        <SplashNetworkITN
          onClose={() => networkStatusActions.toggleSplash.trigger()}
          openExternalLink={openExternalLink}
        />
      );
    }
    if (isFlight) {
      return (
        <SplashNetworkByronReboot
          onClose={() => networkStatusActions.toggleSplash.trigger()}
          openExternalLink={openExternalLink}
        />
      );
    }
    return null;
  }
}
