// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SplashNetworkSTN from '../../components/splash/SplashNetworkSTN';
import SplashNetworkITN from '../../components/splash/SplashNetworkITN';
import SplashNetworkFlight from '../../components/splash/SplashNetworkFlight';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class SplashNetworkPage extends Component<Props> {
  static defaultProps = { actions: null, stores: {} };

  render() {
    const { networkStatus: networkStatusActions } = this.props.actions;
    const { openExternalLink } = this.props.stores.app;
    const {
      isIncentivizedTestnetTheme,
      currentLocale,
    } = this.props.stores.profile;
    const { isIncentivizedTestnet, isFlight } = global;
    if (global.isShelleyTestnet) {
      return (
        <SplashNetworkSTN
          onClose={() => networkStatusActions.toggleSplash.trigger()}
          openExternalLink={openExternalLink}
          currentLocale={currentLocale}
        />
      );
    }
    if (isIncentivizedTestnet) {
      return (
        <SplashNetworkITN
          onClose={() => networkStatusActions.toggleSplash.trigger()}
          openExternalLink={openExternalLink}
          isIncentivizedTestnetTheme={isIncentivizedTestnetTheme}
        />
      );
    }
    if (isFlight) {
      return (
        <SplashNetworkFlight
          onClose={() => networkStatusActions.toggleSplash.trigger()}
          openExternalLink={openExternalLink}
        />
      );
    }
    return null;
  }
}
