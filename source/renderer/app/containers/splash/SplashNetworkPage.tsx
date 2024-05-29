import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SplashNetworkFlight from '../../components/splash/SplashNetworkFlight';

type Props = InjectedProps;

class SplashNetworkPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: {},
  };

  render(): React.ReactNode {
    const { networkStatus: networkStatusActions } = this.props.actions;
    const { openExternalLink } = this.props.stores.app;

    if (global.isFlight) {
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

export default inject('stores', 'actions')(observer(SplashNetworkPage));
