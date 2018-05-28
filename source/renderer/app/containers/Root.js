// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import resolver from '../utils/imports';
import environment from '../../../common/environment';
import WalletAddPage from './wallet/WalletAddPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

const LoadingPage = resolver('containers/LoadingPage');

type Props = InjectedContainerProps;

@inject(['stores']) @observer
export default class Root extends Component<Props> {

  render() {
    const { stores, children } = this.props;
    const { networkStatus, profile } = stores;
    const wallets = stores[environment.API].wallets;
    if (networkStatus.isConnected && profile.isSettingsPage) {
      return React.Children.only(children);
    }
    if (
      !networkStatus.isSynced ||
      !wallets.hasLoadedWallets ||
      !networkStatus.isSystemTimeCorrect
    ) {
      return <LoadingPage />;
    } else if (!wallets.hasAnyWallets) {
      return <WalletAddPage />;
    }
    return React.Children.only(children);
  }
}

