// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import Layout from '../MainLayout';
import WalletImporter from '../../components/wallet/WalletImporter';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores') @observer
export default class WalletImporterPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  constructor(props: Props) {
    super(props);
    props.stores.sidebar._resetActivateSidebarCategory();
  }

  render() {
    return <Layout><WalletImporter /></Layout>;
  }

}
