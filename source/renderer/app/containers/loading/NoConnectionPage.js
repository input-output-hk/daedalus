// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedStoresProps } from '../../types/injectedPropsType';
import NoConnectionOverlay from '../../components/loading/no-connection/NoConnectionOverlay';

type Props = InjectedStoresProps;

@inject('stores')
@observer
export default class NoConnectionPage extends Component<Props> {
  static defaultProps = { stores: null };

  render() {
    return <NoConnectionOverlay />;
  }
}
