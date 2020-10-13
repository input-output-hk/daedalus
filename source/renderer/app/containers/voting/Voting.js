// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import MainLayout from '../MainLayout';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

@observer
export default class Voting extends Component<Props> {
  render() {
    const { children } = this.props;
    return <MainLayout>{children}</MainLayout>;
  }
}
