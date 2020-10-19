// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';
import MainLayout from '../MainLayout';

type Props = {
  children: Node,
};

@observer
export default class Voting extends Component<Props> {
  render() {
    const { children } = this.props;
    return <MainLayout>{children}</MainLayout>;
  }
}
