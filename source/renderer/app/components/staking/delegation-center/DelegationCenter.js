// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './DelegationCenter.scss';
import type { Node } from 'react';

type Props = {
  name: string,
  children: ?Node,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const { name, children } = this.props;
    return (
      <div className={styles.component}>
        {name}
        {children}
      </div>
    );
  }
}
