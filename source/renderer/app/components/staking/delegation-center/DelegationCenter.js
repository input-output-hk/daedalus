// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './DelegationCenter.scss';

type Props = {
  name: string,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    return <div className={styles.component}>{this.props.name}</div>;
  }
}
