// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingDelegationCenter.scss';

type Props = {
  name: string,
};

@observer
export default class StakingDelegationCenter extends Component<Props> {
  render() {
    return (
      <div className={styles.component}>
        {this.props.name}
        {this.props.children}
      </div>
    );
  }
}
