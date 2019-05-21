// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingStakePools.scss';

type Props = {
  name: string,
};

@observer
export default class StakingStakePools extends Component<Props> {
  render() {
    return <div className={styles.component}>{this.props.name}</div>;
  }
}
