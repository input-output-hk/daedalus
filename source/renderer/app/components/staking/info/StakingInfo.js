// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingInfo.scss';

type Props = {
  name: string,
};

@observer
export default class StakingInfo extends Component<Props> {
  render() {
    return <div className={styles.component}>{this.props.name}</div>;
  }
}
