// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import StakingSystemStateElement from './StakingSystemStateElement';
import styles from './StakingSystemState.scss';

@observer
export default class StakingSystemState extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <div className={styles.heading}>System state</div>
        <StakingSystemStateElement value="YY" label="epoch" />
        <StakingSystemStateElement value="XX" label="slot" />
        <StakingSystemStateElement value="XX:XX" label="slot time left" />
        <StakingSystemStateElement value="Shares" label="MPC phase" />
        <StakingSystemStateElement value="AA" label="commitments" />
        <StakingSystemStateElement value="BB" label="openings" />
        <StakingSystemStateElement value="CC" label="shares" />
      </div>
    );
  }
}
