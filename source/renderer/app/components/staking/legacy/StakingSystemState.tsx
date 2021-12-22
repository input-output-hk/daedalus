import React, { Component } from 'react';
import { observer } from 'mobx-react';
import StakingSystemStateElement from './StakingSystemStateElement';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingSystemState.scss' or ... Remove this comment to see the full error message
import styles from './StakingSystemState.scss';

@observer
class StakingSystemState extends Component<any> {
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

export default StakingSystemState;
