// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './BlockGenerationInfo.scss';

@observer
export default class BlockGenerationInfo extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <div className={styles.heading}>Cardano decentralisation</div>
        <div className={styles.info}>
          Cardano will soon start its transition from a federated to a
          decentralized system. This will mark the beginning of the reward era
          in which stakeholders will be able to participate in the process of
          staking or can delegate their stake to stake pools to earn rewards in
          ada.
        </div>
        <div className={styles.timeLeftDesc}>Reward era begins in</div>
        <div className={styles.timeLeft}>3 days 6 hours 10 minutes</div>
        <Button label="Learn more" skin={ButtonSkin} />
      </div>
    );
  }
}
