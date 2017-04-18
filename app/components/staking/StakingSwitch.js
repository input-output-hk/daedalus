// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Switch from 'react-toolbox/lib/switch';
import styles from './StakingSwitch.scss';

@observer
export default class StakingSwitch extends Component {

  props: {
    active: boolean,
  };

  handleChange = () => {};

  render() {
    const { active } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>Staking</div>
        <Switch
          checked={active}
          onChange={() => this.handleChange()}
        />
      </div>
    );
  }

}
