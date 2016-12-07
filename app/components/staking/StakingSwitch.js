// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Switch from 'react-toolbox/lib/switch';
import styles from './StakingSwitch.scss';

@observer
export default class StakingSwitch extends Component {

  static propTypes = {
    active: PropTypes.bool.isRequired,
  };

  state = {
    stakingOn: true
  };

  handleChange = () => {
    this.setState({ stakingOn: !this.state.stakingOn });
  };

  render() {
    const { stakingOn } = this.state;
    return (
      <div className={styles.component}>
        <div className={styles.label}>Staking</div>
        <Switch
          checked={stakingOn}
          onChange={() => this.handleChange()}
        />
      </div>
    );
  }

}
