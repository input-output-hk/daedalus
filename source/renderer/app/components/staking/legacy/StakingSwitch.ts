// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './StakingSwitch.scss';

type Props = {
  active: boolean,
};

@observer
export default class StakingSwitch extends Component<Props> {
  handleChange = () => {};

  render() {
    const { active } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>Staking</div>
        <Checkbox
          themeId={IDENTIFIERS.SWITCH}
          onChange={this.handleChange}
          checked={active}
          skin={SwitchSkin}
        />
      </div>
    );
  }
}
