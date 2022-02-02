import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingSwitch.scss' or its c... Remove this comment to see the full error message
import styles from './StakingSwitch.scss';

type Props = {
  active: boolean;
};

@observer
class StakingSwitch extends Component<Props> {
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

export default StakingSwitch;
