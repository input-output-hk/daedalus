import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NormalSwitch.scss' or its co... Remove this comment to see the full error message
import styles from './NormalSwitch.scss';

type Props = {
  checked?: boolean;
  label?: string;
  onChange?: (...args: Array<any>) => any;
};
export default class NormalSwitch extends Component<Props> {
  render() {
    return (
      <Checkbox
        className={styles.component}
        themeId={IDENTIFIERS.SWITCH}
        skin={SwitchSkin}
        checked={this.props.checked}
        onChange={this.props.onChange}
        label={this.props.label}
      />
    );
  }
}
