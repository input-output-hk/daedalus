import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import classnames from 'classnames';
import styles from './NormalSwitch.scss';

type Props = {
  className?: string;
  checked?: boolean;
  label?: string;
  onChange?: (...args: Array<any>) => any;
};
export default class NormalSwitch extends Component<Props> {
  render() {
    return (
      <Checkbox
        className={classnames(styles.component, this.props.className)}
        themeId={IDENTIFIERS.SWITCH}
        skin={SwitchSkin}
        checked={this.props.checked}
        onChange={this.props.onChange}
        label={this.props.label}
      />
    );
  }
}
