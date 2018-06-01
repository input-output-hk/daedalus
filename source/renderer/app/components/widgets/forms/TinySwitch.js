import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinySwitch.scss';

type Props = {
  checked: boolean,
  label: string,
};

export default class TinySwitch extends Component<Props> {

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
