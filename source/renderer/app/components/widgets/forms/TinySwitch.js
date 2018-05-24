import React, { Component } from 'react';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/raw/SwitchSkin';
import styles from './TinySwitch.scss';

type Props = {
  checked: boolean,
  label: string,
};

export default class TinySwitch extends Component<Props> {

  render() {

    return (
      <Checkbox
        skin={
          <SimpleSwitchSkin
            className={styles.component}
          />
        }
        checked={this.props.checked}
        onChange={this.props.onChange}
        label={this.props.label}
      />
    );
  }

}
