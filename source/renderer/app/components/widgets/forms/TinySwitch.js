import React, { Component } from 'react';
import classNames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleSwitchSkin from 'react-polymorph/lib/skins/simple/SwitchSkin';
import styles from './TinySwitch.scss';

type Props = {
  label: string
};

export default class TinySwitch extends Component<Props> {

  render() {

    return (
      <div>

        <Checkbox
          skin={
            <SimpleSwitchSkin
              className={styles.skin}
            />
          }
          label={this.props.label}
        />

      </div>
    );
  }

}
