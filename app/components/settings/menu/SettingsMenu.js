// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SettingsMenuItem from './SettingsMenuItem';
import styles from './SettingsMenu.scss';

@observer
export default class SettingsMenu extends Component {

  render() {
    return (
      <div>
        <div className={styles.component}>
          <SettingsMenuItem label="Profile" active />
          <SettingsMenuItem label="Security" active={false} />
          <SettingsMenuItem label="Identity and verification" active={false} />
          <SettingsMenuItem label="Display" active={false} />
          <SettingsMenuItem label="Privacy" active={false} />
          <SettingsMenuItem label="Terms of use" active={false} />
          <SettingsMenuItem label="Support" active={false} />
        </div>
      </div>
    );
  }

}
