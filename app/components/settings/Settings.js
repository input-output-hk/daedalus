// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import SettingsMenu from './menu/SettingsMenu';
import styles from './Settings.scss';

@observer
export default class Settings extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired,
  };

  render() {
    const { children } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.settingsPaneWrapper}>
          <div className={styles.settingsPane}>
            {children}
          </div>
        </div>
        <SettingsMenu />
      </div>
    );
  }

}
