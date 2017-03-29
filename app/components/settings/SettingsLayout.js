// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { oneOrManyChildElements } from '../../propTypes';
import styles from './SettingsLayout.scss';

@observer
export default class SettingsLayout extends Component {

  static propTypes = {
    children: oneOrManyChildElements,
    menu: PropTypes.element.isRequired
  };

  render() {
    const { menu, children } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.settingsPaneWrapper}>
          <div className={styles.settingsPane}>
            {children}
          </div>
        </div>
        {menu}
      </div>
    );
  }

}
