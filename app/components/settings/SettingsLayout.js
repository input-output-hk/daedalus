// @flow
import React, { Component } from 'react';
import type { Children, Element } from 'react';
import { observer } from 'mobx-react';
import styles from './SettingsLayout.scss';

@observer
export default class SettingsLayout extends Component {

  props: {
    children: Children,
    menu: Element<any>,
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
