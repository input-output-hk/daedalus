// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletSettingsLayout.scss';

type Props = {
  children: Node,
  menu: Node,
};

@observer
export default class WalletSettingsLayout extends Component<Props> {
  render() {
    const { menu, children } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.settingsPaneWrapper}>
          <div className={styles.settingsPane}>{children}</div>
        </div>
        {menu}
      </div>
    );
  }
}
