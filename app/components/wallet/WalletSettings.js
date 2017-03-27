// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletSettings.scss';

@observer
export default class WalletSettings extends Component {

  render() {
    return (
      <div className={styles.component}>

        <div className={styles.borderedBox}>

          <p>Settings page</p>

        </div>

      </div>
    );
  }

}
