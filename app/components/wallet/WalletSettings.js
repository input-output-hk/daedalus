// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSettings.scss';

@observer
export default class WalletSettings extends Component {

  render() {
    return (
      <div className={styles.component}>

        <BorderedBox>

          <p>Settings page</p>

        </BorderedBox>

      </div>
    );
  }

}
