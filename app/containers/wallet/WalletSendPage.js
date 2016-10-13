// @flow
import React, { Component } from 'react';
import { Row } from 'react-flexbox-grid/lib/index';
import AppBar from 'material-ui/AppBar';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import WalletNavigation from '../../components/wallet/navigation/WalletNavigation';
import styles from './WalletSendPage.scss';

export default class WalletSendPage extends Component {
  render() {
    return (
      <div className={styles.component}>

        <AppBar
          title="Daedalus"
          className={styles.appBar}
        />

        <WalletNavigation />

        <Row center="xs">
          <WalletSendForm />
        </Row>

      </div>
    );
  }
}
