// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import QRCode from 'qrcode.react';
import RaisedButton from 'material-ui/RaisedButton';
import styles from './WalletReceive.scss';

@observer
export default class WalletReceive extends Component {
  render() {
    return (
      <div className={styles.component}>

        <div className={styles.heading}>
          Your <strong>Shopping wallet</strong> address
        </div>

        <div className={styles.qrCode}>
          <QRCode
            value="3ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR7"
            bgColor="transparent"
            size={240}
          />
        </div>

        <div className={styles.hash}>
          3ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR7
        </div>

        <div className={styles.instructions}>
          Share this wallet address to receive payments. To protect your
          privacy, new addresses are generated automatically once
          you use them. <a>Generate new address</a>.
        </div>

        <RaisedButton
          className={styles.requestButton}
          label="Request a specific amount"
          primary
          fullWidth
        />

      </div>
    );
  }
}
