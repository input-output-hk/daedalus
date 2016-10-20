// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import QRCode from 'qrcode.react';
import RaisedButton from 'material-ui/RaisedButton';
import styles from './WalletReceive.scss';

@observer
export default class WalletReceive extends Component {

  static propTypes = {
    walletReceive: React.PropTypes.shape({
      walletAddress: PropTypes.string.isRequired
    }),
  };

  render() {
    const { walletReceive } = this.props;
    return (
      <div className={styles.component}>

        <div className={styles.heading}>
          Your <strong>Shopping wallet</strong> address
        </div>

        <div className={styles.qrCode}>
          <QRCode
            value={walletReceive.walletAddress}
            bgColor="transparent"
            size={240}
          />
        </div>

        <div className={styles.hash}>
          {walletReceive.walletAddress}
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
