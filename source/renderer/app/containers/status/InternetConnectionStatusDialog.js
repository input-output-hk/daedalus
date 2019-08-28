// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import InternetConnectionOfflineStatus from '../../components/status/InternetConnectionOfflineStatus';
import styles from './InternetConnectionStatusDialog.scss';

@inject('stores')
@observer
export default class InternetConnectionStatusDialog extends Component<any> {
  render() {
    const { stores } = this.props;
    const {
      checkingInternetConnection,
      checkInternetConnectionStatus,
    } = stores.networkStatus;

    return (
      <ReactModal
        isOpen
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <InternetConnectionOfflineStatus
          checking={checkingInternetConnection}
          checkAgain={checkInternetConnectionStatus}
        />
      </ReactModal>
    );
  }
}
