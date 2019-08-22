// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import InternetConnectionOfflineStatus from '../../components/status/InternetConnectionOfflineStatus';
import styles from './InternetConnectionStatusDialog.scss';

@inject('stores')
@observer
export default class InternetConnectionStatusDialog extends Component<any> {
  componentDidMount() {
    const { stores } = this.props;
    const { updateInternetConnectionStatus } = stores.networkStatus;

    updateInternetConnectionStatus();
  }

  render() {
    const { stores } = this.props;
    const {
      isInternetConnected,
      updateInternetConnectionStatus,
    } = stores.networkStatus;

    if (isInternetConnected) {
      return null;
    }

    return (
      <ReactModal
        isOpen
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <InternetConnectionOfflineStatus
          checkAgain={updateInternetConnectionStatus}
        />
      </ReactModal>
    );
  }
}
