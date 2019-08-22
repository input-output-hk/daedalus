// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import InternetConnectionOfflineStatus from '../../components/status/InternetConnectionOfflineStatus';
import styles from './InternetConnectionStatusDialog.scss';
import type { InjectedStoresProps } from '../../types/injectedPropsType';

type Props = InjectedStoresProps;

@inject('stores')
@observer
export default class InternetConnectionStatusDialog extends Component<Props> {
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
