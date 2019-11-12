// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import backgroundImage from '../../../assets/images/circle-bg-faded.inline.svg';
import styles from './SyncingConnectingBackground.scss';

type Props = {
  isConnecting: boolean,
  isSyncing: boolean,
  hasLoadedCurrentTheme: boolean,
  isIncentivizedTestnet: boolean,
};

@observer
export default class SyncingConnectingBackground extends Component<Props> {
  render() {
    const {
      isConnecting,
      isSyncing,
      hasLoadedCurrentTheme,
      isIncentivizedTestnet,
    } = this.props;
    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles.isLoadingTheme,
      isConnecting ? styles.isConnecting : null,
      isSyncing ? styles.isSyncing : null,
    ]);
    return (
      <div className={componentStyles}>
        {isIncentivizedTestnet && (
          <>
            <div className={styles.backgroundOverlay} />
            <SVGInline
              svg={backgroundImage}
              className={styles.backgroundImage}
            />
          </>
        )}
      </div>
    );
  }
}
