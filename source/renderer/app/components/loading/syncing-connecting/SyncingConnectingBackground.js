// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import backgroundImage from '../../../assets/images/circle-bg-faded.inline.svg';
import styles from './SyncingConnectingBackground.scss';
import { THEMES } from '../../../themes';

type Props = {
  currentTheme: string,
  isConnecting: boolean,
  isSyncing: boolean,
  hasLoadedCurrentTheme: boolean,
};

@observer
export default class SyncingConnectingBackground extends Component<Props> {
  render() {
    const {
      isConnecting,
      isSyncing,
      hasLoadedCurrentTheme,
      currentTheme,
    } = this.props;
    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles.isLoadingTheme,
      isConnecting ? styles.isConnecting : null,
      isSyncing ? styles.isSyncing : null,
    ]);
    return (
      <div className={componentStyles}>
        {currentTheme === THEMES.INCENTIVIZED_TESTNET && (
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
