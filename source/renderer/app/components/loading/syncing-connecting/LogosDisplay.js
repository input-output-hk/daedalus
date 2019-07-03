// @flow
import React from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './LogosDisplay.scss';
import adaLogo from '../../../assets/images/ada-logo.inline.svg';
import daedalusLogo from '../../../assets/images/daedalus-logo-loading-grey.inline.svg';
import cardanoLogo from '../../../assets/images/cardano-logo.inline.svg';

type Props = {
  isConnected: boolean,
};

const LogosDisplay = ({ isConnected }: Props) => {
  const currencyLogoStyles = classNames([
    styles['ada-logo'],
    !isConnected ? styles.connectingLogo : styles.syncingLogo,
  ]);
  const daedalusLogoStyles = classNames([
    styles.daedalusLogo,
    !isConnected ? styles.connectingLogo : styles.syncingLogo,
  ]);
  const apiLogoStyles = classNames([
    styles['ada-apiLogo'],
    !isConnected ? styles.connectingLogo : styles.syncingLogo,
  ]);

  return (
    <div className={styles.component}>
      <SVGInline svg={adaLogo} className={currencyLogoStyles} />
      <SVGInline svg={daedalusLogo} className={daedalusLogoStyles} />
      <SVGInline svg={cardanoLogo} className={apiLogoStyles} />
    </div>
  );
};

export default LogosDisplay;
