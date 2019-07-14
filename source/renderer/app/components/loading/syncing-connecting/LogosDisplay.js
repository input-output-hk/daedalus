// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import Lottie from 'react-lottie';
import styles from './LogosDisplay.scss';
import adaLogo from '../../../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../../../assets/images/cardano-logo.inline.svg';
import animationData from './logo-animation-data.json';

type Props = {
  isConnected: boolean,
};

const logoAnimationOptionsLottie = {
  loop: true,
  autoplay: true,
  animationData,
  rendererSettings: {
    preserveAspectRatio: 'xMidYMid slice',
  },
};

export default class LogosDisplay extends Component<Props> {
  componentDidMount() {
    // Manual adjustment due to `logo-animation-data.json` canvas size
    const svg: Object = document.querySelector(
      '.LogosDisplay_daedalusLogo svg'
    );
    svg.setAttribute('viewBox', '534 250 212 220');
  }

  render() {
    const { isConnected } = this.props;
    const currencyLogoStyles = classNames(styles['ada-logo'], {
      [styles.connectingLogo]: !isConnected,
      [styles.syncingLogo]: isConnected,
    });
    const daedalusLogoStyles = classNames(styles.daedalusLogo, {
      [styles.connectingLogo]: !isConnected,
      [styles.syncingLogo]: isConnected,
    });
    const apiLogoStyles = classNames(styles['ada-apiLogo'], {
      [styles.connectingLogo]: !isConnected,
      [styles.syncingLogo]: isConnected,
    });

    return (
      <div className={styles.component}>
        <SVGInline svg={adaLogo} className={currencyLogoStyles} />
        <div className={daedalusLogoStyles}>
          <Lottie options={logoAnimationOptionsLottie} />
        </div>
        <SVGInline svg={cardanoLogo} className={apiLogoStyles} />
      </div>
    );
  }
}
