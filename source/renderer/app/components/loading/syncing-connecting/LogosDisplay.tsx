import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import Lottie from 'react-lottie';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './LogosDisplay.scss' or its co... Remove this comment to see the full error message
import styles from './LogosDisplay.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-log... Remove this comment to see the full error message
import adaLogo from '../../../assets/images/ada-logo.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/cardano... Remove this comment to see the full error message
import cardanoLogo from '../../../assets/images/cardano-logo.inline.svg';
import animationData from './logo-animation-data.json';

type Props = {
  isConnected: boolean;
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
    const svg: Record<string, any> = document.querySelector(
      '.LogosDisplay_daedalusLogo svg'
    );
    svg.setAttribute('viewBox', '534 250 212 220');
  }

  render() {
    const { isConnected } = this.props;
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
        <div className={daedalusLogoStyles}>
          <Lottie options={logoAnimationOptionsLottie} />
        </div>
        <SVGInline svg={cardanoLogo} className={apiLogoStyles} />
      </div>
    );
  }
}
