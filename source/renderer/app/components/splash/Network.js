// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import backgroundImage from '../../assets/images/splash-network-bg-faded.inline.svg';
import daedalusIcon from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './Network.scss';

const messages = defineMessages({
  title: {
    id: 'static.splash.network.title',
    defaultMessage: '!!!Daedalus',
    description: 'Daedalus',
  },
  actionLabel: {
    id: 'static.splash.network.actionLabel',
    defaultMessage: '!!!I understand',
    description: 'I understand',
  },
  learnMore: {
    id: 'static.splash.network.learnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
});

type Props = {
  subTitle1: string,
  subTitle2: string,
  description: string,
  onClose: Function,
  onLearnMoreClick: Function,
};

export default class SplashNetwork extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      subTitle1,
      subTitle2,
      description,
      onClose,
      onLearnMoreClick,
    } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.backgroundContainer}>
          <SVGInline svg={backgroundImage} className={styles.backgroundImage} />
        </div>
        <SVGInline svg={daedalusIcon} className={styles.daedalusIcon} />
        <div className={styles.title}>{intl.formatMessage(messages.title)}</div>
        <div className={styles.subTitle1}>{subTitle1}</div>
        <div className={styles.subTitle2}>{subTitle2}</div>
        <div className={styles.description}>{description}</div>
        <div className={styles.action}>
          <Button
            className={styles.actionButton}
            label={intl.formatMessage(messages.actionLabel)}
            onClick={onClose}
            skin={ButtonSkin}
          />
        </div>
        <div className={styles.learnMore}>
          <button onClick={onLearnMoreClick}>
            {intl.formatMessage(messages.learnMore)}
            <SVGInline svg={externalLinkIcon} />
          </button>
        </div>
      </div>
    );
  }
}
