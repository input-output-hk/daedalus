// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
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
  incentivizedTestnet: {
    id: 'static.splash.network.incentivizedTestnet',
    defaultMessage: '!!!Incentivized Testnet',
    description: 'Incentivized Testnet',
  },
  balanceCheck: {
    id: 'static.splash.network.balanceCheck',
    defaultMessage: '!!!Balance Check',
    description: 'Balance Check',
  },
  incentivizedTestnetDescription: {
    id: 'static.splash.network.incentivizedTestnetDescription',
    defaultMessage:
      '!!!This version of Daedalus has been created specifically for the balance check, the first stage in the roll-out of the Incentivized Testnet. It is not compatible with the Cardano mainnet. The balance check is a practice run for the official balance snapshot that is currently planned for later in November. This initial test will allow us to test core functionality, while enabling users to validate that the value of their mainnet ada balances is accurately captured ahead of the Incentivized Testnet.',
    description:
      'This version of Daedalus has been created specifically for the balance check, the first stage in the roll-out of the Incentivized Testnet. It is not compatible with the Cardano mainnet. The balance check is a practice run for the official balance snapshot that is currently planned for later in November. This initial test will allow us to test core functionality, while enabling users to validate that the value of their mainnet ada balances is accurately captured ahead of the Incentivized Testnet.',
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
  isIncentivizedTestnet: boolean,
  onClose: Function,
  onLearnMoreClick: Function,
};

export default class SplashNetwork extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { isIncentivizedTestnet, onClose, onLearnMoreClick } = this.props;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = intl.formatMessage(messages.incentivizedTestnet);
    const subTitle2 = intl.formatMessage(messages.balanceCheck);
    const description = (
      <FormattedHTMLMessage {...messages.incentivizedTestnetDescription} />
    );
    const actionLabel = intl.formatMessage(messages.actionLabel);

    return (
      <div className={styles.component}>
        <div className={styles.backgroundContainer}>
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
        <div className={styles.content}>
          <SVGInline svg={daedalusIcon} className={styles.daedalusIcon} />
          <div className={styles.title}>{title}</div>
          <div className={styles.subTitle1}>{subTitle1}</div>
          <div className={styles.subTitle2}>{subTitle2}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.action}>
            <Button label={actionLabel} onClick={onClose} skin={ButtonSkin} />
          </div>
          <div className={styles.learnMore}>
            <button onClick={onLearnMoreClick}>
              {intl.formatMessage(messages.learnMore)}
              <SVGInline svg={externalLinkIcon} />
            </button>
          </div>
        </div>
      </div>
    );
  }
}
