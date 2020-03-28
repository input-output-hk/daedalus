// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { inject } from 'mobx-react';
import backgroundImage from '../../assets/images/circle-bg-faded.inline.svg';
import Splash from '../widgets/splash/Splash';

const messages = defineMessages({
  title: {
    id: 'static.splash.network.title',
    defaultMessage: '!!!Daedalus',
    description: 'Daedalus',
  },
  versionName: {
    id: 'static.splash.network.itnVersionName',
    defaultMessage: '!!!INCENTIVIZED TESTNET v1',
    description: 'INCENTIVIZED TESTNET v1',
  },
  networkName: {
    id: 'static.splash.network.itnNetworkName',
    defaultMessage: '!!!Rewards',
    description: 'Rewards',
  },
  itnDescription: {
    id: 'static.splash.network.itnDescription',
    defaultMessage:
      '!!!This version of Daedalus has been created specifically for use with the Incentivized Testnet. It is not compatible with the Cardano mainnet. If you had ada in a mainnet Daedalus or Yoroi wallet at the time of the balance snapshot (12.00 UTC, November 29) you can use this version of Daedalus to restore those funds as testnet ada, for use exclusively on the Incentivized Testnet. The rewards earned for delegating stake and running stake pools on the Incentivized Testnet will be paid out in real ada at the end of the Incentivized Testnet program. Important: Please keep your Rewards wallet recovery phrase safe. You will need it to receive your ada rewards on the mainnet.',
    description:
      'This version of Daedalus has been created specifically for the balance check, the first stage in the roll-out of the Incentivized Testnet. It is not compatible with the Cardano mainnet. The balance check is a practice run for the official balance snapshot that is currently planned for later in November. This initial test will allow us to test core functionality, while enabling users to validate that the value of their mainnet ada balances is accurately captured ahead of the Incentivized Testnet.',
  },
  buttonLabel: {
    id: 'static.splash.network.buttonLabel',
    defaultMessage: '!!!I understand',
    description: 'I understand',
  },
  linkLabel: {
    id: 'static.splash.network.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'static.splash.network.itnLinkUrl',
    defaultMessage: '!!!http://staking.cardano.org/',
    description: '"Learn more" link URL on the network splash screen',
  },
});

type Props = {
  onClose: Function,
  openExternalLink: Function,
  isIncentivizedTestnetTheme?: boolean,
};

export default class SplashNetworkITN extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, openExternalLink, isIncentivizedTestnetTheme } = this.props;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = intl.formatMessage(messages.versionName);
    const subTitle2 = intl.formatMessage(messages.networkName);
    const description = <FormattedHTMLMessage {...messages.itnDescription} />;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const onLinkClick = () =>
      openExternalLink(intl.formatMessage(messages.linkUrl));

    return (
      <Splash
        onButtonClick={onClose}
        onLinkClick={onLinkClick}
        title={title}
        subTitle1={subTitle1}
        subTitle2={subTitle2}
        description={description}
        buttonLabel={buttonLabel}
        linkLabel={linkLabel}
        isIncentivizedTestnetTheme={isIncentivizedTestnetTheme}
        backgroundImage={backgroundImage}
      />
    );
  }
}
