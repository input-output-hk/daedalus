import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Splash from '../widgets/splash/Splash';

const messages = defineMessages({
  title: {
    id: 'static.splash.network.title',
    defaultMessage: '!!!Daedalus',
    description: 'Daedalus',
  },
  versionName: {
    id: 'static.splash.network.flightVersionName',
    defaultMessage: '!!!FLIGHT',
    description: 'FLIGHT',
  },
  networkName: {
    id: 'static.splash.network.flightNetworkName',
    defaultMessage: '!!!CARDANO MAINNET',
    description: 'Rewards',
  },
  flightDescription1: {
    id: 'static.splash.network.flightDescription1',
    defaultMessage:
      '!!!Thank you for downloading the Daedalus Flight wallet! This version of Daedalus is specially created so users can test new features and we can squash usability bugs before pushing releases to the mainnet production version of the Daedalus wallet. ',
    description: 'flightDescription1 on network splash screen',
  },
  flightDescription2: {
    id: 'static.splash.network.flightDescription2',
    defaultMessage:
      '!!!Although Flight candidates are designed to test functionality, this is on the mainnet and will be using mainnet ada. Transactions made using Flight candidates will be real ada payments. If you are not a power user, we recommend you stick to using our stable, fully-tested production Daedalus wallet client. It is very important to note that transactions performed in Daedalus Flight are real and your funds will be transferred because the Cardano blockchain will be validating all transactions on mainnet.',
    description: 'flightDescription2 on network splash screen',
  },
  flightDescription3: {
    id: 'static.splash.network.flightDescription3',
    defaultMessage:
      '!!!This is a separate and secure installation, but you will be able to run both Flight and production versions of Daedalus at the same time. To help you differentiate between the two wallets, Daedalus Flight will have a different, dark-blue-and-yellow user interface theme. ',
    description: 'flightDescription3 on network splash screen',
  },
  flightDescription4: {
    id: 'static.splash.network.flightDescription4',
    defaultMessage:
      '!!!If you already have a production version of Daedalus installed on your computer, your wallets should be visible in this Flight version as well, and you should have access to your ada in both versions of Daedalus.',
    description: 'flightDescription4 on network splash screen',
  },
  flightDescription5: {
    id: 'static.splash.network.flightDescription5',
    defaultMessage:
      '!!!If you do spot any bugs or inconsistencies in balances and transaction history when using Flight candidates, or want to suggest improvements, feed them directly back to the IOHK development team by submitting a support ticket from the wallet. Wherever relevant, please include your wallet logs so the team can properly assess any issues. ',
    description: 'flightDescription5 on network splash screen',
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
    id: 'static.splash.network.flightLinkUrl',
    defaultMessage: '!!!https://daedaluswallet.io/flight',
    description: '"Learn more" link URL on the network splash screen',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  openExternalLink: (...args: Array<any>) => any;
};
export default class SplashNetworkFlight extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, openExternalLink } = this.props;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = intl.formatMessage(messages.versionName);
    const subTitle2 = intl.formatMessage(messages.networkName);
    const description = (
      <>
        <p>{intl.formatMessage(messages.flightDescription1)}</p>
        <FormattedHTMLMessage tagName="p" {...messages.flightDescription2} />
        <p>{intl.formatMessage(messages.flightDescription3)}</p>
        <p>{intl.formatMessage(messages.flightDescription4)}</p>
        <p>{intl.formatMessage(messages.flightDescription5)}</p>
      </>
    );
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
      />
    );
  }
}
