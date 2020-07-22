// @flow
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
    id: 'static.splash.network.stnVersionName',
    defaultMessage: '!!!SHELLEY TESTNET',
    description: 'SHELLEY TESTNET',
  },
  stnDescription: {
    id: 'static.splash.network.stnDescription',
    defaultMessage:
      '!!!This version of Daedalus has been created to be used specifically with the third iteration of the Shelley testnet network.',
    description: 'stnDescription on network splash screen',
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
    id: 'static.splash.network.stnLinkUrl',
    defaultMessage: '!!!https://testnets.cardano.org/en/shelley/overview/',
    description: '"Learn more" link URL on the STN network splash screen',
  },
});

type Props = {
  onClose: Function,
  openExternalLink: Function,
  currentLocale: string,
};

export default class SplashNetworkSTN extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, openExternalLink, currentLocale } = this.props;
    const title = intl.formatMessage(messages.title);
    const subTitle1 = intl.formatMessage(messages.versionName);
    const description = <FormattedHTMLMessage {...messages.stnDescription} />;
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
        description={description}
        buttonLabel={buttonLabel}
        linkLabel={currentLocale === 'en-US' && linkLabel}
      />
    );
  }
}
