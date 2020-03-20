// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import SplashNetwork from '../../components/splash/Network';

const messages = defineMessages({
  learnMoreLinkUrl: {
    id: 'static.splash.network.learnMore.linkUrl',
    defaultMessage: '!!!http://staking.cardano.org/',
    description: '"Learn more" link URL on the network splash screen',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class SplashNetworkPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: {} };

  handleLearnMoreClick = () => {
    const { intl } = this.context;
    const { stores } = this.props;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);

    stores.app.openExternalLink(learnMoreLinkUrl);
  };

  render() {
    const { networkStatus: networkStatusActions } = this.props.actions;

    return (
      <SplashNetwork
        isIncentivizedTestnet={global.isIncentivizedTestnet}
        onClose={() => networkStatusActions.toggleSplash.trigger()}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}
