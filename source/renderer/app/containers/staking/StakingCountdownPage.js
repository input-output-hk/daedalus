// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import StakingCountdown from '../../components/staking/countdown/StakingCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';

const messages = defineMessages({
  learnMoreLinkUrl: {
    id: 'staking.countdown.learnMore.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Learn more" link URL in the staking countdown page',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingCountdownPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: {} };

  handleLearnMoreClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  render() {
    const { stores, actions } = this.props;
    const { staking } = stores;
    const {
      staking: { goToStakingPage },
    } = actions;
    const redirectToStakingPage = goToStakingPage.trigger;

    return (
      <StakingCountdown
        redirectToStakingPage={redirectToStakingPage}
        startDateTime={staking.startDateTime}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}
