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
class StakingCountdownPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: {},
  };
  handleLearnMoreClick = (event: React.SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  render() {
    const { stores, actions } = this.props;
    const { networkStatus } = stores;
    const {
      staking: { goToStakingInfoPage },
    } = actions;
    const redirectToStakingInfo = goToStakingInfoPage.trigger;
    return (
      <StakingCountdown
        redirectToStakingInfo={redirectToStakingInfo}
        startDateTime={networkStatus.shelleyActivationTime}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}

export default StakingCountdownPage;
