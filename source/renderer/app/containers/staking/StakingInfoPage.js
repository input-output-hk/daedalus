// @flow
import React, { Component } from 'react';
import { Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { ROUTES } from '../../routes-config';
import StakingInfo from '../../components/staking/info/StakingInfo';
import type { InjectedProps } from '../../types/injectedPropsType';

const messages = defineMessages({
  learnMoreLinkUrl: {
    id: 'staking.info.learnMore.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Learn more" link URL in the staking info page',
  },
});

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingInfoPage extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  handleLearnMoreClick = (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const learnMoreLinkUrl = intl.formatMessage(messages.learnMoreLinkUrl);
    this.props.stores.app.openExternalLink(learnMoreLinkUrl);
  };

  render() {
    const {
      stores: { networkStatus },
    } = this.props;

    if (networkStatus.isIncentivizedTestnet) {
      return <Redirect to={ROUTES.STAKING.DELEGATION_CENTER} />;
    }

    return (
      <StakingInfo
        percentage={this.props.stores.staking.decentralizationProgress}
        onLearnMoreClick={this.handleLearnMoreClick}
      />
    );
  }
}
