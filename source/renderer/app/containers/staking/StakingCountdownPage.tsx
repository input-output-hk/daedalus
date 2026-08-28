import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { intlShape } from 'react-intl';
import StakingCountdown from '../../components/staking/countdown/StakingCountdown';
import type { InjectedProps } from '../../types/injectedPropsType';
import { getSupportUrl } from '../../../../common/utils/reporting';

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
    this.props.stores.app.openExternalLink(getSupportUrl(intl.locale));
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
