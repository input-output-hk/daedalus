import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationContainer from './dialogs/redeem-itn-rewards/Step1ConfigurationContainer';
import ConfirmationContainer from './dialogs/redeem-itn-rewards/Step2ConfirmationContainer';
import ResultContainer from './dialogs/redeem-itn-rewards/Step3ResultContainer';
import NoWalletsContainer from './dialogs/redeem-itn-rewards/NoWalletsContainer';
import RedemptionUnavailableContainer from './dialogs/redeem-itn-rewards/RedemptionUnavailableContainer';
import LoadingOverlay from '../../components/staking/redeem-itn-rewards/LoadingOverlay';
import { REDEEM_ITN_REWARDS_STEPS } from '../../config/stakingConfig';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class RedeemItnRewardsContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  get containers() {
    return {
      configuration: ConfigurationContainer,
      confirmation: ConfirmationContainer,
      result: ResultContainer,
    };
  }

  render() {
    const { stores, actions } = this.props;
    const { allWallets } = stores.wallets;
    const {
      redeemStep,
      isSubmittingReedem,
      isCalculatingReedemFees,
    } = stores.staking;
    const { isSynced } = stores.networkStatus;
    const { onRedeemStart, closeRedeemDialog } = actions.staking;
    if (!redeemStep) return null;
    if (!isSynced && redeemStep === REDEEM_ITN_REWARDS_STEPS.CONFIGURATION)
      return (
        <RedemptionUnavailableContainer onClose={closeRedeemDialog.trigger} />
      );
    if (!allWallets.length)
      return <NoWalletsContainer onClose={closeRedeemDialog.trigger} />;
    const CurrentContainer = this.containers[redeemStep];
    return (
      <>
        {(isSubmittingReedem || isCalculatingReedemFees) && <LoadingOverlay />}
        <CurrentContainer
          onBack={onRedeemStart.trigger}
          onClose={closeRedeemDialog.trigger}
        />
      </>
    );
  }
}

export default RedeemItnRewardsContainer;
