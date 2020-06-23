// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationContainer from './dialogs/redeem-itn-rewards/Step1ConfigurationContainer';
import ConfirmationContainer from './dialogs/redeem-itn-rewards/Step2ConfirmationContainer';
import ResultContainer from './dialogs/redeem-itn-rewards/Step3ResultContainer';
import type { InjectedProps } from '../../types/injectedPropsType';
import { REDEEM_ITN_REWARDS_STEPS } from '../../config/stakingConfig';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class RedeemItnRewardsContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  get containers() {
    return {
      configuration: ConfigurationContainer,
      confirmation: ConfirmationContainer,
      result: ResultContainer,
    };
  }

  get nextStep() {
    return {
      configuration: REDEEM_ITN_REWARDS_STEPS.CONFIRMATION,
      confirmation: REDEEM_ITN_REWARDS_STEPS.RESULT,
      result: REDEEM_ITN_REWARDS_STEPS.RESULT,
    };
  }

  get prevStep() {
    return {
      configuration: REDEEM_ITN_REWARDS_STEPS.CONFIRMATION,
      confirmation: REDEEM_ITN_REWARDS_STEPS.RESULT,
      result: REDEEM_ITN_REWARDS_STEPS.RESULT,
    };
  }

  render() {
    const { stores, actions } = this.props;
    const { redeemStep } = stores.staking;
    const { goToRedeemStep, closeRedeemStep } = actions.staking;
    if (!redeemStep) return null;
    const CurrentContainer = this.containers[redeemStep];
    const nextStep = this.nextStep[redeemStep];
    const onContinue = () => {
      console.log('CONTINUE');
      console.log('nextStep', nextStep);
      goToRedeemStep.trigger({ step: nextStep });
    };
    const onClose = closeRedeemStep.trigger;
    return (
      <Fragment>
        <CurrentContainer onContinue={onContinue} onClose={onClose} />
      </Fragment>
    );
  }
}
