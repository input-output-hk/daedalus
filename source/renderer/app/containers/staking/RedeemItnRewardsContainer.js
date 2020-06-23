// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationContainer from './dialogs/redeem-itn-rewards/Step1ConfigurationContainer';
import ConfirmationContainer from './dialogs/redeem-itn-rewards/Step2ConfirmationContainer';
import ResultContainer from './dialogs/redeem-itn-rewards/Step3ResultContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

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

  get onContinue() {
    const {
      onConfigurationContinue,
      onConfirmationContinue,
      onResultContinue,
    } = this.props.actions.staking;
    return {
      configuration: onConfigurationContinue,
      confirmation: onConfirmationContinue,
      result: onResultContinue,
    };
  }

  render() {
    const { stores, actions } = this.props;
    const { redeemStep } = stores.staking;
    const {
      // goToNextRedeemStep,
      goToPrevRedeemStep,
      closeRedeemDialog,
    } = actions.staking;
    if (!redeemStep) return null;
    const onContinue = this.onContinue[redeemStep].trigger;
    const CurrentContainer = this.containers[redeemStep];
    return (
      <Fragment>
        <CurrentContainer
          onContinue={onContinue}
          onBack={goToPrevRedeemStep.trigger}
          onClose={closeRedeemDialog.trigger}
        />
      </Fragment>
    );
  }
}
