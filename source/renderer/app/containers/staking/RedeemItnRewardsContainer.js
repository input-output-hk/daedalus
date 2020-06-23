// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import ConfigurationContainer from './dialogs/redeem-itn-rewards/Step1ConfigurationContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class RedeemItnRewardsContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  get containers() {
    return {
      configuration: ConfigurationContainer,
      // confirmation: ConfirmationContainer,
      // success: SuccessContainer,
      // failure: FailureContainer,
    };
  }

  render() {
    const { stores, actions } = this.props;
    const { redeemStep } = stores.staking;
    const { restoreWalletClose, restoreWalletChangeStep } = actions.wallets;
    if (!redeemStep) return null;
    // const CurrentContainer = this.containers[redeemStep];
    const CurrentContainer = ConfigurationContainer;
    return (
      <Fragment>
        <CurrentContainer
          onContinue={() => restoreWalletChangeStep.trigger()}
          onBack={() => restoreWalletChangeStep.trigger(true)}
          onClose={() => restoreWalletClose.trigger()}
        />
      </Fragment>
    );
  }
}
