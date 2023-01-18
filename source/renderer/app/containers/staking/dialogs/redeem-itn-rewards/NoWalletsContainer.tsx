import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import NoWalletsDialog from '../../../../components/staking/redeem-itn-rewards/NoWalletsDialog';
import { ROUTES } from '../../../../routes-config';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class NoWalletsContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, actions } = this.props;

    const onAddWallet = () => {
      actions.router.goToRoute.trigger({
        route: ROUTES.WALLETS.ADD,
      });
      onClose();
    };

    return <NoWalletsDialog onClose={onClose} onAddWallet={onAddWallet} />;
  }
}

export default NoWalletsContainer;
