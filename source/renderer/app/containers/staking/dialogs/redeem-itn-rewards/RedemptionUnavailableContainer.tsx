import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import RedemptionUnavailableDialog from '../../../../components/staking/redeem-itn-rewards/RedemptionUnavailableDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class RedemptionUnavailableContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { onClose, stores } = this.props;
    const { syncPercentage } = stores.networkStatus;
    return (
      <RedemptionUnavailableDialog
        syncPercentage={syncPercentage}
        onClose={onClose}
      />
    );
  }
}

export default RedemptionUnavailableContainer;
