// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { getNetworkExplorerUrlByType } from '../../utils/network';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakePoolsListPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleDelegate = (poolId: string) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({ dialog: DelegationSetupWizardDialog });
    updateDataForActiveDialog.trigger({
      data: { poolId },
    });
  };

  render() {
    const { uiDialogs, staking, app, profile } = this.props.stores;
    const { currentTheme, currentLocale, environment } = profile;
    const { stakePools, fetchingStakePoolsFailed, recentStakePools } = staking;
    const { network, rawNetwork } = environment;
    const getPledgeAddressUrl = (pledgeAddres: string) =>
      getNetworkExplorerUrlByType(
        'address',
        pledgeAddres,
        network,
        rawNetwork,
        currentLocale
      );
    const hasLoadedStakePools = stakePools.length > 0;

    return (
      <Fragment>
        <StakePools
          stakePoolsList={stakePools}
          stakePoolsDelegatingList={recentStakePools}
          onOpenExternalLink={app.openExternalLink}
          getPledgeAddressUrl={getPledgeAddressUrl}
          currentTheme={currentTheme}
          onDelegate={this.handleDelegate}
          isLoading={fetchingStakePoolsFailed || !hasLoadedStakePools}
        />
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}
