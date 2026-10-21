import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { find, get, take } from 'lodash';
import BigNumber from 'bignumber.js';
import { getSupportUrl } from '../../../../../common/utils/reporting';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import {
  MIN_DELEGATION_FUNDS,
  RECENT_STAKE_POOLS_COUNT,
  DELEGATION_ACTIONS,
} from '../../../config/stakingConfig';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import StakePool from '../../../domains/StakePool';
import type { DelegationCalculateFeeResponse } from '../../../api/staking/types';

const messages = defineMessages({
  delegationSetupStep1Label: {
    id: 'staking.delegationSetup.steps.step.1.label',
    defaultMessage: '!!!Wallet',
    description: 'Step 1 label text on delegation steps dialog.',
  },
  delegationSetupStep2Label: {
    id: 'staking.delegationSetup.steps.step.2.label',
    defaultMessage: '!!!Stake pool',
    description: 'Step 2 label text on delegation steps dialog.',
  },
  delegationSetupStep3Label: {
    id: 'staking.delegationSetup.steps.step.3.label',
    defaultMessage: '!!!Confirmation',
    description: 'Step 3 label text on delegation steps dialog.',
  },
});
type State = {
  activeStep: number;
  selectedWalletId: string;
  selectedPoolId: string;
  stakePoolJoinFee: DelegationCalculateFeeResponse | null;
};
type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class DelegationSetupWizardDialogContainer extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  _ownsHardwareTransaction = false;

  cancelHardwareTransaction = () => {
    if (!this._ownsHardwareTransaction) return;
    this._ownsHardwareTransaction = false;
    this.props.stores.hardwareWallets._resetTransaction({
      cancelDeviceAction: true,
    });
  };

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.cancelHardwareTransaction();
  }

  handleIsWalletAcceptable = (
    walletAmount?: BigNumber,
    // @ts-ignore ts-migrate(2322) FIXME: Type 'number' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    walletReward: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_DELEGATION_FUNDS)) &&
    !walletAmount.isEqualTo(walletReward);

  get selectedWalletId() {
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'walletId'],
      null
    );
  }

  get selectedPoolId() {
    return get(
      this.props,
      ['stores', 'uiDialogs', 'dataForActiveDialog', 'poolId'],
      null
    );
  }

  state = {
    activeStep: 0,
    selectedWalletId: this.selectedWalletId,
    selectedPoolId: this.selectedPoolId,
    stakePoolJoinFee: null,
  };
  STEPS_LIST = [
    this.context.intl.formatMessage(messages.delegationSetupStep1Label),
    this.context.intl.formatMessage(messages.delegationSetupStep2Label),
    this.context.intl.formatMessage(messages.delegationSetupStep3Label),
  ];
  handleDialogClose = () => {
    const { stores, actions } = this.props;
    stores.staking.joinStakePoolRequest.reset();
    actions.dialogs.closeActiveDialog.trigger();

    this.cancelHardwareTransaction();
  };
  handleContinue = () => {
    const { activeStep } = this.state;
    this.setState({
      activeStep: activeStep + 1,
    });
  };
  onBack = () => {
    const { activeStep } = this.state;
    this.cancelHardwareTransaction();
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.setState({
      activeStep: activeStep - 1,
    });
  };
  handleLearnMoreClick = (event: React.SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    this.props.stores.app.openExternalLink(getSupportUrl(intl.locale));
  };
  handleSelectWallet = (walletId: string) => {
    this.setState({
      selectedWalletId: walletId,
    });
    this.props.actions.staking.selectDelegationWallet.trigger(walletId);
    this.handleContinue();
  };
  handleChoosePool = (poolId: string) => {
    this.setState({
      selectedPoolId: poolId,
    });
  };
  handleConfirm = (
    spendingPassword: string | null | undefined,
    isHardwareWallet: boolean
  ) => {
    const { selectedPoolId, selectedWalletId } = this.state;
    this.props.stores.staking.joinStakePoolRequest.reset();
    this.props.actions.staking.joinStakePool.trigger({
      stakePoolId: selectedPoolId,
      walletId: selectedWalletId,
      passphrase: spendingPassword,
      isHardwareWallet,
    });
  };

  async _handleCalculateTransactionFee(poolId: string) {
    const { staking, hardwareWallets, wallets } = this.props.stores;
    const { selectedWalletId } = this.state;
    const wallet = wallets.getWalletById(selectedWalletId);
    if (!wallet) return;
    if (wallet.isHardwareWallet) this._ownsHardwareTransaction = true;
    const stakePoolJoinFee = wallet.isHardwareWallet
      ? await hardwareWallets.selectDelegationCoins({
          walletId: selectedWalletId,
          poolId,
          delegationAction: DELEGATION_ACTIONS.JOIN,
        })
      : await staking.calculateDelegationFee({ walletId: selectedWalletId });
    if (
      !this._isMounted ||
      (wallet.isHardwareWallet && !this._ownsHardwareTransaction) ||
      this.state.activeStep !== 3 ||
      this.state.selectedWalletId !== selectedWalletId ||
      this.state.selectedPoolId !== poolId
    )
      return;
    if (stakePoolJoinFee) this.setState({ stakePoolJoinFee });
    if (wallet.isHardwareWallet) {
      hardwareWallets.initiateTransaction({ walletId: selectedWalletId });
    }
  }

  handleSelectPool = async (pool: StakePool) => {
    const { selectedWalletId } = this.state;
    const wallet = this.props.stores.wallets.getWalletById(selectedWalletId);
    if (!wallet) return;
    this.setState({
      selectedPoolId: pool.id,
      activeStep: 3,
      stakePoolJoinFee: null,
    });
    if (!global.cip30Enabled) {
      try {
        await this._handleCalculateTransactionFee(pool.id);
      } catch {
        // Request errors remain available to the confirmation for retry/cancel.
      }
      return;
    }
    try {
      await this.props.stores.staking._joinStakePool({
        stakePoolId: pool.id,
        walletId: selectedWalletId,
        passphrase: '',
        isHardwareWallet: wallet.isHardwareWallet,
      });
      if (this._isMounted) this.setState({ activeStep: 4 });
    } catch {
      if (this._isMounted) this.setState({ activeStep: 2 });
    }
  };

  render() {
    const { activeStep, selectedWalletId, selectedPoolId } = this.state;
    const { app, staking, wallets, profile, networkStatus } = this.props.stores;
    const { futureEpoch } = networkStatus;
    const { currentTheme, currentLocale } = profile;
    const {
      stakePools,
      recentStakePools,
      getStakePoolById,
      maxDelegationFunds,
    } = staking;
    const futureEpochStartTime = get(futureEpoch, 'epochStart', 0);
    const selectedPool = find(stakePools, (pool) => pool.id === selectedPoolId);
    const selectedWallet = find(
      wallets.allWallets,
      (wallet) => wallet.id === selectedWalletId
    );
    const acceptableWallets = find(wallets.allWallets, ({ amount, reward }) =>
      this.handleIsWalletAcceptable(amount, reward)
    );

    return (
      <DelegationSetupWizardDialog
        wallets={wallets.allWallets}
        stepsList={this.STEPS_LIST}
        activeStep={activeStep}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        isDisabled={activeStep === 1 && !acceptableWallets}
        isWalletAcceptable={this.handleIsWalletAcceptable}
        maxDelegationFunds={maxDelegationFunds}
        selectedWallet={selectedWallet}
        selectedPool={selectedPool || null}
        stakePoolsList={stakePools}
        recentStakePools={take(recentStakePools, RECENT_STAKE_POOLS_COUNT)}
        futureEpochStartTime={futureEpochStartTime}
        currentLocale={currentLocale}
        onOpenExternalLink={app.openExternalLink}
        currentTheme={currentTheme}
        onClose={this.handleDialogClose}
        onContinue={this.handleContinue}
        onSelectWallet={this.handleSelectWallet}
        onSelectPool={this.handleSelectPool}
        onThumbPoolSelect={this.handleChoosePool}
        onBack={this.onBack}
        onLearnMoreClick={this.handleLearnMoreClick}
        getStakePoolById={getStakePoolById}
        stakePoolJoinFee={this.state.stakePoolJoinFee}
        onConfirm={this.handleConfirm}
        isSubmitting={
          staking.joinStakePoolRequest.isExecuting ||
          this.props.stores.hardwareWallets.sendMoneyRequest.isExecuting ||
          staking.isDelegationTransactionPending
        }
        error={
          staking.joinStakePoolRequest.error ||
          this.props.stores.hardwareWallets.sendMoneyRequest.error ||
          this.props.stores.hardwareWallets.selectCoinsRequest.error
        }
        hwDeviceStatus={this.props.stores.hardwareWallets.hwDeviceStatus}
        isTrezor={
          !!selectedWallet &&
          this.props.stores.hardwareWallets.checkIsTrezorByWalletId(
            selectedWallet.id
          )
        }
      />
    );
  }
}

export default DelegationSetupWizardDialogContainer;
