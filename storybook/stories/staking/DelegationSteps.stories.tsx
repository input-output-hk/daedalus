import React, { Component } from 'react';
import { BigNumber } from 'bignumber.js';
import moment from 'moment';
import { number, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { find } from 'lodash';
import DelegationStepsIntroDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseWalletDialog';
import DelegationStepsChooseStakePoolDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsChooseStakePoolDialog';
import DelegationStepsNotAvailableDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsNotAvailableDialog';
import DelegationStepsConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsConfirmationDialog';
import DelegationStepsSuccessDialog from '../../../source/renderer/app/components/staking/delegation-setup-wizard/DelegationStepsSuccessDialog';
import { MIN_DELEGATION_FUNDS } from '../../../source/renderer/app/config/stakingConfig';
import translations from '../../../source/renderer/app/i18n/translations';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../_support/utils';
import {
  WalletSyncStateStatuses,
  HwDeviceStatuses,
} from '../../../source/renderer/app/domains/Wallet';

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};
const WALLETS = [
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
  generateWallet('Wallet 1', '1000000000', assets, 0, STAKE_POOLS[0]),
  generateWallet(
    'Wallet 2 - Rewards Only',
    '500000000',
    assets,
    500000000,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
    STAKE_POOLS[100]
  ),
  generateWallet(
    'Wallet 3 - Min Amount - Rewards',
    '10',
    assets,
    10,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
    STAKE_POOLS[150]
  ),
  generateWallet(
    'Wallet 4 - Min Amount - No Reward',
    '0',
    assets,
    0,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
    STAKE_POOLS[290]
  ),
  generateWallet(
    'Wallet 5 - Restoring',
    '0',
    assets,
    0,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
    STAKE_POOLS[290],
    true,
    WalletSyncStateStatuses.RESTORING
  ),
];

const getDelegationWizardStepsList = (locale) => [
  translations[locale]['staking.delegationSetup.steps.step.1.label'],
  translations[locale]['staking.delegationSetup.steps.step.2.label'],
  translations[locale]['staking.delegationSetup.steps.step.3.label'],
];

type Props = {
  currentTheme: string;
  locale: string;
  isDisabled?: boolean;
  oversaturationPercentage: number;
};
type State = {
  currentStep: number;
};
const NUMBER_OF_STEPS = 6;
export class StakingDelegationSteps extends Component<Props, State> {
  state = {
    currentStep: 0,
  };

  get dialogs() {
    const stakePoolsList = STAKE_POOLS.slice(
      0,
      number('Pools', 100, {
        range: true,
        min: 37,
        max: 300,
        step: 1,
      })
    );

    if (this.props.isDisabled) {
      return [
        <DelegationStepsNotAvailableDialog
          key="DelegationStepsNotAvailableDialog"
          minDelegationFunds={MIN_DELEGATION_FUNDS}
          onClose={action('onClose')}
        />,
      ];
    }

    return [
      <DelegationStepsIntroDialog
        key="DelegationStepsIntroDialog"
        onClose={action('onClose')}
        onContinue={this.onContinue}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onLearnMoreClick={action('onLearnMoreClick')}
      />,
      <DelegationStepsChooseWalletDialog
        key="DelegationStepsChooseWalletDialog"
        numberOfStakePools={stakePoolsList.length}
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        onClose={action('onClose')}
        getStakePoolById={(poolId) =>
          find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
        }
        onSelectWallet={this.onContinue}
        onBack={this.onBack}
        wallets={WALLETS}
        minDelegationFunds={MIN_DELEGATION_FUNDS}
        selectedWalletId={null}
        isWalletAcceptable={(amount, reward = 0) =>
          amount.minus(reward).gte(MIN_DELEGATION_FUNDS)
        }
      />,
      <DelegationStepsChooseStakePoolDialog
        key="DelegationStepsChooseStakePoolDialog"
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        stakePoolsList={stakePoolsList}
        recentStakePools={[STAKE_POOLS[0], STAKE_POOLS[13], STAKE_POOLS[36]]}
        onOpenExternalLink={action('onOpenExternalLink')}
        currentTheme={this.props.currentTheme}
        onClose={action('onClose')}
        onBack={this.onBack}
        onContinue={this.onContinue}
        onSelectPool={() => {}}
        selectedPool={null}
        selectedWallet={WALLETS[0]}
        oversaturationPercentage={this.props.oversaturationPercentage}
      />,
      <DelegationStepsConfirmationDialog
        key="DelegationStepsConfirmationDialog"
        transactionFee={{
          fee: new BigNumber(0.172081),
          deposits: new BigNumber(2),
          depositsReclaimed: new BigNumber(0),
        }}
        stepsList={getDelegationWizardStepsList(this.props.locale)}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        selectedPool={STAKE_POOLS[0]}
        isSubmitting={false}
        selectedWallet={WALLETS[0]}
        onConfirm={this.onContinue}
        onClose={action('onClose')}
        onBack={this.onBack}
        error={null}
        isHardwareWallet={false}
        hwDeviceStatus={HwDeviceStatuses.CONNECTING}
        onExternalLinkClick={action('onOpenExternalLink')}
        isTrezor={boolean('isTrezor', false)}
        maxDelegationFunds={63000000}
        oversaturationPercentage={this.props.oversaturationPercentage}
      />,
      <DelegationStepsSuccessDialog
        key="DelegationStepsSuccessDialog"
        delegatedWallet={WALLETS[0]}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        delegatedStakePool={STAKE_POOLS[0]}
        slotLength={null}
        currentLocale={this.props.locale}
        futureEpochStartTime={moment().add(35, 'hour').toString()}
        onClose={this.onReset}
      />,
    ];
  }

  onContinue = () => {
    const { currentStep } = this.state;
    let nextStep = currentStep + 1;
    if (nextStep > NUMBER_OF_STEPS - 1) nextStep = 0;
    this.setState({
      currentStep: nextStep,
    });
  };
  onBack = () => {
    const { currentStep } = this.state;
    let nextStep = currentStep - 1;
    if (nextStep < NUMBER_OF_STEPS - 1) nextStep = 0;
    this.setState({
      currentStep: nextStep,
    });
  };
  onReset = () => {
    this.setState({
      currentStep: 0,
    });
  };

  render() {
    const { currentStep } = this.state;
    return this.dialogs[currentStep];
  }
}
