import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

export const getMessages = () => {
  const messages: Record<string, ReactIntlMessage> = defineMessages({
    title: {
      id: 'staking.delegationSetup.chooseStakePool.step.dialog.title',
      defaultMessage: '!!!Choose a stake pool',
      description:
        'Title "Choose a stake pool" on the delegation setup "choose stake pool" dialog.',
    },
    description: {
      id: 'staking.delegationSetup.chooseStakePool.step.dialog.description',
      defaultMessage: '!!!Currently selected stake pool:',
      description:
        'Description on the delegation setup "choose stake pool" dialog.',
    },
    selectStakePoolLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.selectStakePoolLabel',
      defaultMessage:
        '!!!Select a stake pool to receive your delegated funds in the <span>{selectedWalletName}<span> wallet.',
      description:
        'Select / Selected pool section label on the delegation setup "choose stake pool" dialog.',
    },
    selectedStakePoolLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.selectedStakePoolLabel',
      defaultMessage:
        '!!!You have selected [{selectedPoolTicker}] stake pool to delegate to for <span>{selectedWalletName}</span> wallet.',
      description:
        '"Selected Pools" Selected pool label on the delegation setup "choose stake pool" dialog.',
    },
    selectedStakePoolLabelRetiring: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.selectedStakePoolLabelRetiring',
      defaultMessage:
        '!!!The [{selectedPoolTicker}] stake pool which you have selected to delegate your <span>{selectedWalletName}</span> wallet funds is about to retire.',
      description:
        '"Selected Pools" Selected pool label on the delegation setup "choose stake pool" dialog.',
    },
    delegatedStakePoolLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.delegatedStakePoolLabel',
      defaultMessage:
        '!!!You are already delegating <span>{selectedWalletName}</span> wallet to <span>[{selectedPoolTicker}]</span> stake pool. <span>If you wish to re-delegate your stake, please select a different pool.</span>',
      description:
        '"You are already delegating to stake pool" label on the delegation setup "choose stake pool" dialog.',
    },
    delegatedStakePoolNextLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.delegatedStakePoolNextLabel',
      defaultMessage:
        '!!!You are already pending delegation <span>{selectedWalletName}</span> wallet to <span>[{selectedPoolTicker}]</span> stake pool. <span>If you wish to re-delegate your stake, please select a different pool.</span>',
      description:
        '"You are already delegating to stake pool" label on the delegation setup "choose stake pool" dialog.',
    },
    recentPoolsLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.recentPoolsLabel',
      defaultMessage: '!!!Choose one of your recent stake pool choices:',
      description:
        'Recent "Pool" choice section label on the delegation setup "choose stake pool" dialog.',
    },
    searchInputLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.searchInput.label',
      defaultMessage:
        '!!!Or select a stake pool from the list of all available stake pools:',
      description:
        'Search "Pools" input label on the delegation setup "choose stake pool" dialog.',
    },
    searchInputPlaceholder: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.searchInput.placeholder',
      defaultMessage: '!!!Search stake pools',
      description:
        'Search "Pools" input placeholder on the delegation setup "choose stake pool" dialog.',
    },
    continueButtonLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.continueButtonLabel',
      defaultMessage: '!!!Continue',
      description:
        'Label for continue button on the delegation setup "choose stake pool" dialog.',
    },
    stepIndicatorLabel: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.stepIndicatorLabel',
      defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
      description:
        'Step indicator label on the delegation setup "choose wallet" step dialog.',
    },
    retiringPoolFooter: {
      id:
        'staking.delegationSetup.chooseStakePool.step.dialog.retiringPoolFooter',
      defaultMessage:
        '!!!The stake pool you have selected is about to be retired. If you continue the delegation process, you will need to delegate your stake to another pool at least one complete epoch before the current pool’s retirement date to avoid losing rewards.',
      description:
        'Retiring Pool Footer label on the delegation setup "choose wallet" step dialog.',
    },
  });
  messages.fieldIsRequired = globalMessages.fieldIsRequired;
  return messages;
};
