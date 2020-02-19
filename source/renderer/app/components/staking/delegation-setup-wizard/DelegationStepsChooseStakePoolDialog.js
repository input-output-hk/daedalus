// @flow
import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { find, get } from 'lodash';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import { StakePoolsList } from '../stake-pools/StakePoolsList';
import { StakePoolsSearch } from '../stake-pools/StakePoolsSearch';
import { getFilteredStakePoolsList } from '../stake-pools/helpers';
import BackToTopButton from '../../widgets/BackToTopButton';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsChooseStakePoolDialog.scss';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import questionmarkImage from '../../../assets/images/questionmark.inline.svg';
import { getColorFromRange } from '../../../utils/colors';
import Wallet from '../../../domains/Wallet';

import StakePool from '../../../domains/StakePool';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.title',
    defaultMessage: '!!!Delegate wallet',
    description:
      'Title "Delegate wallet" on the delegation setup "choose stake pool" dialog.',
  },
  description: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.description',
    defaultMessage: '!!!Currently selected stake pool',
    description:
      'Description on the delegation setup "choose stake pool" dialog.',
  },
  selectStakePoolLabel: {
    id:
      'staking.delegationSetup.chooseStakePool.step.dialog.selectStakePoolLabel',
    defaultMessage:
      '!!!Select a stake pool to delegate to for <span>{selectedWalletName}<span>  wallet.',
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
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.recentPoolsLabel',
    defaultMessage: '!!!Pick one of your recent stake pool choices:',
    description:
      'Recent "Pool" choice section label on the delegation setup "choose stake pool" dialog.',
  },
  searchInputLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.searchInput.label',
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
      'Step indicator labe on the delegation setup "choose wallet" step dialog.',
  },
});

type Props = {
  stepsList: Array<string>,
  recentStakePools: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  selectedWallet: ?Wallet,
  onOpenExternalLink: Function,
  getPledgeAddressUrl: Function,
  currentTheme: string,
  selectedPool: ?StakePool,
  onClose: Function,
  onBack: Function,
  onSelectPool: Function,
};

type State = {
  searchValue: string,
  selectedList?: ?string,
  selectedPoolId: ?number,
};

export default class DelegationStepsChooseStakePoolDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    searchValue: '',
    selectedList: null,
    selectedPoolId: get(this.props, ['selectedPool', 'id'], null),
  };

  handleSearch = (searchValue: string) => this.setState({ searchValue });
  handleClearSearch = () => this.setState({ searchValue: '' });

  handleSelect = (selectedPoolId: number) => {
    this.setState({ selectedPoolId });
  };

  handleSetListActive = (selectedList: string) => {
    this.setState({ selectedList });
  };

  onAcceptPool = () => {
    const { selectedPoolId } = this.state;
    this.props.onSelectPool(selectedPoolId);
  };

  render() {
    const { intl } = this.context;
    const {
      stepsList,
      recentStakePools,
      stakePoolsList,
      onOpenExternalLink,
      getPledgeAddressUrl,
      currentTheme,
      selectedWallet,
      onClose,
      onBack,
    } = this.props;
    const { searchValue, selectedList, selectedPoolId } = this.state;
    const selectedWalletName = get(selectedWallet, 'name');
    const selectedPool = find(
      stakePoolsList,
      stakePool => stakePool.id === selectedPoolId
    );
    const lastDelegatedStakePoolId = get(
      selectedWallet,
      'lastDelegationStakePoolId',
      null
    );
    const delegatedStakePoolId = get(
      selectedWallet,
      'delegatedStakePoolId',
      null
    );

    const activeStakePoolId = lastDelegatedStakePoolId || delegatedStakePoolId;

    const selectedPoolTicker = get(selectedPool, 'ticker');
    const canSubmit =
      !activeStakePoolId || activeStakePoolId !== selectedPoolId;

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: this.onAcceptPool,
        primary: true,
        disabled: !selectedPoolId || !canSubmit,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsChooseStakePoolDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);

    const selectedPoolBlock = () => {
      const selectedPoolBlockClasses = classNames([
        selectedPool
          ? styles.selectedPoolBlock
          : styles.selectPoolBlockPlaceholder,
        selectedPool && !canSubmit ? styles.alreadyDelegated : null,
      ]);

      const selectedPoolImageClasses = classNames([
        selectedPool ? styles.checkmarkImage : styles.questionmarkImage,
      ]);

      const wrapperClasses = classNames([
        selectedPool ? styles.checkmarkWrapper : styles.questionmarkWrapper,
      ]);

      const rankColor = selectedPool
        ? getColorFromRange(selectedPool.ranking, stakePoolsList.length)
        : 'transparent';

      return (
        <div
          role="presentation"
          className={selectedPoolBlockClasses}
          style={{
            background: rankColor,
          }}
        >
          {selectedPoolTicker && (
            <div className={styles.ticker}>{selectedPoolTicker}</div>
          )}
          <div className={wrapperClasses}>
            <SVGInline
              svg={selectedPool ? checkmarkImage : questionmarkImage}
              className={selectedPoolImageClasses}
            />
          </div>
        </div>
      );
    };

    const selectionPoolLabel = () => {
      let label = '';
      // Label when selected wallet already delegating to selected stake pool
      if (
        selectedPoolId &&
        activeStakePoolId === delegatedStakePoolId &&
        delegatedStakePoolId === selectedPoolId
      ) {
        label = (
          <FormattedHTMLMessage
            {...messages.delegatedStakePoolLabel}
            values={{
              selectedWalletName,
              selectedPoolTicker,
            }}
          />
        );
      } else if (
        selectedPoolId &&
        activeStakePoolId === lastDelegatedStakePoolId &&
        lastDelegatedStakePoolId === selectedPoolId
      ) {
        label = (
          <FormattedHTMLMessage
            {...messages.delegatedStakePoolNextLabel}
            values={{
              selectedWalletName,
              selectedPoolTicker,
            }}
          />
        );
      } else if (selectedPoolId) {
        // Stake pool selected and selected wallet are not delegated to it
        label = (
          <FormattedHTMLMessage
            {...messages.selectedStakePoolLabel}
            values={{
              selectedWalletName,
              selectedPoolTicker,
            }}
          />
        );
      } else {
        // Stake pool not selected.
        label = (
          <FormattedHTMLMessage
            {...messages.selectStakePoolLabel}
            values={{
              selectedWalletName,
            }}
          />
        );
      }
      return label;
    };

    const stepsIndicatorLabel = (
      <FormattedMessage
        {...messages.stepIndicatorLabel}
        values={{
          currentStep: 2,
          totalSteps: stepsList.length,
        }}
      />
    );

    const filteredStakePoolsList: Array<StakePool> = getFilteredStakePoolsList(
      stakePoolsList,
      searchValue
    );

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={stepsIndicatorLabel}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <BackToTopButton
          scrollableElementClassName="Dialog_content"
          buttonTopPosition={100}
          scrollTopToActivate={100}
        />

        <div className={commonStyles.delegationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={2}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={contentClassName}>
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>

          <div className={styles.selectStakePoolWrapper}>
            {selectedPoolBlock()}

            <p className={styles.selectStakePoolLabel}>
              {selectionPoolLabel()}
            </p>
          </div>

          <div className={styles.recentStakePoolsWrapper}>
            <p className={styles.recentStakePoolsLabel}>
              <FormattedMessage {...messages.recentPoolsLabel} values={{}} />
            </p>
            <StakePoolsList
              listName="recentStakePools"
              stakePoolsList={recentStakePools}
              onOpenExternalLink={onOpenExternalLink}
              getPledgeAddressUrl={getPledgeAddressUrl}
              currentTheme={currentTheme}
              isListActive={selectedList === 'recentStakePools'}
              setListActive={this.handleSetListActive}
              containerClassName="Dialog_content"
              onSelect={this.handleSelect}
              selectedPoolId={selectedPoolId}
              numberOfStakePools={stakePoolsList.length}
              disabledStakePoolId={activeStakePoolId}
              showSelected
              highlightOnHover
            />
          </div>

          <div className={styles.searchStakePoolsWrapper}>
            <StakePoolsSearch
              search={searchValue}
              label={intl.formatMessage(messages.searchInputLabel)}
              placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
              onSearch={this.handleSearch}
              onClearSearch={this.handleClearSearch}
              scrollableElementClassName="Dialog_content"
              disabledStakePoolId={activeStakePoolId}
            />
          </div>

          <div className={styles.stakePoolsListWrapper}>
            <StakePoolsList
              listName="selectedIndexList"
              stakePoolsList={filteredStakePoolsList}
              onOpenExternalLink={onOpenExternalLink}
              getPledgeAddressUrl={getPledgeAddressUrl}
              currentTheme={currentTheme}
              isListActive={selectedList === 'selectedIndexList'}
              setListActive={this.handleSetListActive}
              onSelect={this.handleSelect}
              selectedPoolId={selectedPoolId}
              containerClassName="Dialog_content"
              numberOfStakePools={stakePoolsList.length}
              disabledStakePoolId={activeStakePoolId}
              showSelected
              highlightOnHover
            />
          </div>
        </div>
      </Dialog>
    );
  }
}
