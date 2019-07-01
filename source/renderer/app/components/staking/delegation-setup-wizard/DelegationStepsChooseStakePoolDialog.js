// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { debounce, find } from 'lodash';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import { StakePoolsList } from '../stake-pools/StakePoolsList';
import { StakePoolsSearch } from '../stake-pools/StakePoolsSearch';
import styles from './DelegationStepsChooseStakePoolDialog.scss';
import { rangeMap } from '../../../utils/rangeMap';
import selectedStakePoolPlaceholderImage from '../../../assets/images/stake-pool-placeholder.inline.svg';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import type { StakePool } from '../../../api/staking/types';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.title',
    defaultMessage: '!!!Delegation Setup',
    description:
      'Title "Delegation Setup" on the delegation setup "choose stake pool" dialog.',
  },
  description: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.description',
    defaultMessage:
      '!!!Choose a stake pool to which you would like to delegate.',
    description: 'Description on the delegation setup "choose stake pool" dialog.',
  },
  delegatedPoolsLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.delegatedPoolsLabel',
    defaultMessage:
      '!!!Stake pools you are already delegating to:',
    description: '"Delegated Pools" section label on the delegation setup "choose stake pool" dialog.',
  },
  searchInputLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.searchInput.label',
    defaultMessage:
      '!!!Or search for a stake pool:',
    description: 'Search "Pools" input label on the delegation setup "choose stake pool" dialog.',
  },
  searchInputPlaceholder: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.searchInput.placeholder',
    defaultMessage:
      '!!!Search stake pools',
    description: 'Search "Pools" input placeholder on the delegation setup "choose stake pool" dialog.',
  },
   continueButtonLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "choose stake pool" dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator labe on the delegation setup "choose wallet" step dialog.',
  },
});

type Props = {
  stepsList: Array<string>,
  stakePoolsDelegatingList: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  onClose: Function,
  onContinue: Function,
  onBack: Function,
};

type State = {
  searchValue: string,
  selectedList?: ?string,
  flipHorizontal: boolean,
  flipVertical: boolean,
  selectedPoolId: ?number
};

const initialState = {
  selectedList: null,
  flipHorizontal: false,
  flipVertical: false,
};

export default class DelegationStepsChooseStakePoolDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    searchValue: '',
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  handleSearch = (searchValue: string) => this.setState({ searchValue });

  handleHover = (item) => {
    console.debug('Hovered: ', item);
  };

   handleSelect = (selectedPoolId) => {
    this.setState({ selectedPoolId });
  };

  handleSetListActive = (selectedList: string) => {
    this.setState({ selectedList });
  };

  handleDeselectStakePool = () => {
    this.setState({ selectedPoolId: null });
  };

  render() {
    const { intl } = this.context;
    const {
      stepsList,
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      onClose,
      onContinue,
      onBack,
    } = this.props;
    const {
      searchValue,
      flipHorizontal,
      flipVertical,
      selectedList,
      selectedPoolId,
    } = this.state;

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
      },
    ];

    const selectPoolPlaceholder = (
      <SVGInline
        svg={selectedStakePoolPlaceholderImage}
        className={styles.placeholderImage}
      />
    );

    const selectedPoolBlock = (selectedPoolId) => {
      const { stakePoolsList } = this.props;
      const selectedPool = find(stakePoolsList, (stakePools) => (stakePools.id === selectedPoolId));

      return (
        <div
          className={styles.selectedPoolBlock}
          onClick={this.handleDeselectStakePool}
        >
          <div className={styles.label}>{selectedPool.slug}</div>
          <div className={styles.checkmarkWrapper}>
            <SVGInline
              svg={checkmarkImage}
              className={styles.checkmarkImage}
            />
          </div>
        </div>
      );
    };

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsChooseStakePoolDialogWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={styles.delegationStepsIndicatorWrapper}>
          <p className={styles.stepIndicatorLabel}>
            <FormattedMessage
              {...messages.stepIndicatorLabel}
              values={{
                currentStep: 2,
                totalSteps: stepsList.length,
              }}
            />
          </p>
          <Stepper
            steps={stepsList}
            activeStep={2}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={styles.content}>
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>
          <div className={styles.delegatedStakePoolsWrapper}>
            {selectedPoolId ? selectedPoolBlock(selectedPoolId) : selectPoolPlaceholder}

            <div className={styles.delegatedStakePoolsList}>
              <p className={styles.stakePoolsDelegatingListLabel}>
                {intl.formatMessage(messages.delegatedPoolsLabel)}
              </p>
              <StakePoolsList
                listName="stakePoolsDelegatingList"
                flipHorizontal={flipHorizontal}
                flipVertical={flipVertical}
                stakePoolsList={stakePoolsDelegatingList}
                onOpenExternalLink={onOpenExternalLink}
                currentTheme={currentTheme}
                onHover={this.handleHover}
                isListActive={selectedList === 'stakePoolsDelegatingList'}
                setListActive={this.handleSetListActive}
              />
            </div>
          </div>

          <div className={styles.searchStakePoolsWrapper}>
            <StakePoolsSearch
              search={searchValue}
              label={intl.formatMessage(messages.searchInputLabel)}
              placeholder={intl.formatMessage(messages.searchInputPlaceholder)}
              onSearch={this.handleSearch}
              registerSearchInput={searchInput => {
                this.searchInput = searchInput;
              }}
            />
          </div>

          <div className={styles.stakePoolsListWrapper}>
            <StakePoolsList
              listName="selectedIndexList"
              stakePoolsList={stakePoolsList}
              onOpenExternalLink={onOpenExternalLink}
              currentTheme={currentTheme}
              onHover={this.handleHover}
              isListActive={selectedList === 'selectedIndexList'}
              setListActive={this.handleSetListActive}
              onSelect={this.handleSelect}
              selectedPoolId={selectedPoolId}
              showSelected
            />
          </div>
        </div>
      </Dialog>
    );
  }
}
