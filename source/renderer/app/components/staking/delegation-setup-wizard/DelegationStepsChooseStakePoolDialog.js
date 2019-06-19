// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { debounce } from 'lodash';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import { StakePoolsList } from '../stake-pools/StakePoolsList';
import { StakePoolsSearch } from '../stake-pools/StakePoolsSearch';
import styles from './DelegationStepsChooseStakePoolDialog.scss';
import { rangeMap } from '../../../utils/rangeMap';
import selectedStakePoolPlaceholderImage from '../../../assets/images/stake-pool-placeholder.inline.svg';
import type { StakePool } from '../../../api/staking/types';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.steps.dialog.title',
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
   continueButtonLabel: {
    id: 'staking.delegationSetup.chooseStakePool.step.dialog.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the delegation setup "choose stake pool" dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.stepIndicatorLabel',
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
  search: string,
  filter: string,
  selectedList?: ?string,
  selectedIndex?: ?number,
  flipHorizontal: boolean,
  flipVertical: boolean,
};

const initialState = {
  selectedList: null,
  selectedIndex: null,
  flipHorizontal: false,
  flipVertical: false,
};

export default class DelegationStepsChooseStakePoolDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    window.addEventListener(
      'resize',
      debounce(this.handleClose, 200, { leading: true, trailing: false })
    );
  }

  state = {
    search: '',
    ...initialState,
  };

  getIndex = (ranking: number) => {
    console.debug('getIndex: ', ranking);
    return rangeMap(ranking, 1, this.props.stakePoolsList.length, 0, 99)
  };

  getIsSelected = (list: string, index: number) => {
    console.debug('getIsSelected: ', list, index);
    return list === this.state.selectedList && index === this.state.selectedIndex;
  };

  handleClose = () => this.setState({ ...initialState });

  handleSearch = (search: string) => this.setState({ search });

  handleHover = (
    selectedList: string,
    event: SyntheticMouseEvent<HTMLElement>,
    selectedIndex: number
  ) => {
    if (
      this.state.selectedList === selectedList &&
      this.state.selectedIndex === selectedIndex
    ) {
      return this.handleClose();
    }
    event.persist();
    if (event.target instanceof HTMLElement) {
      const targetElement =
        event.target.className === 'StakePool_content'
          ? event.target
          : event.target.parentNode;
      if (targetElement instanceof HTMLElement) {
        const { top, left } = targetElement.getBoundingClientRect();
        const flipHorizontal = left > window.innerWidth - window.innerWidth / 2;
        const flipVertical = top > window.innerHeight - window.innerHeight / 2;
        return this.setState({
          selectedList,
          selectedIndex,
          flipHorizontal,
          flipVertical,
        });
      }
    }
    return false;
  };

  handleSelect = (
    selectedList: string,
    event: SyntheticMouseEvent<HTMLElement>,
    selectedIndex: number
  ) => {
    console.debug('Select 22: ', {
      selectedList,
      event,
      selectedIndex,
    });
  }

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
    const { search, flipHorizontal, flipVertical } = this.state;

    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(messages.continueButtonLabel),
        onClick: onContinue,
        primary: true,
      },
    ];

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
            <SVGInline svg={selectedStakePoolPlaceholderImage} className={styles.placeholderImage} />
            <div className={styles.delegatedStakePoolsList}>
              <p className={styles.stakePoolsDelegatingListLabel}>
                Stake pools you are already delegating to:
              </p>
              <StakePoolsList
                listName="stakePoolsDelegatingList"
                flipHorizontal={flipHorizontal}
                flipVertical={flipVertical}
                stakePoolsList={stakePoolsDelegatingList}
                onOpenExternalLink={onOpenExternalLink}
                currentTheme={currentTheme}
                getIsSelected={this.getIsSelected}
                onClose={this.handleClose}
                onHover={this.handleHover}
                getIndex={this.getIndex}
              />
            </div>
          </div>

          <div className={styles.searchStakePoolsWrapper}>
            <StakePoolsSearch
              search={search}
              onSearch={this.handleSearch}
              registerSearchInput={searchInput => {
                this.searchInput = searchInput;
              }}
            />
          </div>

          <div className={styles.stakePoolsListWrapper}>
            <StakePoolsList
              listName="stakePoolsList"
              stakePoolsList={stakePoolsList}
              onOpenExternalLink={onOpenExternalLink}
              currentTheme={currentTheme}
              getIsSelected={this.getIsSelected}
              onClose={this.handleClose}
              onHover={this.handleHover}
              onClick={this.handleSelect}
              getIndex={this.getIndex}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}
