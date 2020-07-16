// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce } from 'lodash';
import classNames from 'classnames';
import FlipMove from 'react-flip-move';
import styles from './StakePoolsList.scss';
import StakePool from '../../../domains/StakePool';
import { ThumbPool } from '../widgets/ThumbPool';

type Props = {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  highlightOnHover?: boolean,
  onSelect?: Function,
  showWithSelectButton?: boolean,
  showSelected?: boolean,
  containerClassName: string,
  numberOfStakePools: number,
  selectedPoolId?: ?number,
  disabledStakePoolId?: ?string,
  /**
   *
   * If the parent component has more than one <StakePoolsList />
   * these 3 props need to be passed, as it's the parent who will control
   * which list is active and prevent multiple Tooltips to be displayed
   *
   */
  listName?: string,
  isListActive?: boolean,
  setListActive?: Function,
};

type State = {
  highlightedPoolId?: ?number,
};

const initialState = {
  highlightedPoolId: null,
};

@observer
export class StakePoolsList extends Component<Props, State> {
  static defaultProps = {
    isListActive: true,
    showWithSelectButton: false,
  };

  constructor(props: Props) {
    super(props);
    window.addEventListener('resize', this.handleResize);
  }

  state = {
    ...initialState,
  };

  componentWillUnmount() {
    window.removeEventListener('resize', this.handleClose);
  }

  handleResize = () =>
    debounce(this.handleClose, 200, { leading: true, trailing: false });

  searchInput: ?HTMLElement = null;

  getIsHighlighted = (id: string) =>
    this.props.isListActive !== false && id === this.state.highlightedPoolId;

  handleOpenThumbnail = (highlightedPoolId: number) => {
    const { isListActive, setListActive, listName } = this.props;
    if (isListActive === false && setListActive) setListActive(listName);
    return this.setState({
      highlightedPoolId,
    });
  };

  handleClose = () => {
    this.setState({
      ...initialState,
    });
  };

  handleSelect = (stakePoolId: number) => {
    const { onSelect } = this.props;
    const selectedPoolId =
      this.props.selectedPoolId === stakePoolId ? null : stakePoolId;
    if (onSelect) {
      onSelect(selectedPoolId);
    }
  };

  render() {
    const {
      currentTheme,
      highlightOnHover,
      onOpenExternalLink,
      showSelected,
      showWithSelectButton,
      stakePoolsList,
      selectedPoolId,
      containerClassName,
      numberOfStakePools,
      disabledStakePoolId,
      listName,
    } = this.props;

    const componentClasses = classNames([styles.component, listName]);

    return (
      <div className={componentClasses}>
        <FlipMove>
          {stakePoolsList.map(stakePool => {
            const isHighlighted = this.getIsHighlighted(stakePool.id);
            const isSelected =
              selectedPoolId && stakePool.id === selectedPoolId;

            return (
              <ThumbPool
                stakePool={stakePool}
                key={stakePool.id + stakePool.ranking}
                onOpenExternalLink={onOpenExternalLink}
                isHighlighted={isHighlighted}
                onClose={this.handleClose}
                onClick={!highlightOnHover && this.handleOpenThumbnail}
                onHover={highlightOnHover && this.handleOpenThumbnail}
                onSelect={this.handleSelect}
                showWithSelectButton={showWithSelectButton}
                currentTheme={currentTheme}
                isSelected={isSelected}
                showSelected={showSelected}
                containerClassName={containerClassName}
                numberOfStakePools={numberOfStakePools}
                disabledStakePoolId={disabledStakePoolId}
              />
            );
          })}
        </FlipMove>
      </div>
    );
  }
}
