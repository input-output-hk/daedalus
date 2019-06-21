// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce } from 'lodash';
import styles from './StakePoolsList.scss';
import type { StakePool } from '../../../api/staking/types';
import { StakePoolThumbnail } from './StakePoolThumbnail';
import { rangeMap } from '../../../utils/rangeMap';

type Props = {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  onHover?: Function,
  onSelect?: Function,
  showWithSelectButton?: boolean,
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
  selectedIndex?: ?number,
  flipHorizontal: boolean,
  flipVertical: boolean,
};

const initialState = {
  selectedIndex: null,
  flipHorizontal: false,
  flipVertical: false,
};

@observer
export class StakePoolsList extends Component<Props, State> {
  static defaultProps = {
    isListActive: true,
    showWithSelectButton: false,
  };

  constructor(props: Props) {
    super(props);
    window.addEventListener(
      'resize',
      debounce(this.handleClose, 200, { leading: true, trailing: false })
    );
  }

  state = {
    ...initialState,
  };

  searchInput: ?HTMLElement = null;

  getIndex = (ranking: number) =>
    rangeMap(ranking, 1, this.props.stakePoolsList.length, 0, 99);

  getIsSelected = (index: number) => index === this.state.selectedIndex;

  handleOpenThumbnail = (
    event: SyntheticMouseEvent<HTMLElement>,
    selectedIndex: number
  ) => {
    const { isListActive, setListActive, listName } = this.props;
    if (isListActive === false && setListActive) setListActive(listName);
    if (this.state.selectedIndex === selectedIndex) {
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
          selectedIndex,
          flipHorizontal,
          flipVertical,
        });
      }
    }
    return false;
  };

  handleClose = () => this.setState({ ...initialState });

  render() {
    const {
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      isListActive,
      onHover,
      onSelect,
      showWithSelectButton,
    } = this.props;

    const { flipHorizontal, flipVertical } = this.state;

    return (
      <div className={styles.component}>
        {stakePoolsList.map(stakePool => {
          const index = this.getIndex(stakePool.ranking);
          const isSelected =
            this.getIsSelected(stakePool.ranking) && isListActive !== false;
          return (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              onOpenExternalLink={onOpenExternalLink}
              isSelected={isSelected}
              onClose={this.handleClose}
              onClick={!onHover && this.handleOpenThumbnail}
              onHover={onHover && this.handleOpenThumbnail}
              onSelect={onSelect}
              showWithSelectButton={showWithSelectButton}
              currentTheme={currentTheme}
              flipHorizontal={flipHorizontal}
              flipVertical={flipVertical}
              index={index}
            />
          );
        })}
      </div>
    );
  }
}
