// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { debounce } from 'lodash';
import classNames from 'classnames';
import styles from './StakePoolsList.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { ThumbPool } from '../widgets/ThumbPool';

// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;

type Props = {
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  highlightOnHover?: boolean,
  onSelect?: Function,
  showWithSelectButton?: boolean,
  showSelected?: boolean,
  containerClassName: string,
  numberOfRankedStakePools: number,
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
  isPreloading: boolean,
};

const initialState = {
  highlightedPoolId: null,
  isPreloading: true,
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

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted) this.setState({ isPreloading: false });
    }, 0);
  }

  componentWillUnmount() {
    this._isMounted = false;
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
      isPreloading: false,
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
      numberOfRankedStakePools,
      disabledStakePoolId,
      listName,
    } = this.props;
    const { isPreloading } = this.state;
    const componentClasses = classNames([styles.component, listName]);

    if (stakePoolsList.length > PRELOADER_THRESHOLD && isPreloading)
      return (
        <div className={styles.preloadingBlockWrapper}>
          <LoadingSpinner big />
        </div>
      );

    return (
      <div className={componentClasses}>
        {stakePoolsList.map((stakePool) => {
          const isHighlighted = this.getIsHighlighted(stakePool.id);
          const isSelected = selectedPoolId && stakePool.id === selectedPoolId;

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
              numberOfRankedStakePools={numberOfRankedStakePools}
              disabledStakePoolId={disabledStakePoolId}
            />
          );
        })}
      </div>
    );
  }
}
