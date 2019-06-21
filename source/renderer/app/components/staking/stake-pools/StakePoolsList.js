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
  selectedPoolId?: ?number,
};

const initialState = {
  selectedPoolId: null,
};

@observer
export class StakePoolsList extends Component<Props, State> {
  static defaultProps = {
    isListActive: true,
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

  getIsSelected = (id: string) =>
    this.props.isListActive !== false && id === this.state.selectedPoolId;

  handleClick = (selectedPoolId: number) => {
    const { isListActive, setListActive, listName } = this.props;
    if (isListActive === false && setListActive) setListActive(listName);
    return this.setState({
      selectedPoolId,
    });
  };

  handleClose = () => this.setState({ ...initialState });

  render() {
    const { stakePoolsList, onOpenExternalLink, currentTheme } = this.props;

    return (
      <div className={styles.component}>
        {stakePoolsList.map(stakePool => {
          const index = this.getIndex(stakePool.ranking);
          const isSelected = this.getIsSelected(stakePool.id);
          return (
            <StakePoolThumbnail
              stakePool={stakePool}
              key={stakePool.id}
              onOpenExternalLink={onOpenExternalLink}
              isSelected={isSelected}
              onClose={this.handleClose}
              onClick={this.handleClick}
              currentTheme={currentTheme}
              index={index}
            />
          );
        })}
      </div>
    );
  }
}
