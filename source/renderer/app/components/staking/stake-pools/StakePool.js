// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock.inline.svg';
import styles from './StakePool.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePoolProps } from '../../../api/staking/types';
import StakePoolTooltip from './StakePoolTooltip';

type Props = {
  ...$Exact<StakePoolProps>,
  ranking: number,
  onOpenExternalLink: Function,
  onClick: Function,
  onClose: Function,
  isSelected: boolean,
};

@observer
export default class StakePool extends Component<Props> {
  get color() {
    return getHSLColor(this.props.ranking);
  }

  render() {
    const { index, id, retirement, isSelected, onClick, onClose } = this.props;

    const componentClassnames = classnames([
      styles.component,
      isSelected ? styles.isSelected : null,
    ]);

    return (
      <div className={componentClassnames}>
        <div
          className={styles.content}
          onClick={() => onClick(index)}
          role="link"
          aria-hidden
        >
          <div className={styles.id}>{id}</div>
          <div
            className={styles.index}
            style={{
              color: this.color,
            }}
          >
            {index}
          </div>
          {retirement && (
            <div className={styles.clock}>
              <SVGInline svg={clockIcon} className={styles.clockIcon} />
            </div>
          )}
          <div
            className={styles.colorBand}
            style={{
              background: this.color,
            }}
          />
        </div>
        <StakePoolTooltip
          {...this.props}
          className={styles.tooltip}
          visible={isSelected}
          onClick={onClose}
        />
      </div>
    );
  }
}
