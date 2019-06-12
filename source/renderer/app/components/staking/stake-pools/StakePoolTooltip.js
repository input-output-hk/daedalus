// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import chroma from 'chroma-js';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './StakePoolTooltip.scss';
import { getHSLColor } from '../../../utils/colors';
import type { StakePool } from '../../../api/staking/types';
import closeCross from '../../../assets/images/close-cross.inline.svg';

const messages = defineMessages({
  ranking: {
    id: 'staking.stakePools.tooltip.ranking',
    defaultMessage: '!!!Rank:',
    description: '"" for the Stake Pools Tooltip page.',
  },
  controlledStake: {
    id: 'staking.stakePools.tooltip.controlledStake',
    defaultMessage: '!!!Controlled stake:',
    description: '"Controlled stake" for the Stake Pools Tooltip page.',
  },
  profitMargin: {
    id: 'staking.stakePools.tooltip.profitMargin',
    defaultMessage: '!!!Profit margin:',
    description: '"Profit margin" for the Stake Pools Tooltip page.',
  },
  performance: {
    id: 'staking.stakePools.tooltip.performance',
    defaultMessage: '!!!Performance:',
    description: '"Performance" for the Stake Pools Tooltip page.',
  },
  retirement: {
    id: 'staking.stakePools.tooltip.retirement',
    defaultMessage: '!!!Retirement:',
    description: '"Retirement" for the Stake Pools Tooltip page.',
  },
  delegateButton: {
    id: 'staking.stakePools.tooltip.delegateButton',
    defaultMessage: '!!!Delegate to this pool',
    description:
      '"Delegate to this pool" Button for the Stake Pools Tooltip page.',
  },
});

type Props = {
  stakePool: StakePool,
  index: number,
  isVisible: boolean,
  currentTheme: string,
  flipHorizontal: boolean,
  flipVertical: boolean,
  onClick: Function,
  onOpenExternalLink: Function,
};

@observer
export default class StakePoolTooltip extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  tooltipClick: boolean = false;

  componentWillMount() {
    window.document.addEventListener('click', this.handleOutterClick);
    window.addEventListener('keydown', this.handleInputKeyDown);
  }

  componentWillUnmount() {
    window.document.removeEventListener('click', this.handleOutterClick);
    window.removeEventListener('keydown', this.handleInputKeyDown);
  }

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.key === 'Escape') {
      this.props.onClick();
    }
  };

  handleOutterClick = () => {
    if (!this.tooltipClick) {
      this.props.onClick();
    } else {
      this.tooltipClick = false;
    }
  };

  handleInnerClick = () => {
    this.tooltipClick = true;
  };

  get color() {
    const { index } = this.props;
    return getHSLColor(index);
  }

  render() {
    const { intl } = this.context;
    const {
      stakePool,
      isVisible,
      currentTheme,
      flipHorizontal,
      flipVertical,
      onClick,
      onOpenExternalLink,
    } = this.props;

    const {
      id,
      name,
      description,
      url,
      ranking,
      controlledStake,
      profitMargin,
      performance,
      retirement,
    } = stakePool;

    const componentClassnames = classnames([
      styles.component,
      isVisible ? styles.isVisible : null,
      flipHorizontal ? styles.flipHorizontal : null,
      flipVertical ? styles.flipVertical : null,
    ]);

    const lighnessOffset = currentTheme === 'dark-blue' ? -20 : 0;

    return (
      <div
        className={componentClassnames}
        onClick={this.handleInnerClick}
        role="link"
        aria-hidden
      >
        <div
          className={styles.colorBand}
          style={{
            background: this.color,
          }}
        />
        <h3 className={styles.name}>{name}</h3>
        <button className={styles.closeButton} onClick={onClick}>
          <SVGInline svg={closeCross} />
        </button>
        <div className={styles.id}>{id}</div>
        <div className={styles.description}>{description}</div>
        <button className={styles.url} onClick={() => onOpenExternalLink(url)}>
          {url}
        </button>
        <dl className={styles.table}>
          <dt>{intl.formatMessage(messages.ranking)}</dt>
          <dd className={styles.ranking}>
            <span
              style={{
                background: chroma(
                  getHSLColor(ranking, { lighnessOffset })
                ).alpha(0.3),
              }}
            >
              {parseFloat(ranking).toFixed(2)}
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.controlledStake)}</dt>
          <dd className={styles.controlledStake}>
            <span
              style={{
                background: chroma(
                  getHSLColor(controlledStake, { lighnessOffset })
                ).alpha(0.3),
              }}
            >
              {controlledStake}%
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.profitMargin)}</dt>
          <dd className={styles.profitMargin}>
            <span
              style={{
                background: chroma(
                  getHSLColor(profitMargin, { lighnessOffset })
                ).alpha(0.3),
              }}
            >
              {profitMargin}%
            </span>
          </dd>
          <dt>{intl.formatMessage(messages.performance)}</dt>
          <dd className={styles.performance}>
            <span
              style={{
                background: chroma(
                  getHSLColor(performance, { lighnessOffset })
                ).alpha(0.3),
              }}
            >
              {performance}%
            </span>
          </dd>
          {retirement && (
            <Fragment>
              <dt>{intl.formatMessage(messages.retirement)}</dt>
              <dd className={styles.retirement}>
                <span>{moment(retirement).fromNow(true)}</span>
              </dd>
            </Fragment>
          )}
        </dl>
        <Button
          label={intl.formatMessage(messages.delegateButton)}
          onClick={() => {}}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}
