// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import { capitalize } from 'lodash';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './StakePoolTooltip.scss';
import type { StakePool } from '../../../api/staking/types';
import closeCross from '../../../assets/images/close-cross.inline.svg';
import { getColorFromRange } from '../../../utils/colors';
import { rangeMap } from '../../../utils/rangeMap';
import {
  OFFSET_LEFT,
  OFFSET_TOP,
  THUMBNAIL_HEIGHT,
  THUMBNAIL_OFFSET,
  ARROW_WIDTH,
  ARROW_HEIGHT,
  ARROW_OFFSET,
  TOOLTIP_DELTA,
  TOOLTIP_MAX_HEIGHT,
  TOOLTIP_WIDTH,
  CONTAINER_MARGIN,
} from '../../../config/stakingConfig';

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
    defaultMessage: '!!!Retirement in {retirementFromNow}',
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
  isVisible: boolean,
  currentTheme: string,
  onClick: Function,
  onOpenExternalLink: Function,
  top: number,
  left: number,
  color: string,
};

type State = {
  tooltipPosition: 'top' | 'right' | 'bottom' | 'left',
  componentStyle: {},
  arrowStyle: {},
  colorBandStyle: {},
};

@observer
export default class StakePoolTooltip extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    componentStyle: {},
    arrowStyle: {},
    colorBandStyle: {},
    tooltipPosition: 'right',
  };

  componentWillReceiveProps(nextProps: Props) {
    const { isVisible: nextVisibility, top, left } = nextProps;
    const { isVisible: currentVisibility } = this.props;
    if (nextVisibility !== currentVisibility) this.getTooltipStyle(top, left);
  }

  componentDidMount() {
    const { top, left } = this.props;
    this.getTooltipStyle(top, left);
  }

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

  getTopBottomPosition = (left: number) => {
    let containerWidth = 0;
    const containerNode: ?HTMLElement = document.querySelector(
      '.StakePoolsList_component'
    );
    if (containerNode && containerNode instanceof HTMLElement)
      containerWidth = containerNode.offsetWidth;

    const paddingOffset = rangeMap(
      left,
      THUMBNAIL_OFFSET,
      containerWidth - THUMBNAIL_OFFSET,
      -(THUMBNAIL_OFFSET / 2),
      THUMBNAIL_OFFSET / 2
    );

    const componentLeft =
      -((TOOLTIP_WIDTH * left) / containerWidth) +
      THUMBNAIL_OFFSET +
      paddingOffset;
    const componentTop = THUMBNAIL_HEIGHT + ARROW_HEIGHT;

    const arrowLeft = -componentLeft + THUMBNAIL_OFFSET - ARROW_OFFSET;
    const arrowTop = -ARROW_WIDTH;

    return {
      componentLeft,
      componentTop,
      arrowLeft,
      arrowTop,
    };
  };

  getLeftRightPosition = (top: number) => {
    const componentTop = -((TOOLTIP_MAX_HEIGHT * top) / window.innerHeight);
    const componentLeft = THUMBNAIL_HEIGHT;

    const arrowTop = -componentTop + ARROW_WIDTH;
    const arrowLeft = -ARROW_WIDTH;

    return {
      componentTop,
      componentLeft,
      arrowTop,
      arrowLeft,
    };
  };

  getTooltipPosition = (top: number, left: number) => {
    if (top <= TOOLTIP_DELTA) {
      return 'bottom';
    }
    if (
      TOOLTIP_DELTA >=
      window.innerHeight - (top + THUMBNAIL_HEIGHT + OFFSET_TOP)
    ) {
      return 'top';
    }
    if (left + OFFSET_LEFT > window.innerWidth - window.innerWidth / 2) {
      return 'left';
    }
    return 'right';
  };

  getTooltipStyle = (originalTop: number, originalLeft: number) => {
    const { color } = this.props;

    const top = originalTop - OFFSET_TOP;
    const left =
      originalLeft - OFFSET_LEFT - CONTAINER_MARGIN + THUMBNAIL_OFFSET;

    const tooltipPosition = this.getTooltipPosition(top, left);

    const { componentTop, componentLeft, arrowTop, arrowLeft } =
      tooltipPosition === 'top' || tooltipPosition === 'bottom'
        ? this.getTopBottomPosition(left)
        : this.getLeftRightPosition(top);

    const componentStyle = this.getComponenStyle(
      tooltipPosition,
      componentTop,
      componentLeft
    );
    const arrowStyle = this.getArrowStyle(tooltipPosition, arrowTop, arrowLeft);
    const colorBandStyle = {
      background: color,
    };

    this.setState({
      componentStyle,
      arrowStyle,
      colorBandStyle,
      tooltipPosition,
    });
  };

  getComponenStyle = (
    tooltipPosition: string,
    top: number,
    left: number,
    right: number = left,
    bottom: number = top
  ) => {
    if (tooltipPosition === 'top') {
      return {
        bottom,
        left,
      };
    }
    if (tooltipPosition === 'right') {
      return {
        left,
        top,
      };
    }
    if (tooltipPosition === 'bottom') {
      return {
        left,
        top,
      };
    }
    return {
      right,
      top,
    };
  };

  getArrowStyle = (
    tooltipPosition: string,
    top: number,
    left: number,
    right: number = left,
    bottom: number = top
  ) => {
    if (tooltipPosition === 'top')
      return {
        bottom,
        left,
      };
    if (tooltipPosition === 'right')
      return {
        left,
        top,
      };
    if (tooltipPosition === 'bottom')
      return {
        borderBottomColor: this.props.color,
        left,
        top,
      };
    return {
      right,
      top,
    };
  };

  render() {
    const { intl } = this.context;
    const {
      stakePool,
      isVisible,
      currentTheme,
      onClick,
      onOpenExternalLink,
    } = this.props;

    const {
      componentStyle,
      arrowStyle,
      colorBandStyle,
      tooltipPosition,
    } = this.state;

    const {
      name,
      description,
      slug,
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
    ]);

    const arrowClassnames = classnames([
      styles.arrow,
      styles[`tooltipPosition${capitalize(tooltipPosition)}`],
    ]);

    const darken = currentTheme === 'dark-blue' ? 1 : 0;
    const alpha = 0.3;
    const reverse = true;
    const retirementFromNow = retirement
      ? moment(retirement).fromNow(true)
      : '';

    return (
      <div
        className={componentClassnames}
        onClick={this.handleInnerClick}
        role="link"
        aria-hidden
        style={componentStyle}
      >
        <div className={styles.colorBand} style={colorBandStyle} />
        <div className={arrowClassnames} style={arrowStyle} />
        <div className={styles.container}>
          <h3 className={styles.name}>{name}</h3>
          <button className={styles.closeButton} onClick={onClick}>
            <SVGInline svg={closeCross} />
          </button>
          <div className={styles.slug}>{slug}</div>
          {retirement && (
            <div className={styles.retirement}>
              <FormattedMessage
                {...messages.retirement}
                values={{ retirementFromNow }}
              />
            </div>
          )}
          <div className={styles.description}>{description}</div>
          <button
            className={styles.url}
            onClick={() => onOpenExternalLink(url)}
          >
            {url}
          </button>
          <dl className={styles.table}>
            <dt>{intl.formatMessage(messages.ranking)}</dt>
            <dd className={styles.ranking}>
              <span
                style={{
                  background: getColorFromRange(ranking, { darken, alpha }),
                }}
              >
                {ranking}
              </span>
            </dd>
            <dt>{intl.formatMessage(messages.controlledStake)}</dt>
            <dd className={styles.controlledStake}>
              <span
                style={{
                  background: getColorFromRange(controlledStake, {
                    darken,
                    alpha,
                  }),
                }}
              >
                {controlledStake}%
              </span>
            </dd>
            <dt>{intl.formatMessage(messages.profitMargin)}</dt>
            <dd className={styles.profitMargin}>
              <span
                style={{
                  background: getColorFromRange(profitMargin, {
                    darken,
                    alpha,
                    reverse,
                  }),
                }}
              >
                {profitMargin}%
              </span>
            </dd>
            <dt>{intl.formatMessage(messages.performance)}</dt>
            <dd className={styles.performance}>
              <span
                style={{
                  background: getColorFromRange(performance, {
                    darken,
                    alpha,
                    reverse,
                  }),
                }}
              >
                {performance}%
              </span>
            </dd>
          </dl>
        </div>
        <Button
          label={intl.formatMessage(messages.delegateButton)}
          onClick={() => {}}
          skin={ButtonSkin}
        />
      </div>
    );
  }
}
