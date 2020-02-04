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
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './StakePoolTooltip.scss';
import StakePool from '../../../domains/StakePool';
import closeCross from '../../../assets/images/close-cross.inline.svg';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import { formattedWalletAmount, shortNumber } from '../../../utils/formatters';
import { rangeMap } from '../../../utils/rangeMap';
import {
  THUMBNAIL_HEIGHT,
  THUMBNAIL_OFFSET_WIDTH,
  ARROW_WIDTH,
  ARROW_HEIGHT,
  ARROW_OFFSET,
  TOOLTIP_DELTA,
  TOOLTIP_MAX_HEIGHT,
  TOOLTIP_WIDTH,
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
  costPerEpoch: {
    id: 'staking.stakePools.tooltip.costPerEpoch',
    defaultMessage: '!!!Cost per epoch:',
    description: '"Cost per epoch" for the Stake Pools Tooltip page.',
  },
  performance: {
    id: 'staking.stakePools.tooltip.performance',
    defaultMessage: '!!!Performance:',
    description: '"Performance" for the Stake Pools Tooltip page.',
  },
  producedBlocks: {
    id: 'staking.stakePools.tooltip.producedBlocks',
    defaultMessage: '!!!Produced blocks:',
    description: '"Produced blocks" for the Stake Pools Tooltip page.',
  },
  retirement: {
    id: 'staking.stakePools.tooltip.retirement',
    defaultMessage: '!!!Retirement in {retirementFromNow}',
    description: '"Retirement" for the Stake Pools Tooltip page.',
  },
  saturation: {
    id: 'staking.stakePools.tooltip.saturation',
    defaultMessage: '!!!Saturation:',
    description: '"Saturation" for the Stake Pools Tooltip page.',
  },
  desirability: {
    id: 'staking.stakePools.tooltip.desirability',
    defaultMessage: '!!!Desirability:',
    description: '"Desirability" for the Stake Pools Tooltip page.',
  },
  // cost: {
  //  id: 'staking.stakePools.tooltip.cost',
  //  defaultMessage: '!!!Operating Costs:',
  //  description: 'Cost" for the Stake Pools Tooltip page.',
  // },
  // pledge: {
  //   id: 'staking.stakePools.tooltip.pledge',
  //   defaultMessage: '!!!Pledge:',
  //   description: '"Pledge" for the Stake Pools Tooltip page.',
  // },
  pledgeAddressLabel: {
    id: 'staking.stakePools.tooltip.pledgeAddressLabel',
    defaultMessage: '!!!Pledge address',
    description: '"pledgeAddressLabel" for the Stake Pools Tooltip page.',
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
  getPledgeAddressUrl: Function,
  onSelect?: Function,
  showWithSelectButton?: boolean,
  top: number,
  left: number,
  color: string,
  containerClassName: string,
};

type State = {
  tooltipPosition: 'top' | 'right' | 'bottom' | 'left',
  componentStyle: Object,
  arrowStyle: Object,
  colorBandStyle: Object,
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
    const { top, left, containerClassName } = this.props;
    const container = document.querySelector(`.${containerClassName}`);
    if (container instanceof HTMLElement) {
      this.containerWidth = container.offsetWidth;
      this.containerHeight = container.offsetHeight;
    }
    this.getTooltipStyle(top, left);
  }

  tooltipClick: boolean = false;
  containerWidth: number = 0;
  containerHeight: number = 0;

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

  getTooltipStyle = (top: number, originalLeft: number) => {
    const { color } = this.props;

    const left = originalLeft + THUMBNAIL_OFFSET_WIDTH;

    const isTopHalf = top < this.containerHeight / 2;
    const isLeftHalf = left < this.containerWidth - this.containerWidth / 2;

    const tooltipPosition = this.getTooltipPosition(top, isLeftHalf);

    const {
      componentTop,
      componentBottom,
      componentLeft,
      arrowTop,
      arrowBottom,
      arrowLeft,
    } =
      tooltipPosition === 'top' || tooltipPosition === 'bottom'
        ? this.getTopBottomPosition(left)
        : this.getLeftRightPosition(top, isTopHalf);

    const componentStyle = this.getComponenStyle(
      tooltipPosition,
      componentTop,
      componentBottom,
      componentLeft
    );
    const arrowStyle = this.getArrowStyle(
      tooltipPosition,
      arrowTop,
      arrowBottom,
      arrowLeft
    );
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

  getTopBottomPosition = (left: number) => {
    const paddingOffset = rangeMap(
      left,
      THUMBNAIL_OFFSET_WIDTH,
      this.containerWidth - THUMBNAIL_OFFSET_WIDTH,
      -(THUMBNAIL_OFFSET_WIDTH / 2),
      THUMBNAIL_OFFSET_WIDTH / 2
    );

    const componentLeft =
      -((TOOLTIP_WIDTH * left) / this.containerWidth) +
      THUMBNAIL_OFFSET_WIDTH +
      paddingOffset;
    const componentTop = THUMBNAIL_HEIGHT + ARROW_HEIGHT / 2;
    const componentBottom = THUMBNAIL_HEIGHT + ARROW_HEIGHT / 2;

    const arrowLeft = -componentLeft + THUMBNAIL_OFFSET_WIDTH - ARROW_OFFSET;
    const arrowTop = -(ARROW_WIDTH / 2);
    const arrowBottom = -(ARROW_WIDTH / 2);

    return {
      componentLeft,
      componentTop,
      componentBottom,
      arrowLeft,
      arrowTop,
      arrowBottom,
    };
  };

  getLeftRightPosition = (top: number, isTopHalf: boolean) => {
    const bottom = this.containerHeight - (top + THUMBNAIL_HEIGHT);

    const componentLeft = THUMBNAIL_HEIGHT;
    let componentTop = 'auto';
    let componentBottom = 'auto';
    let arrowTop = 'auto';
    let arrowBottom = 'auto';

    if (isTopHalf) {
      componentTop = -((TOOLTIP_MAX_HEIGHT * top) / this.containerHeight);
      arrowTop = -componentTop + ARROW_WIDTH / 2;
    } else {
      componentBottom = -((TOOLTIP_MAX_HEIGHT * bottom) / this.containerHeight);
      arrowBottom = -componentBottom + ARROW_WIDTH / 2;
    }

    const arrowLeft = -(ARROW_WIDTH / 2);

    return {
      componentTop,
      componentBottom,
      componentLeft,
      arrowTop,
      arrowBottom,
      arrowLeft,
    };
  };

  getTooltipPosition = (top: number, isLeftHalf: boolean) => {
    const ignoreTopBottom = false;
    if (!ignoreTopBottom) {
      if (top <= TOOLTIP_DELTA) {
        return 'bottom';
      }
      if (
        TOOLTIP_DELTA >=
        this.containerHeight - (top + (THUMBNAIL_HEIGHT - TOOLTIP_DELTA))
      ) {
        return 'top';
      }
    }
    if (!isLeftHalf) {
      return 'left';
    }
    return 'right';
  };

  getComponenStyle = (
    tooltipPosition: string,
    top: number | 'auto',
    bottom: number | 'auto',
    left: number,
    right: number = left
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
        bottom,
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
      bottom,
    };
  };

  getArrowStyle = (
    tooltipPosition: string,
    top: number | 'auto',
    bottom: number | 'auto',
    left: number,
    right: number = left
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
        bottom,
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
      bottom,
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
      getPledgeAddressUrl,
      onSelect,
      showWithSelectButton,
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
      ticker,
      homepage,
      ranking,
      controlledStake,
      performance,
      producedBlocks,
      retiring,
      pledgeAddress,
      cost,
      profitMargin,
      saturation,
      desirability,
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
    const retirementFromNow = retiring ? moment(retiring).fromNow(true) : '';

    const saturationBarClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);

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
          <div className={styles.ticker}>{ticker}</div>
          {retiring && (
            <div className={styles.retirement}>
              <FormattedMessage
                {...messages.retirement}
                values={{ retirementFromNow }}
              />
            </div>
          )}
          <div className={styles.description}>{description}</div>

          <Link
            onClick={() => onOpenExternalLink(homepage)}
            className={styles.homepage}
            label={homepage}
            skin={LinkSkin}
          />

          <dl className={styles.table}>
            <dt className={styles.saturationLabel}>
              {intl.formatMessage(messages.saturation)}
            </dt>
            <dd className={styles.saturationValue}>
              <span>
                <span className={saturationBarClassnames}>
                  <span
                    style={{
                      width: `${parseFloat(saturation.toFixed(2))}%`,
                    }}
                  />
                </span>
                {`${parseFloat(saturation.toFixed(2))}%`}
              </span>
            </dd>
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
            <dt>{intl.formatMessage(messages.desirability)}</dt>
            <dd className={styles.defaultColor}>
              <span>{desirability}</span>
            </dd>
            <dt>{intl.formatMessage(messages.controlledStake)}</dt>
            <dd className={styles.defaultColor}>
              <span>{formattedWalletAmount(controlledStake, true, false)}</span>
            </dd>
            <dt>{intl.formatMessage(messages.profitMargin)}</dt>
            <dd className={styles.profitMargin}>
              <span
                style={{
                  background: getColorFromRange(profitMargin, {
                    darken,
                    alpha,
                  }),
                }}
              >
                {`${parseFloat(profitMargin.toFixed(2))}%`}
              </span>
            </dd>
            <dt>{intl.formatMessage(messages.costPerEpoch)}</dt>
            <dd className={styles.cost}>
              <span
                style={{
                  background: getColorFromRange(profitMargin, {
                    darken,
                    alpha,
                  }),
                }}
              >
                {`${formattedWalletAmount(cost, true, false)}`}
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
                {parseFloat(performance.toFixed(2))}%
              </span>
            </dd>
            <dt>{intl.formatMessage(messages.producedBlocks)}</dt>
            <dd className={styles.defaultColor}>
              <span>{shortNumber(producedBlocks)}</span>
            </dd>
            {/* <dt>{intl.formatMessage(messages.cost)}</dt>
            <dd>
              <span
                style={{
                  background: getColorFromRange(shortNumber(cost), {
                    darken,
                    alpha,
                  }),
                }}
              >
                {formattedWalletAmount(shortNumber(cost))}
              </span>
            </dd> */}
          </dl>
          {/* <dt>{intl.formatMessage(messages.pledge)}</dt> */}
          {/* <dd>
              <span
                style={{
                  background: getColorFromRange(pledge, {
                    darken,
                    alpha,
                  }),
                }}
              >
                {formattedWalletAmount(pledge)}
              </span>
            </dd> */}
          <Link
            onClick={() =>
              onOpenExternalLink(getPledgeAddressUrl(pledgeAddress))
            }
            label={intl.formatMessage(messages.pledgeAddressLabel)}
            skin={LinkSkin}
          />
        </div>
        {onSelect && showWithSelectButton && (
          <Button
            label={intl.formatMessage(messages.delegateButton)}
            onClick={onSelect}
            skin={ButtonSkin}
          />
        )}
      </div>
    );
  }
}
