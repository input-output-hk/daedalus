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

const OFFSET_LEFT = 84;
const OFFSET_TOP = 135;
const THUMBNAIL_HEIGHT = 71;
const THUMBNAIL_WIDTH = 80;
const THUMBNAIL_OFFSET = THUMBNAIL_WIDTH / 2;
const ARROW_WIDTH = 22;
const ARROW_HEIGHT = 11;
const ARROW_OFFSET = ARROW_WIDTH / 2;
const TOOLTIP_DELTA = 20;
const TOOLTIP_MAX_HEIGHT = 337;
const TOOLTIP_WIDTH = 240;
const CONTAINER_MARGIN = 21;

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
    if (nextVisibility !== currentVisibility)
      this.getTooltipPosition(top, left);
  }

  componentDidMount() {
    const { top, left } = this.props;
    this.getTooltipPosition(top, left);
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

  getTooltipPosition = (originalTop: number, originalLeft: number) => {
    const { color } = this.props;

    const top = originalTop - OFFSET_TOP;
    const left =
      originalLeft - OFFSET_LEFT - CONTAINER_MARGIN + THUMBNAIL_OFFSET;

    let tooltipPosition = 'right';
    if (top <= TOOLTIP_DELTA) {
      tooltipPosition = 'bottom';
    } else if (
      TOOLTIP_DELTA >=
      window.innerHeight - (top + THUMBNAIL_HEIGHT + OFFSET_TOP)
    ) {
      tooltipPosition = 'top';
    } else if (left + OFFSET_LEFT > window.innerWidth - window.innerWidth / 2) {
      tooltipPosition = 'left';
    }

    let containerNode;
    let containerWidth;
    let componentLeft;
    let arrowLeft;
    let componentTop;
    let arrowTop;

    if (tooltipPosition === 'top' || tooltipPosition === 'bottom') {
      containerNode = document.querySelector('.StakePoolsList_component');
      containerWidth = containerNode.offsetWidth;
      const paddingOffset = rangeMap(
        left,
        THUMBNAIL_OFFSET,
        containerWidth - THUMBNAIL_OFFSET,
        -(THUMBNAIL_OFFSET / 2),
        THUMBNAIL_OFFSET / 2
      );
      componentLeft =
        -((TOOLTIP_WIDTH * left) / containerWidth) +
        THUMBNAIL_OFFSET +
        paddingOffset;
      arrowLeft = -componentLeft + THUMBNAIL_OFFSET - ARROW_OFFSET;
      componentTop = THUMBNAIL_HEIGHT + ARROW_HEIGHT;
    } else {
      componentTop = -((TOOLTIP_MAX_HEIGHT * top) / window.innerHeight);
      arrowTop = -componentTop;
      componentLeft = THUMBNAIL_WIDTH - 30;
    }

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

  getArrowStyle = (tooltipPosition: string, top: number, left: number) => {
    if (tooltipPosition === 'top')
      return {
        bottom: -22,
        left,
      };
    if (tooltipPosition === 'right')
      return {
        left: -22,
        top,
      };
    if (tooltipPosition === 'bottom')
      return {
        borderBottomColor: this.props.color,
        left,
        top: -22,
      };
    return {
      right: -22,
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
