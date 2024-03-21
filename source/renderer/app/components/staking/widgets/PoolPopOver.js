'use strict';
// @ts-nocheck
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.PoolPopOver = void 0;
const react_1 = __importStar(require('react'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const timingConfig_1 = require('../../../config/timingConfig');
const TooltipPool_1 = __importDefault(require('./TooltipPool'));
const PoolPopOver_scss_1 = __importDefault(require('./PoolPopOver.scss'));
/**
 * Stake pool tooltip component that can be wrapped around
 * any trigger component (e.g: our pool thumbs and pool tickers) by
 * passing them as children of this component.
 *
 * By default it opens on "click" events on the trigger components
 * but can also be configured to openOnHover=true.
 */
function PoolPopOver(props) {
  // Track hover state manually to optimize performance by lazy init pop overs
  const [isHovered, setIsHovered] = (0, react_1.useState)(false);
  // The ref passed to Tippy.js as trigger target
  const popOverTargetRef = (0, react_1.useRef)(null);
  const poolId = props.stakePool.id;
  const close = (isStillHovered = false) => {
    setIsHovered(isStillHovered);
    props.onClose?.();
  };
  return react_1.default.createElement(
    react_1.default.Fragment,
    null,
    react_1.default.createElement(
      'div',
      {
        className: PoolPopOver_scss_1.default.triggerTarget,
        onMouseEnter: () => setIsHovered(true),
        ref: popOverTargetRef,
      },
      props.children
    ),
    isHovered // Init the pop over only when the target is hovered
      ? react_1.default.createElement(PopOver_1.PopOver, {
          interactive: true,
          className: 'PoolPopOver',
          delay: props.openWithDelay
            ? timingConfig_1.STAKE_POOL_TOOLTIP_HOVER_WAIT
            : 0,
          duration: 0,
          appendTo: document.body,
          trigger: props.openOnHover ? 'mouseenter' : 'click',
          placement: 'auto',
          onShow: () => props.onOpen && props.onOpen(poolId),
          onHide: () => close(true),
          onClickOutside: close,
          themeVariables: {
            '--rp-pop-over-bg-color':
              'var(--theme-staking-stake-pool-tooltip-background-color)',
            '--rp-pop-over-box-shadow':
              '0 5px 20px 0 var(--theme-staking-stake-pool-tooltip-shadow-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-radius': '5px',
            '--rp-pop-over-border-style': 'solid',
            '--rp-pop-over-padding': 0,
          },
          reference: popOverTargetRef,
          content: react_1.default.createElement(TooltipPool_1.default, {
            color: props.color,
            containerClassName: props.containerClassName,
            currentTheme: props.currentTheme,
            numberOfRankedStakePools: props.numberOfRankedStakePools,
            onClose: close,
            onOpenExternalLink: props.onOpenExternalLink,
            onSelect: () => {
              close();
              props.onSelect?.(poolId);
            },
            showWithSelectButton: props.showWithSelectButton,
            stakePool: props.stakePool,
            isGridRewardsView: props.isGridRewardsView,
          }),
        })
      : null
  );
}
exports.PoolPopOver = PoolPopOver;
PoolPopOver.defaultProps = {
  openWithDelay: false,
};
//# sourceMappingURL=PoolPopOver.js.map
