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
exports.StakePoolsList = void 0;
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const react_1 = __importStar(require('react'));
const react_virtualized_1 = require('react-virtualized');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const StakePoolsList_scss_1 = __importDefault(require('./StakePoolsList.scss'));
const ThumbPool_1 = require('../widgets/ThumbPool');
// Maximum number of stake pools for which we do not need to use the preloading
const PRELOADER_THRESHOLD = 100;
const POOL_THUMB_SIZE = 80;
const POOL_THUMB_GRID_GAP = 10;
/**
 * Utility function to programmatically hide the active pop over
 * This is used to hide the pool tooltips on scrolling the list
 */
function hidePoolPopOver() {
  const popOver = document.querySelector('.PoolPopOver')?.parentElement;
  if (popOver) {
    popOver?._tippy.hide();
  }
}
exports.StakePoolsList = (0, mobx_react_1.observer)((props) => {
  const [isLoading, setIsLoading] = (0, react_1.useState)(true);
  (0, react_1.useEffect)(() => {
    // Feature: Hide pool pop overs if the list scroll container is scrolled
    // Note: do not use window here otherwise the pool description cannot be
    // scrolled anymore because it closes the pop over immediately.
    const scrollContainer = props.scrollElementRef
      ? // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
        props.scrollElementRef.current
      : null;
    if (scrollContainer !== null) {
      scrollContainer.addEventListener('scroll', hidePoolPopOver, true);
    }
    setTimeout(() => setIsLoading(false));
    return () => {
      if (scrollContainer !== null) {
        scrollContainer.removeEventListener('scroll', hidePoolPopOver);
      }
    };
  });
  if (props.stakePoolsList.length > PRELOADER_THRESHOLD && isLoading) {
    return react_1.default.createElement(
      'div',
      { className: StakePoolsList_scss_1.default.preloadingBlockWrapper },
      react_1.default.createElement(LoadingSpinner_1.default, { big: true })
    );
  }
  const stakePoolsCount = props.stakePoolsList.length;
  function rowRenderer(itemsPerRow, { index, key, style }) {
    const startIndex = itemsPerRow * index;
    const endIndex = startIndex + itemsPerRow;
    const stakePools = props.stakePoolsList.slice(startIndex, endIndex);
    const numberOfMissingRowItems = itemsPerRow - stakePools.length;
    return react_1.default.createElement(
      'div',
      { key: key, style: style },
      react_1.default.createElement(
        'div',
        { className: StakePoolsList_scss_1.default.tiles },
        stakePools.map((stakePool) =>
          react_1.default.createElement(ThumbPool_1.ThumbPool, {
            key: stakePool.id,
            stakePool: stakePool,
            onOpenExternalLink: props.onOpenExternalLink,
            highlightOnHover: props.highlightOnHover,
            highlightWithDelay: props.highlightWithDelay,
            showWithSelectButton: props.showWithSelectButton,
            onSelect: props.onSelect,
            selectOnClick: props.selectOnClick,
            isSelected: props.selectedPoolId === stakePool.id,
            currentTheme: props.currentTheme,
            containerClassName: props.containerClassName,
            numberOfRankedStakePools: props.numberOfRankedStakePools,
            disabledStakePoolId: props.disabledStakePoolId,
            isGridRewardsView: props.isGridRewardsView,
          })
        ),
        numberOfMissingRowItems > 0
          ? (0, lodash_1.times)(numberOfMissingRowItems, (i) =>
              react_1.default.createElement('div', {
                key: `${key}-${i}`,
                className: StakePoolsList_scss_1.default.rowFillerItem,
              })
            )
          : null
      )
    );
  }
  return react_1.default.createElement(
    react_virtualized_1.WindowScroller,
    {
      scrollElement:
        // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
        props.scrollElementRef && props.scrollElementRef.current
          ? // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
            props.scrollElementRef.current
          : window,
    },
    ({ height, scrollTop, registerChild }) =>
      react_1.default.createElement(
        react_virtualized_1.AutoSizer,
        { disableHeight: true },
        ({ width }) => {
          if (!stakePoolsCount || !width) {
            return null;
          }
          const itemsPerRow = Math.floor(
            width / (POOL_THUMB_SIZE + POOL_THUMB_GRID_GAP)
          );
          const rowCount = Math.ceil(stakePoolsCount / itemsPerRow);
          return react_1.default.createElement(
            'div',
            { ref: (el) => registerChild(el) },
            react_1.default.createElement(react_virtualized_1.List, {
              autoHeight: true,
              className: StakePoolsList_scss_1.default.list,
              width: width,
              height: height,
              scrollTop: scrollTop,
              rowHeight: POOL_THUMB_SIZE,
              rowCount: rowCount,
              rowRenderer: (args) => rowRenderer(itemsPerRow, args),
              overscanRowCount: 3,
            })
          );
        }
      )
  );
});
// @ts-ignore ts-migrate(2339) FIXME: Property 'defaultProps' does not exist on type '(p... Remove this comment to see the full error message
exports.StakePoolsList.defaultProps = {
  showWithSelectButton: false,
  highlightWithDelay: false,
};
//# sourceMappingURL=StakePoolsList.js.map
