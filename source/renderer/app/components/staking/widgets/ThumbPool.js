'use strict';
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
exports.ThumbPool = void 0;
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const PoolPopOver_1 = require('./PoolPopOver');
const ThumbPool_scss_1 = __importDefault(require('./ThumbPool.scss'));
const colors_1 = require('../../../utils/colors');
const ThumbSelectedPool_1 = __importDefault(require('./ThumbSelectedPool'));
const ThumbPoolContent_1 = __importDefault(require('./ThumbPoolContent'));
/**
 * Stake pool thumbnail component that uses the PoolPopOver
 * to show stake pool information on click (by default) or
 * highlightOnHover (configurable via prop).
 *
 * It also renders differently depending on the isSelected prop
 */
function ThumbPool(props) {
  const {
    isGridRewardsView,
    isSelected,
    numberOfRankedStakePools,
    stakePool,
  } = props;
  const { ranking, id } = stakePool;
  const color = (0, colors_1.getColorFromRange)(
    ranking,
    numberOfRankedStakePools
  );
  const isDisabled = props.disabledStakePoolId === id;
  const [isHighlighted, setIsHighlighted] = (0, react_1.useState)(false);
  const contentClassnames = (0, classnames_1.default)([
    ThumbPool_scss_1.default.content,
    isDisabled ? ThumbPool_scss_1.default.disabled : null,
    isHighlighted ? ThumbPool_scss_1.default.isHighlighted : null,
    props.highlightOnHover
      ? ThumbPool_scss_1.default.shouldHighlightOnHover
      : null,
  ]);
  const content = isSelected
    ? react_1.default.createElement(ThumbSelectedPool_1.default, {
        stakePool: stakePool,
        numberOfRankedStakePools: numberOfRankedStakePools,
      })
    : react_1.default.createElement(ThumbPoolContent_1.default, {
        stakePool: stakePool,
        isGridRewardsView: isGridRewardsView,
        numberOfRankedStakePools: numberOfRankedStakePools,
      });
  return react_1.default.createElement(
    'div',
    { className: ThumbPool_scss_1.default.component },
    react_1.default.createElement(
      PoolPopOver_1.PoolPopOver,
      {
        color: color,
        currentTheme: props.currentTheme,
        openOnHover: props.highlightOnHover,
        onClose: () => setIsHighlighted(false),
        onOpen: () => setIsHighlighted(true),
        onOpenExternalLink: props.onOpenExternalLink,
        openWithDelay: props.highlightWithDelay,
        onSelect: props.onSelect,
        stakePool: stakePool,
        containerClassName: props.containerClassName,
        numberOfRankedStakePools: numberOfRankedStakePools,
        showWithSelectButton: props.showWithSelectButton,
        isGridRewardsView: isGridRewardsView,
      },
      react_1.default.createElement(
        'div',
        {
          className: contentClassnames,
          onClick: () =>
            props.selectOnClick && props.onSelect && props.onSelect(id),
        },
        content
      )
    )
  );
}
exports.ThumbPool = ThumbPool;
//# sourceMappingURL=ThumbPool.js.map
