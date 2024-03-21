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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const ThumbSelectedPool_scss_1 = __importDefault(
  require('./ThumbSelectedPool.scss')
);
const colors_1 = require('../../../utils/colors');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/check-w... Remove this comment to see the full error message
const check_w_inline_svg_1 = __importDefault(
  require('../../../assets/images/check-w.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
const questionmark_inline_svg_1 = __importDefault(
  require('../../../assets/images/questionmark.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock.i... Remove this comment to see the full error message
const clock_inline_svg_1 = __importDefault(
  require('../../../assets/images/clock.inline.svg')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
let ThumbSelectedPool = class ThumbSelectedPool extends react_1.Component {
  render() {
    const {
      stakePool,
      alreadyDelegated,
      numberOfRankedStakePools,
    } = this.props;
    const { ticker, retiring, ranking } = stakePool || {};
    const rankColor =
      stakePool && !retiring && stakingConfig_1.IS_RANKING_DATA_AVAILABLE
        ? (0, colors_1.getColorFromRange)(ranking, numberOfRankedStakePools)
        : null;
    const selectedPoolBlockStyle = rankColor
      ? {
          background: rankColor,
        }
      : {};
    const selectedPoolBlockClasses = (0, classnames_1.default)([
      ThumbSelectedPool_scss_1.default.component,
      stakePool
        ? ThumbSelectedPool_scss_1.default.selectedPoolBlock
        : ThumbSelectedPool_scss_1.default.selectPoolBlockPlaceholder,
      retiring ? ThumbSelectedPool_scss_1.default.retiring : null,
      alreadyDelegated
        ? ThumbSelectedPool_scss_1.default.alreadyDelegated
        : null,
    ]);
    let icon = questionmark_inline_svg_1.default;
    if (retiring) {
      icon = clock_inline_svg_1.default;
    } else if (stakePool) {
      icon = check_w_inline_svg_1.default;
    }
    return react_1.default.createElement(
      'div',
      { className: selectedPoolBlockClasses, style: selectedPoolBlockStyle },
      ticker &&
        react_1.default.createElement(
          'div',
          { className: ThumbSelectedPool_scss_1.default.ticker },
          ticker
        ),
      react_1.default.createElement(
        'div',
        { className: ThumbSelectedPool_scss_1.default.icon },
        react_1.default.createElement(react_svg_inline_1.default, { svg: icon })
      )
    );
  }
};
ThumbSelectedPool = __decorate([mobx_react_1.observer], ThumbSelectedPool);
exports.default = ThumbSelectedPool;
//# sourceMappingURL=ThumbSelectedPool.js.map
