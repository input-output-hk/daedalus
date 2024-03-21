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
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock-c... Remove this comment to see the full error message
const clock_corner_inline_svg_1 = __importDefault(
  require('../../../assets/images/clock-corner.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
const no_data_dash_big_inline_svg_1 = __importDefault(
  require('../../../assets/images/no-data-dash-big.inline.svg')
);
const ThumbPoolContent_scss_1 = __importDefault(
  require('./ThumbPoolContent.scss')
);
const colors_1 = require('../../../utils/colors');
const stakingConfig_1 = require('../../../config/stakingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-sym... Remove this comment to see the full error message
const ada_symbol_inline_svg_1 = __importDefault(
  require('../../../assets/images/ada-symbol.inline.svg')
);
const formatters_1 = require('../../../utils/formatters');
let ThumbPoolContent = class ThumbPoolContent extends react_1.Component {
  formattedRewards = (potentialRewards) => {
    const potentialRewardsAsString = (0, formatters_1.formattedWalletAmount)(
      potentialRewards
    );
    let targetLength = 4;
    if (potentialRewardsAsString.includes('.')) {
      targetLength++;
    }
    if (potentialRewardsAsString.includes(',')) {
      targetLength++;
    }
    if (potentialRewardsAsString.includes(' ')) {
      targetLength++;
    }
    return potentialRewardsAsString.substring(0, targetLength);
  };
  render() {
    const {
      stakePool,
      numberOfRankedStakePools,
      isGridRewardsView,
    } = this.props;
    const {
      ranking,
      nonMyopicMemberRewards,
      ticker,
      retiring,
      saturation,
      potentialRewards,
    } = stakePool;
    const color = (0, colors_1.getColorFromRange)(
      ranking,
      numberOfRankedStakePools
    );
    const componentClassnames = (0, classnames_1.default)([
      ThumbPoolContent_scss_1.default.component,
      !stakingConfig_1.IS_SATURATION_DATA_AVAILABLE
        ? ThumbPoolContent_scss_1.default.hideSaturation
        : null,
    ]);
    const saturationClassnames = (0, classnames_1.default)([
      ThumbPoolContent_scss_1.default.saturationBar,
      ThumbPoolContent_scss_1.default[
        (0, colors_1.getSaturationColor)(saturation)
      ],
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClassnames },
      react_1.default.createElement(
        'div',
        { className: ThumbPoolContent_scss_1.default.ticker },
        ticker
      ),
      isGridRewardsView &&
        (stakingConfig_1.IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards
          ? react_1.default.createElement(
              'div',
              { className: ThumbPoolContent_scss_1.default.rewards },
              this.formattedRewards(potentialRewards),
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: ada_symbol_inline_svg_1.default,
                className: ThumbPoolContent_scss_1.default.adaIcon,
              })
            )
          : react_1.default.createElement(
              'div',
              { className: ThumbPoolContent_scss_1.default.noDataDash },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: no_data_dash_big_inline_svg_1.default,
              })
            )),
      !isGridRewardsView &&
        (stakingConfig_1.IS_RANKING_DATA_AVAILABLE
          ? react_1.default.createElement(
              'div',
              {
                className: ThumbPoolContent_scss_1.default.ranking,
                style: {
                  color,
                },
              },
              nonMyopicMemberRewards
                ? ranking
                : react_1.default.createElement(
                    react_1.default.Fragment,
                    null,
                    numberOfRankedStakePools + 1,
                    react_1.default.createElement('sup', null, '*')
                  )
            )
          : react_1.default.createElement(
              'div',
              { className: ThumbPoolContent_scss_1.default.noDataDash },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: no_data_dash_big_inline_svg_1.default,
              })
            )),
      stakingConfig_1.IS_SATURATION_DATA_AVAILABLE &&
        react_1.default.createElement(
          'div',
          { className: saturationClassnames },
          react_1.default.createElement('span', {
            style: {
              // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
              width: `${parseFloat(saturation).toFixed(2)}%`,
            },
          })
        ),
      stakingConfig_1.IS_RANKING_DATA_AVAILABLE
        ? react_1.default.createElement(
            react_1.default.Fragment,
            null,
            retiring &&
              react_1.default.createElement(
                'div',
                { className: ThumbPoolContent_scss_1.default.clock },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: clock_corner_inline_svg_1.default,
                  className: ThumbPoolContent_scss_1.default.clockIcon,
                })
              ),
            react_1.default.createElement('div', {
              className: ThumbPoolContent_scss_1.default.colorBand,
              style: {
                background: color,
              },
            })
          )
        : react_1.default.createElement('div', {
            className: ThumbPoolContent_scss_1.default.greyColorBand,
          })
    );
  }
};
ThumbPoolContent = __decorate([mobx_react_1.observer], ThumbPoolContent);
exports.default = ThumbPoolContent;
//# sourceMappingURL=ThumbPoolContent.js.map
