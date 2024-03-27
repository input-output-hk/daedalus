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
exports.Slider = void 0;
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const rc_slider_1 = __importDefault(require('rc-slider'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const formatters_1 = require('../../utils/formatters');
const Slider_scss_1 = __importDefault(require('./Slider.scss'));
exports.Slider = (0, mobx_react_1.observer)((props) => {
  const [initialValue, setInitialValue] = (0, react_1.useState)(null);
  const {
    showTooltip,
    minTooltip,
    maxTooltip,
    minDisplayValue,
    maxDisplayValue,
    displayValue,
    showRawValue,
    tooltipAppendTo,
    ...rest
  } = props;
  const { min, max, value } = rest;
  const valueMarkLeftPosition =
    max === min ? `0` : `${((value - min) / (max - min)) * 100}%`;
  const valueMarkStyle = {
    left: valueMarkLeftPosition,
  };
  const formattedValue = showRawValue
    ? displayValue || value
    : new bignumber_js_1.default(value).toFormat(0);
  return react_1.default.createElement(
    'div',
    { className: Slider_scss_1.default.component },
    react_1.default.createElement(
      'div',
      { className: Slider_scss_1.default.upperMarks },
      react_1.default.createElement(
        'div',
        { className: Slider_scss_1.default.minMark },
        showTooltip
          ? react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: minTooltip,
                appendTo: tooltipAppendTo,
                popperOptions: {
                  placement: 'top',
                  modifiers: [
                    {
                      name: 'flip',
                      options: {
                        fallbackPlacements: ['bottom'],
                      },
                    },
                  ],
                },
              },
              (0, formatters_1.shortNumber)(minDisplayValue || min)
            )
          : (0, formatters_1.shortNumber)(minDisplayValue || min)
      ),
      react_1.default.createElement(
        'div',
        { className: Slider_scss_1.default.maxMark },
        showTooltip
          ? react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: maxTooltip,
                appendTo: tooltipAppendTo,
                popperOptions: {
                  placement: 'top',
                  modifiers: [
                    {
                      name: 'flip',
                      options: {
                        fallbackPlacements: ['bottom', 'left'],
                      },
                    },
                  ],
                },
              },
              (0, formatters_1.shortNumber)(maxDisplayValue || max)
            )
          : (0, formatters_1.shortNumber)(maxDisplayValue || max)
      )
    ),
    react_1.default.createElement(rc_slider_1.default, {
      ...rest,
      onBeforeChange: (e) => {
        if (!initialValue) setInitialValue(e);
      },
      onChange: (e) => {
        rest.onChange(e);
      },
      onAfterChange: (e) => {
        if (e !== initialValue && !!rest.onAfterChange) {
          rest.onAfterChange(e);
        }
        setInitialValue(null);
      },
    }),
    react_1.default.createElement(
      'div',
      { className: Slider_scss_1.default.lowerMarks },
      react_1.default.createElement(
        'div',
        { className: Slider_scss_1.default.valueMark, style: valueMarkStyle },
        formattedValue
      )
    )
  );
});
//# sourceMappingURL=Slider.js.map
