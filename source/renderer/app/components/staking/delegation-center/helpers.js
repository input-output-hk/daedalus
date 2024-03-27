'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateFieldPanel = exports.with2Decimals = void 0;
const lodash_1 = require('lodash');
const react_1 = __importDefault(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const DelegationCenterHeader_scss_1 = __importDefault(
  require('./DelegationCenterHeader.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/delimet... Remove this comment to see the full error message
const delimeter_inline_svg_1 = __importDefault(
  require('../../../assets/images/delimeter.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/delimet... Remove this comment to see the full error message
const delimeter_slash_inline_svg_1 = __importDefault(
  require('../../../assets/images/delimeter-slash.inline.svg')
);
const EPOCH_MAX_LENGTH = 5;
const with2Decimals = (value) => {
  const formattedValue = value.toString().match(/^-?\d+(?:\.\d{0,2})?/);
  const result = (0, lodash_1.get)(formattedValue, 0, 0);
  return result;
};
exports.with2Decimals = with2Decimals;
const generateFieldPanel = (labels, values, index) => {
  const value = values[index];
  const includeSlashDelimeter = index === values.length - 2;
  const includeDotsDelimeter =
    !includeSlashDelimeter && index !== values.length - 1;
  const labelStr = labels[index];
  const valueStr = value.toString();
  let zeroValues = '';
  if (index === 1 && valueStr.length < values[index + 1].toString().length) {
    const zerosToAdd =
      index === 1
        ? parseInt(values[index + 1].toString().length, 10) -
          parseInt(valueStr.length, 10)
        : // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
          parseInt(EPOCH_MAX_LENGTH, 10) - parseInt(valueStr.length, 10);
    switch (zerosToAdd) {
      case 1:
        zeroValues = '0';
        break;
      case 2:
        zeroValues = '00';
        break;
      case 3:
        zeroValues = '000';
        break;
      case 4:
        zeroValues = '0000';
        break;
      default:
        break;
    }
  }
  return react_1.default.createElement(
    'div',
    { className: DelegationCenterHeader_scss_1.default.fieldPanel },
    react_1.default.createElement(
      'div',
      { className: DelegationCenterHeader_scss_1.default.left },
      react_1.default.createElement(
        'div',
        { className: DelegationCenterHeader_scss_1.default.fieldLabel },
        labelStr
      ),
      react_1.default.createElement(
        'div',
        { className: DelegationCenterHeader_scss_1.default.fieldValue },
        zeroValues && react_1.default.createElement('span', null, zeroValues),
        valueStr
      )
    ),
    includeDotsDelimeter &&
      react_1.default.createElement(
        'div',
        { className: DelegationCenterHeader_scss_1.default.right },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: delimeter_inline_svg_1.default,
          className: DelegationCenterHeader_scss_1.default.delimeterIcon,
        })
      ),
    includeSlashDelimeter &&
      react_1.default.createElement(
        'div',
        { className: DelegationCenterHeader_scss_1.default.right },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: delimeter_slash_inline_svg_1.default,
          className: DelegationCenterHeader_scss_1.default.delimeterSlashIcon,
        })
      )
  );
};
exports.generateFieldPanel = generateFieldPanel;
//# sourceMappingURL=helpers.js.map
