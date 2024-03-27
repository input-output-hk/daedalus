'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getSaturationColor = exports.getColorFromRange = void 0;
const chroma_js_1 = __importDefault(require('chroma-js'));
const isNil_1 = __importDefault(require('lodash/isNil'));
const defaultRangeOptions = {
  colors: ['#1cca5b', '#fc602c'],
  numberOfItems: 99,
  darken: 0,
  brighten: 0,
  alpha: 1,
  reverse: false,
};
const getColorFromRange = (index, optionsOrNumberOfItems) => {
  let options = {};
  let { numberOfItems } = defaultRangeOptions;
  if (typeof optionsOrNumberOfItems === 'object') {
    options = optionsOrNumberOfItems;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'numberOfItems' does not exist on type '{... Remove this comment to see the full error message
    numberOfItems = options.numberOfItems || numberOfItems;
  } else if (typeof optionsOrNumberOfItems === 'number') {
    numberOfItems = optionsOrNumberOfItems;
  }
  const { colors, darken, brighten, alpha, reverse } = {
    ...defaultRangeOptions,
    ...options,
  };
  const domain = [0, numberOfItems];
  if (reverse) domain.reverse();
  const scale = chroma_js_1.default.scale(colors).domain(domain);
  if ((0, isNil_1.default)(index)) {
    return 'transparent';
  }
  return scale(index).darken(darken).brighten(brighten).alpha(alpha).hex();
};
exports.getColorFromRange = getColorFromRange;
const getSaturationColor = (saturation) => {
  let color;
  if (saturation > 110) {
    color = 'red';
  } else if (saturation > 105) {
    color = 'orange';
  } else if (saturation > 100) {
    color = 'yellow';
  } else {
    color = 'green';
  }
  return color;
};
exports.getSaturationColor = getSaturationColor;
//# sourceMappingURL=colors.js.map
