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
exports.onSearchItemsDropdown = void 0;
const react_1 = __importStar(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const Select_1 = require('@react-polymorph/components/Select');
const SelectSkin_1 = require('@react-polymorph/skins/simple/SelectSkin');
const ItemDropdownOption_1 = __importDefault(require('./ItemDropdownOption'));
const ItemsDropdown_scss_1 = __importDefault(require('./ItemsDropdown.scss'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const onSearchItemsDropdown = (searchValue, options) => {
  return (0, lodash_1.filter)(options, (option) => {
    const { label, detail, value } = option;
    const regex = new RegExp((0, lodash_1.escapeRegExp)(searchValue), 'i');
    return regex.test(label) || regex.test(detail) || regex.test(value);
  });
};
exports.onSearchItemsDropdown = onSearchItemsDropdown;
class ItemsDropdown extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    optionRenderer: (optionProps) =>
      react_1.default.createElement(ItemDropdownOption_1.default, {
        ...optionProps,
      }),
    selectionRenderer: (optionProps) =>
      react_1.default.createElement(ItemDropdownOption_1.default, {
        selected: true,
        ...optionProps,
      }),
    onSearch: exports.onSearchItemsDropdown,
    skin: SelectSkin_1.SelectSkin,
  };
  render() {
    const { intl } = this.context;
    const { className } = this.props;
    const componentStyles = (0, classnames_1.default)([
      ItemsDropdown_scss_1.default.component,
      className,
    ]);
    return react_1.default.createElement(Select_1.Select, {
      ...this.props,
      className: componentStyles,
      optionHeight: 50,
      noResultsMessage: intl.formatMessage(
        global_messages_1.default.searchNoResultsMessage
      ),
    });
  }
}
exports.default = ItemsDropdown;
//# sourceMappingURL=ItemsDropdown.js.map
