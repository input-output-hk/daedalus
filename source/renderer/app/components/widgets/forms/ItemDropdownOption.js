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
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const ItemDropdownOption_scss_1 = __importDefault(
  require('./ItemDropdownOption.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/spinner... Remove this comment to see the full error message
const spinner_tiny_inline_svg_1 = __importDefault(
  require('../../../assets/images/spinner-tiny.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  syncingLabel: {
    id: 'widgets.itemsDropdown.syncingLabel',
    defaultMessage: '!!!Syncing',
    description: 'syncingLabel for ItemDropdownOption',
  },
  syncingLabelProgress: {
    id: 'widgets.itemsDropdown.syncingLabelProgress',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description: 'syncingLabel for WalletsDropdown',
  },
});
class ItemDropdownOption extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  renderDetail = () => {
    const { detail, isSyncing } = this.props;
    if (!detail || isSyncing) return null;
    return react_1.default.createElement(
      'div',
      { className: ItemDropdownOption_scss_1.default.detail },
      detail
    );
  };
  renderSyncingSpinner = () => {
    const { intl } = this.context;
    const { syncingProgress } = this.props;
    const defaultSyncingLabel =
      typeof syncingProgress === 'number' || typeof syncingProgress === 'string'
        ? intl.formatMessage(messages.syncingLabelProgress, {
            syncingProgress,
          })
        : intl.formatMessage(messages.syncingLabel);
    const { isSyncing, syncingLabel = defaultSyncingLabel } = this.props;
    if (!isSyncing) return null;
    return react_1.default.createElement(
      'div',
      { className: ItemDropdownOption_scss_1.default.syncingSpinner },
      react_1.default.createElement(
        PopOver_1.PopOver,
        { content: syncingLabel },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: spinner_tiny_inline_svg_1.default,
        })
      )
    );
  };
  render() {
    const { selected, detail, isSyncing, label } = this.props;
    const componentStyles = (0, classnames_1.default)(
      ItemDropdownOption_scss_1.default.component,
      {
        [ItemDropdownOption_scss_1.default.selected]: selected,
        [ItemDropdownOption_scss_1.default.noDetail]: !detail || isSyncing,
      }
    );
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(
        'div',
        { className: ItemDropdownOption_scss_1.default.label },
        label,
        this.renderSyncingSpinner()
      ),
      this.renderDetail()
    );
  }
}
exports.default = ItemDropdownOption;
//# sourceMappingURL=ItemDropdownOption.js.map
