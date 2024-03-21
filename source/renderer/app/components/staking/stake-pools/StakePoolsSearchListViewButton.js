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
exports.StakePoolsSearchListViewButton = void 0;
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const StakePoolsSearch_scss_1 = __importDefault(
  require('./StakePoolsSearch.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/list-ic... Remove this comment to see the full error message
const list_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/list-ic.inline.svg')
);
const StakePoolsSearch_messages_1 = require('./StakePoolsSearch.messages');
function StakePoolsSearchListViewButtonComponent({
  onClick,
  onListViewVisited,
  isListView,
  isListViewTooltipVisible,
  intl,
  tooltipTarget,
}) {
  const [visible, setVisible] = (0, react_1.useState)(false);
  const isPopOverVisible = visible || isListViewTooltipVisible;
  return react_1.default.createElement(
    PopOver_1.PopOver,
    {
      visible: isPopOverVisible,
      content: intl.formatMessage(
        StakePoolsSearch_messages_1.messages.listIconTooltip
      ),
      appendTo: () => tooltipTarget,
    },
    react_1.default.createElement(
      'button',
      {
        className: isListView ? StakePoolsSearch_scss_1.default.selected : null,
        onClick: onClick,
        onMouseEnter: () => setVisible(true),
        onMouseLeave: () => {
          setVisible(false);
          onListViewVisited();
        },
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: list_ic_inline_svg_1.default,
      })
    )
  );
}
exports.StakePoolsSearchListViewButton = (0, react_intl_1.injectIntl)(
  StakePoolsSearchListViewButtonComponent
);
//# sourceMappingURL=StakePoolsSearchListViewButton.js.map
