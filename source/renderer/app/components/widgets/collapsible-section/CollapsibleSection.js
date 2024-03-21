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
exports.CollapsibleSection = void 0;
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Link_1 = require('@react-polymorph/components/Link');
const CollapsibleSection_scss_1 = __importDefault(
  require('./CollapsibleSection.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
exports.CollapsibleSection = (0, react_intl_1.injectIntl)(
  ({
    intl,
    header,
    children,
    expandMessage = global_messages_1.default.view,
    collapseMessage,
    headerFontStyle = 'bold',
    expandButtonStyle = 'button',
  }) => {
    const [expanded, setExpanded] = (0, react_1.useState)(false);
    const handleToggle = (0, react_1.useCallback)(() => {
      setExpanded((previousExpanded) => !previousExpanded);
    }, [setExpanded]);
    const buttonMessage = intl.formatMessage(
      expanded
        ? collapseMessage || global_messages_1.default.hide
        : expandMessage
    );
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        'h2',
        {
          className: (0, classnames_1.default)(
            CollapsibleSection_scss_1.default.header,
            headerFontStyle === 'light'
              ? CollapsibleSection_scss_1.default.lightHeader
              : CollapsibleSection_scss_1.default.boldHeader,
            expandButtonStyle === 'button' &&
              CollapsibleSection_scss_1.default.flexHeader
          ),
        },
        header,
        expandButtonStyle === 'button' &&
          react_1.default.createElement(
            'button',
            {
              className: CollapsibleSection_scss_1.default.toggleButton,
              onClick: handleToggle,
            },
            buttonMessage
          ),
        expandButtonStyle === 'link' &&
          react_1.default.createElement(
            react_1.default.Fragment,
            null,
            ' ',
            react_1.default.createElement(Link_1.Link, {
              className: CollapsibleSection_scss_1.default.toggleLink,
              onClick: handleToggle,
              label: buttonMessage,
              hasIconAfter: false,
            }),
            '.'
          )
      ),
      expanded && children
    );
  }
);
//# sourceMappingURL=CollapsibleSection.js.map
