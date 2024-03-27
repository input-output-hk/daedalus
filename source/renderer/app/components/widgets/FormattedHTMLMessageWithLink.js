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
Object.defineProperty(exports, '__esModule', { value: true });
exports.FormattedHTMLMessageWithLink = void 0;
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
class FormattedHTMLMessageWithLink extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { message, onExternalLinkClick } = this.props;
    const { linkPosition, linkLabel, linkURL } = message.values;
    const MainMessage = react_1.default.createElement(
      react_1.Fragment,
      { key: 'mainMessage' },
      '\u00A0',
      intl.formatMessage(message),
      '\u00A0'
    );
    const url = intl.formatMessage(linkURL);
    const Link = react_1.default.createElement(
      react_1.Fragment,
      { key: 'link' },
      react_1.default.createElement(
        'a',
        { href: url, onClick: (event) => onExternalLinkClick(url, event) },
        intl.formatMessage(linkLabel)
      )
    );
    return linkPosition === 'before'
      ? [Link, MainMessage]
      : [MainMessage, Link];
  }
}
exports.FormattedHTMLMessageWithLink = FormattedHTMLMessageWithLink;
//# sourceMappingURL=FormattedHTMLMessageWithLink.js.map
