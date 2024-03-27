'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const react_intl_1 = require('react-intl');
const ExternalLinkButton_1 = require('../../widgets/ExternalLinkButton');
const votingConfig_1 = require('../../../config/votingConfig');
const Headline_scss_1 = __importDefault(require('./Headline.scss'));
const Headline_messages_1 = require('./Headline.messages');
function Headline({ onExternalLinkClick, intl }) {
  return react_1.default.createElement(
    'section',
    { className: Headline_scss_1.default.component },
    react_1.default.createElement(
      'h1',
      { className: Headline_scss_1.default.heading },
      intl.formatMessage(Headline_messages_1.messages.heading)
    ),
    react_1.default.createElement(
      'div',
      { className: Headline_scss_1.default.content },
      react_1.default.createElement(
        'div',
        { className: Headline_scss_1.default.descriptionBlock },
        react_1.default.createElement(
          'p',
          { className: Headline_scss_1.default.description },
          intl.formatMessage(Headline_messages_1.messages.descriptionRow1)
        ),
        react_1.default.createElement(
          'p',
          { className: Headline_scss_1.default.description },
          intl.formatMessage(Headline_messages_1.messages.descriptionRow2, {
            reward: new bignumber_js_1.default(
              votingConfig_1.VOTING_REWARD
            ).toFormat(0),
          })
        )
      ),
      react_1.default.createElement(ExternalLinkButton_1.ExternalLinkButton, {
        label: intl.formatMessage(
          Headline_messages_1.messages.learnMoreLinkLabel
        ),
        onClick: () =>
          onExternalLinkClick(
            intl.formatMessage(Headline_messages_1.messages.learnMoreLinkUrl)
          ),
      })
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(Headline);
//# sourceMappingURL=Headline.js.map
