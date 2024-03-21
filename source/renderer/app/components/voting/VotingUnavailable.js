'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const VotingUnavailable_scss_1 = __importDefault(
  require('./VotingUnavailable.scss')
);
const formatters_1 = require('../../utils/formatters');
function VotingUnavailable({ syncPercentage }) {
  return react_1.default.createElement(
    'div',
    { className: VotingUnavailable_scss_1.default.component },
    react_1.default.createElement(LoadingSpinner_1.default, { big: true }),
    react_1.default.createElement(
      'div',
      { className: VotingUnavailable_scss_1.default.description },
      react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
        ...global_messages_1.default.featureUnavailableWhileSyncing,
        values: {
          syncPercentage: (0, formatters_1.formattedNumber)(syncPercentage, 2),
        },
      })
    )
  );
}
exports.default = (0, mobx_react_1.observer)(VotingUnavailable);
//# sourceMappingURL=VotingUnavailable.js.map
