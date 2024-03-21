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
const formatters_1 = require('../../../utils/formatters');
const ExternalLinkButton_1 = require('../../widgets/ExternalLinkButton');
const ResultsPhase_messages_1 = require('./ResultsPhase.messages');
const VotingInfo_messages_1 = require('./VotingInfo.messages');
const CurrentPhase_scss_1 = __importDefault(require('./CurrentPhase.scss'));
function ResultsPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  onExternalLinkClick,
  intl,
}) {
  const mappedFormats = (0, formatters_1.mapToLongDateTimeFormat)({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const endDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.endTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
      currentTimeFormat: mappedFormats.currentTimeFormat,
    }
  );
  return react_1.default.createElement(
    'section',
    { className: CurrentPhase_scss_1.default.root },
    react_1.default.createElement(
      'h1',
      { className: CurrentPhase_scss_1.default.fundName },
      intl.formatMessage(VotingInfo_messages_1.messages.fundName, {
        votingFundNumber: fundInfo.current.number,
      })
    ),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.block },
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.label },
        intl.formatMessage(ResultsPhase_messages_1.messages.endDateLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        endDate
      )
    ),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.resultsButton },
      react_1.default.createElement(ExternalLinkButton_1.ExternalLinkButton, {
        label: intl.formatMessage(
          ResultsPhase_messages_1.messages.viewResultsLinkLabel
        ),
        onClick: () =>
          onExternalLinkClick(
            intl.formatMessage(
              ResultsPhase_messages_1.messages.viewResultsLinkURL
            )
          ),
      })
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(ResultsPhase)
);
//# sourceMappingURL=ResultsPhase.js.map
