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
const CurrentPhase_scss_1 = __importDefault(require('./CurrentPhase.scss'));
const SnapshotPhase_messages_1 = require('./SnapshotPhase.messages');
const VotingInfo_messages_1 = require('./VotingInfo.messages');
function SnapshotPhase({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundInfo,
  intl,
}) {
  const mappedFormats = (0, formatters_1.mapToLongDateTimeFormat)({
    currentLocale,
    currentDateFormat,
    currentTimeFormat,
  });
  const snapshotDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.registrationSnapshotTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
      currentTimeFormat: mappedFormats.currentTimeFormat,
    }
  );
  const startDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.startTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
    }
  );
  const endDate = (0, formatters_1.formattedDateTime)(
    fundInfo.current.endTime,
    {
      currentLocale,
      currentDateFormat: mappedFormats.currentDateFormat,
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
        intl.formatMessage(SnapshotPhase_messages_1.messages.snapshotDateLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        snapshotDate
      )
    ),
    react_1.default.createElement(
      'div',
      { className: CurrentPhase_scss_1.default.block },
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.label },
        intl.formatMessage(SnapshotPhase_messages_1.messages.votingDateLabel)
      ),
      react_1.default.createElement(
        'span',
        { className: CurrentPhase_scss_1.default.value },
        startDate,
        ' \u2013 ',
        endDate
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(SnapshotPhase)
);
//# sourceMappingURL=SnapshotPhase.js.map
