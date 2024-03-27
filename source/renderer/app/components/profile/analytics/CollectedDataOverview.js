'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.CollectedDataOverview = void 0;
const react_1 = __importDefault(require('react'));
const react_intl_1 = require('react-intl');
const CollapsibleSection_1 = require('../../widgets/collapsible-section/CollapsibleSection');
const CollectedDataOverview_scss_1 = __importDefault(
  require('./CollectedDataOverview.scss')
);
const CollectedDataOverview_messages_1 = require('./CollectedDataOverview.messages');
exports.CollectedDataOverview = (0, react_intl_1.injectIntl)(({ intl }) => {
  return react_1.default.createElement(
    CollapsibleSection_1.CollapsibleSection,
    {
      header: intl.formatMessage(
        CollectedDataOverview_messages_1.messages.title
      ),
      expandMessage: CollectedDataOverview_messages_1.messages.expandButton,
      collapseMessage: CollectedDataOverview_messages_1.messages.collapseButton,
      expandButtonStyle: 'link',
      headerFontStyle: 'light',
    },
    react_1.default.createElement(
      'ol',
      { className: CollectedDataOverview_scss_1.default.dataCollectionList },
      react_1.default.createElement(
        'li',
        null,
        react_1.default.createElement(
          'h3',
          null,
          intl.formatMessage(
            CollectedDataOverview_messages_1.messages.userBehaviorTitle
          )
        ),
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(
            CollectedDataOverview_messages_1.messages.userBehaviorText
          )
        )
      ),
      react_1.default.createElement(
        'li',
        null,
        react_1.default.createElement(
          'h3',
          null,
          intl.formatMessage(
            CollectedDataOverview_messages_1.messages.deviceInfoTitle
          )
        ),
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(
            CollectedDataOverview_messages_1.messages.deviceInfoText
          )
        )
      )
    )
  );
});
//# sourceMappingURL=CollectedDataOverview.js.map
