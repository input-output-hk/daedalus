'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
const react_1 = __importDefault(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const news_feed_icon_inline_svg_1 = __importDefault(
  require('../../assets/images/top-bar/news-feed-icon.inline.svg')
);
const NewsFeedIcon_scss_1 = __importDefault(require('./NewsFeedIcon.scss'));
const timingConfig_1 = require('../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  iconTooltip: {
    id: 'news.newsfeed.iconTooltip',
    defaultMessage: '!!!Newsfeed',
    description: 'Newsfeed',
  },
});
function NewsFeedIcon({
  intl,
  onNewsFeedIconClick,
  newsFeedIconClass,
  hasNotification,
  hasUpdate,
}) {
  const buttonClasses = (0, classnames_1.default)([
    NewsFeedIcon_scss_1.default.button,
    hasNotification &&
      !hasUpdate &&
      NewsFeedIcon_scss_1.default.notificationDot,
    hasUpdate && NewsFeedIcon_scss_1.default.updateDot,
    newsFeedIconClass,
  ]);
  return react_1.default.createElement(
    'div',
    { className: NewsFeedIcon_scss_1.default.component },
    react_1.default.createElement(
      PopOver_1.PopOver,
      {
        appendTo: 'parent',
        delay: timingConfig_1.TOOLTIP_DELAY,
        offset: [0, 0],
        content: react_1.default.createElement(
          'span',
          { className: NewsFeedIcon_scss_1.default.tooltip },
          intl.formatMessage(messages.iconTooltip)
        ),
      },
      react_1.default.createElement(
        'button',
        { className: buttonClasses, onClick: onNewsFeedIconClick },
        react_1.default.createElement(react_svg_inline_1.default, {
          className: NewsFeedIcon_scss_1.default.icon,
          svg: news_feed_icon_inline_svg_1.default,
        })
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(NewsFeedIcon);
//# sourceMappingURL=NewsFeedIcon.js.map
