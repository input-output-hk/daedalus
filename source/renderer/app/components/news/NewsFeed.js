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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const lodash_1 = require('lodash');
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross-thin.inline.svg')
);
const NewsFeed_scss_1 = __importDefault(require('./NewsFeed.scss'));
const NewsItem_1 = __importDefault(require('./NewsItem'));
const UpdateItem_1 = __importDefault(require('./UpdateItem'));
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const messages = (0, react_intl_1.defineMessages)({
  newsFeedEmpty: {
    id: 'news.newsfeed.empty',
    defaultMessage: 'Newsfeed is empty',
    description: 'Newsfeed is empty',
  },
  newsFeedNoFetch: {
    id: 'news.newsfeed.noFetch',
    defaultMessage: 'Trying to fetch the newsfeed...',
    description: 'Trying to fetch the newsfeed...',
  },
  newsFeedTitle: {
    id: 'news.newsfeed.title',
    defaultMessage: 'Newsfeed',
    description: 'Newsfeed',
  },
});
const SCROLLABLE_DOM_ELEMENT_SELECTOR = '.NewsFeed_newsFeedList';
let NewsFeed = class NewsFeed extends react_1.Component {
  static defaultProps = {
    onClose: null,
    openWithoutTransition: false,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    hasShadow: false,
  };
  scrollableDomElement = null;
  newsFeedRef = react_1.default.createRef();
  newsFeedOpenedAt;
  componentDidMount() {
    document.addEventListener('click', this.handleWindowClick);
    this.scrollableDomElement = document.querySelector(
      SCROLLABLE_DOM_ELEMENT_SELECTOR
    );
    if (!(this.scrollableDomElement instanceof HTMLElement)) return;
    this.scrollableDomElement.addEventListener('scroll', this.handleOnScroll);
  }
  componentDidUpdate(prevProps) {
    if (!prevProps.isNewsFeedOpen && this.props.isNewsFeedOpen) {
      this.newsFeedOpenedAt = Date.now();
    }
  }
  componentWillUnmount() {
    document.removeEventListener('click', this.handleWindowClick);
    if (this.scrollableDomElement) {
      this.scrollableDomElement.removeEventListener(
        'scroll',
        this.handleOnScroll
      );
    }
  }
  handleWindowClick = (event) => {
    const newsFeedElement = this.newsFeedRef.current;
    const clickedElement = event.target;
    const { isNewsFeedOpen } = this.props;
    // Detect clicks outside of the newsfeed container
    if (
      isNewsFeedOpen &&
      newsFeedElement &&
      clickedElement instanceof Node &&
      !newsFeedElement.contains(clickedElement)
    ) {
      // This is necessary otherwise the UI click on the newsfeed bell icon
      // would immediately close the newsfeed again
      const msSinceNewsFeedOpened = Date.now() - this.newsFeedOpenedAt;
      if (msSinceNewsFeedOpened > 100) {
        this.props.onClose();
      }
    }
  };
  handleOnScroll = () => {
    const { hasShadow: currentHasShadow } = this.state;
    if (this.scrollableDomElement) {
      const { scrollTop } = this.scrollableDomElement;
      const hasShadow = scrollTop > 0.5;
      if (currentHasShadow !== hasShadow) {
        this.setState({
          hasShadow,
        });
      }
    }
  };
  render() {
    const { intl } = this.context;
    const {
      news,
      isNewsFeedOpen,
      isLoadingNews,
      onClose,
      onOpenAlert,
      onMarkNewsAsRead,
      openWithoutTransition,
      onProceedNewsAction,
      onOpenExternalLink,
      currentDateFormat,
      onOpenAppUpdate,
      updateDownloadProgress = 0,
      isUpdatePostponed,
      displayAppUpdateNewsItem,
    } = this.props;
    const { hasShadow } = this.state;
    const items = (0, lodash_1.get)(news, 'all', []);
    const update = (0, lodash_1.get)(news, 'update');
    const totalUnreadNewsItems = (0, lodash_1.get)(items, 'unread', []).length;
    const hasUpdateItem = displayAppUpdateNewsItem && update;
    const componentClasses = (0, classnames_1.default)([
      NewsFeed_scss_1.default.component,
      isNewsFeedOpen ? NewsFeed_scss_1.default.show : null,
      openWithoutTransition ? NewsFeed_scss_1.default.noTransition : null,
    ]);
    const newsFeedHeaderStyles = (0, classnames_1.default)([
      NewsFeed_scss_1.default.newsFeedHeader,
      hasShadow && !hasUpdateItem ? NewsFeed_scss_1.default.hasShadow : null,
    ]);
    const newsFeedContainerStyles = (0, classnames_1.default)([
      NewsFeed_scss_1.default.newsFeedContainer,
      !hasUpdateItem ? NewsFeed_scss_1.default.noUpdateItem : null,
      hasShadow ? NewsFeed_scss_1.default.hasShadow : null,
    ]);
    const newsFeedListStyles = (0, classnames_1.default)([
      NewsFeed_scss_1.default.newsFeedList,
      hasUpdateItem ? NewsFeed_scss_1.default.hasUpdate : null,
      hasShadow ? NewsFeed_scss_1.default.hasShadow : null,
    ]);
    const newsFeedUpdateStyles = (0, classnames_1.default)([
      NewsFeed_scss_1.default.updateItem,
      hasShadow ? NewsFeed_scss_1.default.hasShadow : null,
    ]);
    return (
      // @ts-ignore ts-migrate(2322) FIXME: Type 'RefObject<HTMLElement>' is not assignable to... Remove this comment to see the full error message
      react_1.default.createElement(
        'div',
        { className: componentClasses, ref: this.newsFeedRef },
        react_1.default.createElement(
          'div',
          { className: newsFeedHeaderStyles },
          react_1.default.createElement(
            'h3',
            { className: NewsFeed_scss_1.default.newsFeedTitle },
            intl.formatMessage(messages.newsFeedTitle),
            totalUnreadNewsItems > 0 &&
              react_1.default.createElement(
                'span',
                { className: NewsFeed_scss_1.default.newsFeedBadge },
                totalUnreadNewsItems
              )
          ),
          react_1.default.createElement(
            'button',
            {
              onClick: onClose,
              className: NewsFeed_scss_1.default.newsFeedCloseBtn,
            },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: close_cross_thin_inline_svg_1.default,
            })
          )
        ),
        react_1.default.createElement(
          'div',
          { className: newsFeedContainerStyles },
          hasUpdateItem &&
            react_1.default.createElement(
              'div',
              { className: newsFeedUpdateStyles },
              react_1.default.createElement(UpdateItem_1.default, {
                key: update.id,
                updateItem: update,
                // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                isNewsFeedOpen: isNewsFeedOpen,
                onMarkNewsAsRead: onMarkNewsAsRead,
                onOpenAlert: onOpenAlert,
                onProceedNewsAction: onProceedNewsAction,
                onOpenAppUpdate: onOpenAppUpdate,
                currentDateFormat: currentDateFormat,
                downloadProgress: updateDownloadProgress,
                isUpdatePostponed: isUpdatePostponed,
              })
            ),
          items.length > 0 &&
            react_1.default.createElement(
              'div',
              { className: newsFeedListStyles },
              hasUpdateItem &&
                react_1.default.createElement('hr', {
                  className: NewsFeed_scss_1.default.separator,
                }),
              items.map((newsItem) =>
                react_1.default.createElement(NewsItem_1.default, {
                  key: newsItem.id,
                  hasUpdateItem: hasUpdateItem,
                  newsItem: newsItem,
                  isNewsFeedOpen: isNewsFeedOpen,
                  onMarkNewsAsRead: onMarkNewsAsRead,
                  onOpenAlert: onOpenAlert,
                  onProceedNewsAction: onProceedNewsAction,
                  onOpenExternalLink: onOpenExternalLink,
                  currentDateFormat: currentDateFormat,
                })
              )
            ),
          news &&
            items.length === 0 &&
            !isLoadingNews &&
            react_1.default.createElement(
              'div',
              { className: NewsFeed_scss_1.default.newsFeedEmptyContainer },
              react_1.default.createElement(
                'p',
                { className: NewsFeed_scss_1.default.newsFeedEmpty },
                intl.formatMessage(messages.newsFeedEmpty)
              )
            ),
          (!news || items.length === 0) &&
            isLoadingNews &&
            react_1.default.createElement(
              'div',
              { className: NewsFeed_scss_1.default.newsFeedNoFetchContainer },
              react_1.default.createElement(
                'p',
                { className: NewsFeed_scss_1.default.newsFeedNoFetch },
                intl.formatMessage(messages.newsFeedNoFetch)
              ),
              react_1.default.createElement(LoadingSpinner_1.default, {
                medium: true,
              })
            )
        )
      )
    );
  }
};
NewsFeed = __decorate([mobx_react_1.observer], NewsFeed);
exports.default = NewsFeed;
//# sourceMappingURL=NewsFeed.js.map
