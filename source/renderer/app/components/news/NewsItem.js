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
const classnames_1 = __importDefault(require('classnames'));
const react_markdown_1 = __importDefault(require('react-markdown'));
const moment_1 = __importDefault(require('moment'));
const lodash_1 = require('lodash');
const react_animate_height_1 = __importDefault(require('react-animate-height'));
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const News_1 = require('../../domains/News');
const ButtonLink_1 = __importDefault(require('../widgets/ButtonLink'));
const NewsItem_scss_1 = __importDefault(require('./NewsItem.scss'));
let NewsItem = class NewsItem extends react_1.Component {
  static defaultProps = {
    onNewsItemActionClick: null,
    expandWithoutTransition: false,
  };
  state = {
    newsItemExpanded: false,
    newsItemCollapsible: true,
  };
  componentDidUpdate(prevProps) {
    const { newsItemExpanded } = this.state;
    if (
      prevProps.isNewsFeedOpen &&
      !this.props.isNewsFeedOpen &&
      newsItemExpanded
    ) {
      this.setState({
        newsItemExpanded: false,
      }); // eslint-disable-line
    }
  }
  newsItemClickHandler(event) {
    const linkUrl = (0, lodash_1.get)(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    } else {
      const { type, id } = this.props.newsItem;
      const { newsItemCollapsible } = this.state;
      if (
        type === News_1.NewsTypes.INFO ||
        type === News_1.NewsTypes.ANNOUNCEMENT
      ) {
        if (newsItemCollapsible) {
          this.setState((prevState) => ({
            newsItemExpanded: !prevState.newsItemExpanded,
          }));
        } else {
          this.setState({
            newsItemCollapsible: true,
          });
        }
      }
      if (News_1.NewsTypes.ALERT && this.props.onOpenAlert) {
        this.props.onOpenAlert(id);
      }
      this.props.onMarkNewsAsRead(id);
    }
  }
  onProceedNewsAction(event) {
    const { newsItem, onProceedNewsAction } = this.props;
    onProceedNewsAction(newsItem, event);
  }
  generateTitleWithBadge = (title, isRead) => {
    const wordsArray = title.split(' ');
    const lastWordIndex = wordsArray.length - 1;
    const lastWord = wordsArray[lastWordIndex];
    // Remove last word from array
    wordsArray.splice(lastWordIndex, 1);
    // Join words without last one
    const firstSentencePart = wordsArray.join(' ');
    return react_1.default.createElement(
      'h4',
      { className: NewsItem_scss_1.default.newsItemTitle },
      firstSentencePart ? `${firstSentencePart} ` : null,
      react_1.default.createElement(
        'span',
        { className: NewsItem_scss_1.default.lastWordWrapper },
        lastWord,
        '\u00A0',
        !isRead &&
          react_1.default.createElement('span', {
            className: NewsItem_scss_1.default.newsItemBadge,
          })
      )
    );
  };
  render() {
    const {
      newsItem,
      expandWithoutTransition,
      currentDateFormat,
      hasUpdateItem,
    } = this.props;
    const componentClasses = (0, classnames_1.default)([
      NewsItem_scss_1.default.component,
      newsItem.type ? NewsItem_scss_1.default[newsItem.type] : null,
      this.state.newsItemExpanded ? NewsItem_scss_1.default.expanded : null,
      newsItem.read ? NewsItem_scss_1.default.isRead : null,
      !hasUpdateItem ? NewsItem_scss_1.default.noUpdateItem : null,
    ]);
    const { url = '' } = newsItem.action;
    const title = this.generateTitleWithBadge(newsItem.title, newsItem.read);
    return react_1.default.createElement(
      'div',
      {
        className: componentClasses,
        role: 'presentation',
        onClick: this.newsItemClickHandler.bind(this),
      },
      title,
      react_1.default.createElement(
        'div',
        { className: NewsItem_scss_1.default.newsItemDate },
        (0, moment_1.default)(newsItem.date).format(currentDateFormat)
      ),
      react_1.default.createElement(
        'div',
        null,
        react_1.default.createElement(
          react_animate_height_1.default,
          {
            duration: expandWithoutTransition ? 0 : 500,
            height: this.state.newsItemExpanded ? 'auto' : 0,
          },
          react_1.default.createElement(
            'div',
            { className: NewsItem_scss_1.default.newsItemContentContainer },
            react_1.default.createElement(react_markdown_1.default, {
              escapeHtml: false,
              source: newsItem.content,
              disallowedTypes: [
                'image',
                'imageReference',
                'table',
                'definition',
                'inlineCode',
                'code',
                'html',
                // @ts-ignore
                'virtualHtml',
              ],
            })
          ),
          react_1.default.createElement(
            ButtonLink_1.default,
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            {
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className: NewsItem_scss_1.default.newsItemActionBtn,
              onClick: (e) => this.onProceedNewsAction(e),
              skin: ButtonSkin_1.ButtonSkin,
              label: newsItem.action.label,
              linkProps: {
                className: NewsItem_scss_1.default.externalLink,
                hasIconBefore: false,
                hasIconAfter: url.length > 0,
              },
            }
          )
        )
      )
    );
  }
};
NewsItem = __decorate([mobx_react_1.observer], NewsItem);
exports.default = NewsItem;
//# sourceMappingURL=NewsItem.js.map
