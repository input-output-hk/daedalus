'use strict';
var __extends =
  (this && this.__extends) ||
  (function () {
    var extendStatics = function (d, b) {
      extendStatics =
        Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array &&
          function (d, b) {
            d.__proto__ = b;
          }) ||
        function (d, b) {
          for (var p in b)
            if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p];
        };
      return extendStatics(d, b);
    };
    return function (d, b) {
      if (typeof b !== 'function' && b !== null)
        throw new TypeError(
          'Class extends value ' + String(b) + ' is not a constructor or null'
        );
      extendStatics(d, b);
      function __() {
        this.constructor = d;
      }
      d.prototype =
        b === null
          ? Object.create(b)
          : ((__.prototype = b.prototype), new __());
    };
  })();
exports.__esModule = true;
exports.InfiniteScroll = void 0;
// @ts-nocheck
var react_1 = require('react');
// utilities
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// constants
var _1 = require('.');
var InfiniteScrollBase = /** @class */ (function (_super) {
  __extends(InfiniteScrollBase, _super);
  function InfiniteScrollBase(props) {
    var _this = _super.call(this, props) || this;
    // calls user's fetchData function from props
    _this._handleFetchData = function () {
      return _this.props.fetchData(_this.setState.bind(_this));
    };
    // scroll event listener attached to scrollContainer element
    _this._handleScroll = function () {
      var _a = _this.state,
        error = _a.error,
        isLoading = _a.isLoading,
        hasMoreData = _a.hasMoreData;
      // return early for error, loading, or lack of future data
      if (error || isLoading || !hasMoreData) {
        return;
      }
      return _this._checkForScrollBottom();
    };
    // prevents new data fetch until user has scrolled near bottom of existing data
    _this._checkForScrollBottom = function () {
      var _a = _this,
        scrollContainer = _a.scrollContainer,
        threshold = _a.props.threshold;
      if (!scrollContainer.current) return;
      var _b = scrollContainer.current,
        offsetHeight = _b.offsetHeight,
        scrollTop = _b.scrollTop,
        scrollHeight = _b.scrollHeight;
      if (offsetHeight + scrollTop >= scrollHeight - threshold) {
        return _this._handleFetchData();
      }
    };
    _this._isFunction = function (renderProp) {
      return renderProp && typeof renderProp === 'function';
    };
    var context = props.context,
      themeId = props.themeId,
      theme = props.theme,
      themeOverrides = props.themeOverrides;
    // refs
    _this.scrollContainer = react_1['default'].createRef();
    _this.state = {
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      data: [],
      error: false,
      isLoading: false,
      hasMoreData: true,
    };
    return _this;
  }
  InfiniteScrollBase.prototype.componentDidMount = function () {
    var scrollContainer = this.scrollContainer;
    this._handleFetchData();
    if (!scrollContainer.current) return;
    scrollContainer.current.addEventListener('scroll', this._handleScroll);
  };
  InfiniteScrollBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  InfiniteScrollBase.prototype.render = function () {
    var _a = this,
      _b = _a.props,
      className = _b.className,
      context = _b.context,
      renderItems = _b.renderItems,
      skin = _b.skin,
      themeId = _b.themeId,
      _c = _a.state,
      composedTheme = _c.composedTheme,
      data = _c.data,
      error = _c.error,
      hasMoreData = _c.hasMoreData,
      isLoading = _c.isLoading,
      scrollContainer = _a.scrollContainer;
    if (!this._isFunction(renderItems)) {
      return null;
    }
    var InfiniteScrollSkin =
      skin || context.skins[_1.IDENTIFIERS.INFINITE_SCROLL];
    return (
      <InfiniteScrollSkin
        className={className}
        data={data}
        error={error}
        hasMoreData={hasMoreData}
        isLoading={isLoading}
        renderItems={renderItems}
        scrollContainerRef={scrollContainer}
        theme={composedTheme}
        themeId={themeId}
      />
    );
  };
  // define static properties
  InfiniteScrollBase.displayName = 'InfiniteScroll';
  InfiniteScrollBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    fetchData: function () {},
    theme: null,
    themeId: _1.IDENTIFIERS.INFINITE_SCROLL,
    themeOverrides: {},
    threshold: 250,
  };
  return InfiniteScrollBase;
})(react_1.Component);
exports.InfiniteScroll = (0, withTheme_1.withTheme)(InfiniteScrollBase);
