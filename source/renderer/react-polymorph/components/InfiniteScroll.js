const __extends =
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
          for (const p in b)
            if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p];
        };
      return extendStatics(d, b);
    };
    return function (d, b) {
      if (typeof b !== 'function' && b !== null)
        throw new TypeError(
          `Class extends value ${String(b)} is not a constructor or null`
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
const react_1 = require('react');
// utilities
const withTheme_1 = require('./HOC/withTheme');
const themes_1 = require('../utils/themes');
// constants
const _1 = require('.');

const InfiniteScrollBase = /** @class */ (function (_super) {
  __extends(InfiniteScrollBase, _super);
  function InfiniteScrollBase(props) {
    const _this = _super.call(this, props) || this;
    // calls user's fetchData function from props
    _this._handleFetchData = function () {
      return _this.props.fetchData(_this.setState.bind(_this));
    };
    // scroll event listener attached to scrollContainer element
    _this._handleScroll = function () {
      const _a = _this.state;
      const { error } = _a;
      const { isLoading } = _a;
      const { hasMoreData } = _a;
      // return early for error, loading, or lack of future data
      if (error || isLoading || !hasMoreData) {
        return;
      }
      return _this._checkForScrollBottom();
    };
    // prevents new data fetch until user has scrolled near bottom of existing data
    _this._checkForScrollBottom = function () {
      const _a = _this;
      const { scrollContainer } = _a;
      const { threshold } = _a.props;
      if (!scrollContainer.current) return;
      const _b = scrollContainer.current;
      const { offsetHeight } = _b;
      const { scrollTop } = _b;
      const { scrollHeight } = _b;
      if (offsetHeight + scrollTop >= scrollHeight - threshold) {
        return _this._handleFetchData();
      }
    };
    _this._isFunction = function (renderProp) {
      return renderProp && typeof renderProp === 'function';
    };
    const { context } = props;
    const { themeId } = props;
    const { theme } = props;
    const { themeOverrides } = props;
    // refs
    _this.scrollContainer = react_1.default.createRef();
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
    const { scrollContainer } = this;
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
    const _a = this;
    const _b = _a.props;
    const { className } = _b;
    const { context } = _b;
    const { renderItems } = _b;
    const { skin } = _b;
    const { themeId } = _b;
    const _c = _a.state;
    const { composedTheme } = _c;
    const { data } = _c;
    const { error } = _c;
    const { hasMoreData } = _c;
    const { isLoading } = _c;
    const { scrollContainer } = _a;
    if (!this._isFunction(renderItems)) {
      return null;
    }
    const InfiniteScrollSkin =
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
    fetchData() {},
    theme: null,
    themeId: _1.IDENTIFIERS.INFINITE_SCROLL,
    themeOverrides: {},
    threshold: 250,
  };
  return InfiniteScrollBase;
})(react_1.Component);
exports.InfiniteScroll = (0, withTheme_1.withTheme)(InfiniteScrollBase);
