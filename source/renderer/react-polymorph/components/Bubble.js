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
var __rest =
  (this && this.__rest) ||
  function (s, e) {
    var t = {};
    for (var p in s)
      if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === 'function')
      for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
        if (
          e.indexOf(p[i]) < 0 &&
          Object.prototype.propertyIsEnumerable.call(s, p[i])
        )
          t[p[i]] = s[p[i]];
      }
    return t;
  };
exports.__esModule = true;
exports.Bubble = void 0;
// @ts-nocheck
var react_1 = require('react');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
var events_1 = require('../utils/events');
// import constants
var _1 = require('.');
var BubbleBase = /** @class */ (function (_super) {
  __extends(BubbleBase, _super);
  function BubbleBase(props) {
    var _this = _super.call(this, props) || this;
    _this._hasEventListeners = false;
    // =========== PRIVATE HELPERS ==============
    _this._handleScrollEventListener = function (action) {
      // const rootNode = this.rootElement;
      var rootElement = _this.rootElement;
      if (rootElement) {
        var scrollableNode = _this._getFirstScrollableParent(rootElement);
        if (scrollableNode) {
          if (action === 'add') {
            scrollableNode.addEventListener('scroll', _this._updatePosition);
          } else if (action === 'remove') {
            scrollableNode.removeEventListener('scroll', _this._updatePosition);
          }
        }
      }
    };
    _this._getFirstScrollableParent = function (element) {
      if (element == null) return null;
      var rootElement = _this.rootElement;
      var node = {}.hasOwnProperty.call(element, 'current')
        ? element.current
        : element;
      if (rootElement) {
        if (
          node === rootElement.current ||
          node.scrollHeight <= node.clientHeight
        ) {
          return _this._getFirstScrollableParent(node.parentElement);
        }
      }
      return node;
    };
    _this._updatePosition = function () {
      var _a = _this.props,
        isOpeningUpward = _a.isOpeningUpward,
        targetRef = _a.targetRef;
      var rootElement = _this.rootElement;
      var target =
        targetRef && typeof targetRef !== 'string' ? targetRef.current : null;
      // Without a target, try to fallback to the parent node
      if (!target) {
        //  Only proceed if the root element is defined
        if (!rootElement || !rootElement.current) return;
        target = rootElement.current.parentElement;
      }
      var targetRect = target.getBoundingClientRect();
      var positionY;
      if (isOpeningUpward) {
        // Since we don't know the height of the bubble before rendering it we positioning
        // it with { bottom: XYpx } (within the viewport) and need this calculation:
        positionY = window.innerHeight - targetRect.top;
      } else {
        positionY = targetRect.bottom;
      }
      var position = {
        width: targetRect.width,
        positionX: targetRect.left,
        positionY: positionY,
      };
      _this.setState({
        position: position,
      });
    };
    // define ref
    _this.rootElement = react_1['default'].createRef();
    var context = props.context,
      themeId = props.themeId,
      theme = props.theme,
      themeOverrides = props.themeOverrides;
    _this.state = {
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      position: null,
    };
    return _this;
  }
  BubbleBase.prototype.componentDidMount = function () {
    var _this = this;
    setTimeout(function () {
      if (_this.props.isFloating) _this._updatePosition();
    }, 0);
  };
  BubbleBase.prototype.componentDidUpdate = function (prevProps) {
    var isHidden = this.props.isHidden;
    var didVisibilityChange = isHidden !== prevProps.isHidden;
    var wasBubbleHidden = !prevProps.isHidden && isHidden;
    if (prevProps.isFloating && !isHidden && !this._hasEventListeners) {
      this._handleScrollEventListener('add');
      (0, events_1.addDocumentListeners)(this._getDocumentEvents());
      window.addEventListener('resize', this._updatePosition);
      this._hasEventListeners = true;
    }
    if (wasBubbleHidden) this._removeAllEventListeners();
    if (didVisibilityChange) this._updatePosition();
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  BubbleBase.prototype.componentWillUnmount = function () {
    if (this._hasEventListeners) this._removeAllEventListeners();
  };
  BubbleBase.prototype._removeAllEventListeners = function () {
    if (this._hasEventListeners) {
      (0, events_1.removeDocumentListeners)(this._getDocumentEvents());
      this._handleScrollEventListener('remove');
      window.removeEventListener('resize', this._updatePosition);
      this._hasEventListeners = false;
    }
  };
  BubbleBase.prototype._getDocumentEvents = function () {
    return {
      resize: this._updatePosition,
      scroll: this._updatePosition,
    };
  };
  BubbleBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      skin = _a.skin,
      theme = _a.theme,
      themeOverrides = _a.themeOverrides,
      context = _a.context,
      rest = __rest(_a, ['skin', 'theme', 'themeOverrides', 'context']);
    var BubbleSkin = skin || context.skins[_1.IDENTIFIERS.BUBBLE];
    return (
      <BubbleSkin
        rootRef={this.rootElement}
        position={this.state.position}
        theme={this.state.composedTheme}
        {...rest}
      />
    );
  };
  // define static properties
  BubbleBase.displayName = 'Bubble';
  BubbleBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    isCentered: false,
    isHidden: false,
    isFloating: false,
    isOpeningUpward: false,
    isTransparent: true,
    arrowRelativeToTip: false,
    noArrow: false,
    theme: null,
    themeId: _1.IDENTIFIERS.BUBBLE,
    themeOverrides: {},
  };
  return BubbleBase;
})(react_1.Component);
exports.Bubble = (0, withTheme_1.withTheme)(BubbleBase);
