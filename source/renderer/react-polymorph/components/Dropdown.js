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
exports.Dropdown = void 0;
// @ts-nocheck
var react_1 = require('react');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// import constants
var _1 = require('.');
var GlobalListeners_1 = require('./HOC/GlobalListeners');
var DropdownBase = /** @class */ (function (_super) {
  __extends(DropdownBase, _super);
  function DropdownBase(props) {
    var _this = _super.call(this, props) || this;
    // ========= PUBLIC SKIN API =========
    _this.isOpen = function () {
      var _a = _this.props,
        clickToOpen = _a.clickToOpen,
        isOpen = _a.isOpen;
      var _b = _this.state,
        isMouseOverItems = _b.isMouseOverItems,
        isMouseOverRoot = _b.isMouseOverRoot;
      var isOpenBecauseOfHover = clickToOpen
        ? false
        : isMouseOverItems || isMouseOverRoot;
      return isOpen || _this.state.isOpen || isOpenBecauseOfHover;
    };
    _this.toggleOpen = function () {
      if (_this.isOpen()) {
        _this.close();
      } else {
        _this.setState({
          isOpen: true,
        });
      }
    };
    _this.close = function () {
      _this._setMouseOverRoot(false);
      _this._setMouseOverItems(false);
      _this.setState({
        isOpen: false,
      });
    };
    // ========= PRIVATE SKIN API =========
    _this._setMouseOverItems = function (isMouseOverItems) {
      _this.setState({
        isMouseOverItems: isMouseOverItems,
      });
    };
    _this._setMouseOverRoot = function (isMouseOverRoot) {
      _this.setState({
        isMouseOverRoot: isMouseOverRoot,
      });
    };
    _this._onItemSelected = function (item) {
      var onItemSelected = _this.props.onItemSelected;
      _this.close();
      if (onItemSelected) {
        onItemSelected(item);
      }
    };
    _this._onLabelClick = function () {
      if (_this.props.clickToOpen) {
        _this.toggleOpen();
      }
    };
    // define ref
    _this.rootElement = react_1['default'].createRef();
    _this.optionsElement = react_1['default'].createRef();
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
      isMouseOverItems: false,
      isMouseOverRoot: false,
      isOpen: false,
    };
    return _this;
  }
  DropdownBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  DropdownBase.prototype.componentWillUnmount = function () {
    this.close();
  };
  DropdownBase.prototype.render = function () {
    var _this = this;
    var _a = this.props,
      clickToOpen = _a.clickToOpen,
      context = _a.context,
      isOpen = _a.isOpen,
      isOpeningUpward = _a.isOpeningUpward,
      onItemSelected = _a.onItemSelected,
      skin = _a.skin,
      theme = _a.theme,
      themeOverrides = _a.themeOverrides,
      rest = __rest(_a, [
        'clickToOpen',
        'context',
        'isOpen',
        'isOpeningUpward',
        'onItemSelected',
        'skin',
        'theme',
        'themeOverrides',
      ]);
    var DropdownSkin = skin || context.skins[_1.IDENTIFIERS.DROPDOWN];
    var _b = this.state,
      isMouseOverItems = _b.isMouseOverItems,
      isMouseOverRoot = _b.isMouseOverRoot;
    return (
      <GlobalListeners_1.GlobalListeners
        mouseIsOverOptions={isMouseOverItems}
        mouseIsOverRoot={isMouseOverRoot}
        optionsIsOpen={this.isOpen()}
        optionsIsOpeningUpward={isOpeningUpward}
        optionsRef={this.optionsElement}
        rootRef={this.rootElement}
        toggleOpen={this.toggleOpen}
      >
        {function (_a) {
          var optionsMaxHeight = _a.optionsMaxHeight,
            optionHeight = _a.optionHeight;
          return (
            <DropdownSkin
              isOpen={_this.isOpen()}
              isOpeningUpward={isOpeningUpward}
              onItemSelected={_this._onItemSelected}
              onLabelClick={_this._onLabelClick}
              optionsRef={_this.optionsElement}
              optionsMaxHeight={optionsMaxHeight}
              optionHeight={optionHeight}
              rootRef={_this.rootElement}
              setMouseOverItems={_this._setMouseOverItems}
              setMouseOverRoot={_this._setMouseOverRoot}
              theme={_this.state.composedTheme}
              {...rest}
            />
          );
        }}
      </GlobalListeners_1.GlobalListeners>
    );
  };
  // define static properties
  DropdownBase.displayName = 'Dropdown';
  DropdownBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    clickToOpen: false,
    isOpeningUpward: false,
    noArrow: false,
    theme: null,
    themeOverrides: {},
    themeId: _1.IDENTIFIERS.DROPDOWN,
  };
  return DropdownBase;
})(react_1.Component);
exports.Dropdown = (0, withTheme_1.withTheme)(DropdownBase);
