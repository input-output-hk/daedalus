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
const __rest =
  (this && this.__rest) ||
  function (s, e) {
    const t = {};
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
const react_1 = require('react');
// internal utility functions
const withTheme_1 = require('./HOC/withTheme');
const themes_1 = require('../utils/themes');
// import constants
const _1 = require('.');
const GlobalListeners_1 = require('./HOC/GlobalListeners');

const DropdownBase = /** @class */ (function (_super) {
  __extends(DropdownBase, _super);
  function DropdownBase(props) {
    const _this = _super.call(this, props) || this;
    // ========= PUBLIC SKIN API =========
    _this.isOpen = function () {
      const _a = _this.props;
      const { clickToOpen } = _a;
      const { isOpen } = _a;
      const _b = _this.state;
      const { isMouseOverItems } = _b;
      const { isMouseOverRoot } = _b;
      const isOpenBecauseOfHover = clickToOpen
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
        isMouseOverItems,
      });
    };
    _this._setMouseOverRoot = function (isMouseOverRoot) {
      _this.setState({
        isMouseOverRoot,
      });
    };
    _this._onItemSelected = function (item) {
      const { onItemSelected } = _this.props;
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
    _this.rootElement = react_1.default.createRef();
    _this.optionsElement = react_1.default.createRef();
    const { context } = props;
    const { themeId } = props;
    const { theme } = props;
    const { themeOverrides } = props;
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
    const _this = this;
    const _a = this.props;
    const { clickToOpen } = _a;
    const { context } = _a;
    const { isOpen } = _a;
    const { isOpeningUpward } = _a;
    const { onItemSelected } = _a;
    const { skin } = _a;
    const { theme } = _a;
    const { themeOverrides } = _a;
    const rest = __rest(_a, [
      'clickToOpen',
      'context',
      'isOpen',
      'isOpeningUpward',
      'onItemSelected',
      'skin',
      'theme',
      'themeOverrides',
    ]);
    const DropdownSkin = skin || context.skins[_1.IDENTIFIERS.DROPDOWN];
    const _b = this.state;
    const { isMouseOverItems } = _b;
    const { isMouseOverRoot } = _b;
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
          const { optionsMaxHeight } = _a;
          const { optionHeight } = _a;
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
