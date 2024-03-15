// @flow
import React, { Component } from 'react';
import type { ComponentType, Element } from 'react';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';
import { GlobalListeners } from './HOC/GlobalListeners';

type Props = {
  activeItem: any,
  className?: string,
  clickToOpen?: boolean,
  context: ThemeContextProp,
  isOpen?: boolean,
  isOpeningUpward: boolean,
  items: Array<any>,
  label: string | Element<any>,
  noArrow?: boolean,
  onItemSelected?: Function,
  optionRenderer?: Function,
  skin?: ComponentType<any>,
  theme: ?Object,
  themeId: string,
  themeOverrides: Object,
};

type State = {
  composedTheme: Object,
  isMouseOverItems: boolean,
  isMouseOverRoot: boolean,
  isOpen: boolean,
};

class DropdownBase extends Component<Props, State> {
  // declare ref types
  rootElement: ?Element<*>;
  optionsElement: ?Element<*>;

  // define static properties
  static displayName = 'Dropdown';
  static defaultProps = {
    context: createEmptyContext(),
    clickToOpen: false,
    isOpeningUpward: false,
    noArrow: false,
    theme: null,
    themeOverrides: {},
    themeId: IDENTIFIERS.DROPDOWN,
  };

  constructor(props: Props) {
    super(props);

    // define ref
    this.rootElement = React.createRef();
    this.optionsElement = React.createRef();

    const { context, themeId, theme, themeOverrides } = props;
    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      isMouseOverItems: false,
      isMouseOverRoot: false,
      isOpen: false,
    };
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  componentWillUnmount() {
    this.close();
  }

  // ========= PUBLIC SKIN API =========

  isOpen = () => {
    const { clickToOpen, isOpen } = this.props;
    const { isMouseOverItems, isMouseOverRoot } = this.state;
    const isOpenBecauseOfHover = clickToOpen
      ? false
      : isMouseOverItems || isMouseOverRoot;
    return isOpen || this.state.isOpen || isOpenBecauseOfHover;
  };

  toggleOpen = () => {
    if (this.isOpen()) {
      this.close();
    } else {
      this.setState({ isOpen: true });
    }
  };

  close = () => {
    this._setMouseOverRoot(false);
    this._setMouseOverItems(false);
    this.setState({ isOpen: false });
  };

  // ========= PRIVATE SKIN API =========

  _setMouseOverItems = (isMouseOverItems: boolean) => {
    this.setState({ isMouseOverItems });
  };

  _setMouseOverRoot = (isMouseOverRoot: boolean) => {
    this.setState({ isMouseOverRoot });
  };

  _onItemSelected = (item) => {
    const { onItemSelected } = this.props;
    this.close();
    if (onItemSelected) {
      onItemSelected(item);
    }
  };

  _onLabelClick = () => {
    if (this.props.clickToOpen) {
      this.toggleOpen();
    }
  };

  render() {
    const {
      clickToOpen,
      context,
      isOpen,
      isOpeningUpward,
      onItemSelected,
      skin,
      theme,
      themeOverrides,
      ...rest
    } = this.props;

    const DropdownSkin = skin || context.skins[IDENTIFIERS.DROPDOWN];
    const { isMouseOverItems, isMouseOverRoot } = this.state;

    return (
      <GlobalListeners
        mouseIsOverOptions={isMouseOverItems}
        mouseIsOverRoot={isMouseOverRoot}
        optionsIsOpen={this.isOpen()}
        optionsIsOpeningUpward={isOpeningUpward}
        optionsRef={this.optionsElement}
        rootRef={this.rootElement}
        toggleOpen={this.toggleOpen}
      >
        {({ optionsMaxHeight, optionHeight }) => (
          <DropdownSkin
            isOpen={this.isOpen()}
            isOpeningUpward={isOpeningUpward}
            onItemSelected={this._onItemSelected}
            onLabelClick={this._onLabelClick}
            optionsRef={this.optionsElement}
            optionsMaxHeight={optionsMaxHeight}
            optionHeight={optionHeight}
            rootRef={this.rootElement}
            setMouseOverItems={this._setMouseOverItems}
            setMouseOverRoot={this._setMouseOverRoot}
            theme={this.state.composedTheme}
            {...rest}
          />
        )}
      </GlobalListeners>
    );
  }
}

export const Dropdown = withTheme(DropdownBase);
