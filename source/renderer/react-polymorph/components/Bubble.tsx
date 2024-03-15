// @ts-nocheck
import React, { Component } from 'react';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
import { addDocumentListeners, removeDocumentListeners } from '../utils/events';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

export type BubblePosition = {
  width: number;
  positionX: number;
  positionY: number;
};
export type BubbleProps = {
  className?: string;
  context: ThemeContextProp;
  isCentered: boolean;
  isHidden: boolean;
  isFloating: boolean;
  isOpeningUpward: boolean;
  isTransparent: boolean;
  arrowRelativeToTip: boolean;
  noArrow?: boolean;
  skin?: ComponentType<any>;
  theme: Record<string, any> | null | undefined;
  // takes precedence over them in context if passed
  themeId: string;
  themeOverrides: Record<string, any>;
  // custom css/scss from user adhering to component's theme API
  targetRef?: ElementRef<any>; // ref to the target DOM element used for positioning the bubble
};
type State = {
  composedTheme: Record<string, any>;
  position: BubblePosition | null | undefined;
};

class BubbleBase extends Component<BubbleProps, State> {
  // declare ref types
  rootElement: Element<any> | null | undefined;
  // define static properties
  static displayName = 'Bubble';
  static defaultProps = {
    context: createEmptyContext(),
    isCentered: false,
    isHidden: false,
    isFloating: false,
    isOpeningUpward: false,
    isTransparent: true,
    arrowRelativeToTip: false,
    noArrow: false,
    theme: null,
    themeId: IDENTIFIERS.BUBBLE,
    themeOverrides: {},
  };

  constructor(props: BubbleProps) {
    super(props);
    // define ref
    this.rootElement = React.createRef();
    const { context, themeId, theme, themeOverrides } = props;
    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      position: null,
    };
  }

  _hasEventListeners = false;

  componentDidMount() {
    setTimeout(() => {
      if (this.props.isFloating) this._updatePosition();
    }, 0);
  }

  componentDidUpdate(prevProps: BubbleProps) {
    const { isHidden } = this.props;
    const didVisibilityChange = isHidden !== prevProps.isHidden;
    const wasBubbleHidden = !prevProps.isHidden && isHidden;

    if (prevProps.isFloating && !isHidden && !this._hasEventListeners) {
      this._handleScrollEventListener('add');

      addDocumentListeners(this._getDocumentEvents());
      window.addEventListener('resize', this._updatePosition);
      this._hasEventListeners = true;
    }

    if (wasBubbleHidden) this._removeAllEventListeners();
    if (didVisibilityChange) this._updatePosition();

    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  componentWillUnmount() {
    if (this._hasEventListeners) this._removeAllEventListeners();
  }

  // =========== PRIVATE HELPERS ==============
  _handleScrollEventListener = (action: string) => {
    // const rootNode = this.rootElement;
    const { rootElement } = this;

    if (rootElement) {
      const scrollableNode = this._getFirstScrollableParent(rootElement);

      if (scrollableNode) {
        if (action === 'add') {
          scrollableNode.addEventListener('scroll', this._updatePosition);
        } else if (action === 'remove') {
          scrollableNode.removeEventListener('scroll', this._updatePosition);
        }
      }
    }
  };

  _removeAllEventListeners() {
    if (this._hasEventListeners) {
      removeDocumentListeners(this._getDocumentEvents());

      this._handleScrollEventListener('remove');

      window.removeEventListener('resize', this._updatePosition);
      this._hasEventListeners = false;
    }
  }

  _getFirstScrollableParent = (element: ElementRef<any>) => {
    if (element == null) return null;
    const { rootElement } = this;
    const node = {}.hasOwnProperty.call(element, 'current')
      ? element.current
      : element;

    if (rootElement) {
      if (
        node === rootElement.current ||
        node.scrollHeight <= node.clientHeight
      ) {
        return this._getFirstScrollableParent(node.parentElement);
      }
    }

    return node;
  };
  _updatePosition = () => {
    const { isOpeningUpward, targetRef } = this.props;
    const { rootElement } = this;
    let target =
      targetRef && typeof targetRef !== 'string' ? targetRef.current : null;

    // Without a target, try to fallback to the parent node
    if (!target) {
      //  Only proceed if the root element is defined
      if (!rootElement || !rootElement.current) return;
      target = rootElement.current.parentElement;
    }

    const targetRect = target.getBoundingClientRect();
    let positionY;

    if (isOpeningUpward) {
      // Since we don't know the height of the bubble before rendering it we positioning
      // it with { bottom: XYpx } (within the viewport) and need this calculation:
      positionY = window.innerHeight - targetRect.top;
    } else {
      positionY = targetRect.bottom;
    }

    const position = {
      width: targetRect.width,
      positionX: targetRect.left,
      positionY,
    };
    this.setState({
      position,
    });
  };

  _getDocumentEvents() {
    return {
      resize: this._updatePosition,
      scroll: this._updatePosition,
    };
  }

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const { skin, theme, themeOverrides, context, ...rest } = this.props;
    const BubbleSkin = skin || context.skins[IDENTIFIERS.BUBBLE];
    return (
      <BubbleSkin
        rootRef={this.rootElement}
        position={this.state.position}
        theme={this.state.composedTheme}
        {...rest}
      />
    );
  }
}

export const Bubble = withTheme(BubbleBase);
