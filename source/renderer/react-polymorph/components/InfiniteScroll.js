// @flow
import React, { Component } from 'react';
import type { ComponentType, Node } from 'react';
// $FlowFixMe

// utilities
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// constants
import { IDENTIFIERS } from '.';
import type { ReactElementRef } from '../utils/types.js';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  className?: string,
  context: ThemeContextProp,
  fetchData: Function,
  renderItems?: Function,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  threshold: number,
};

type State = {
  composedTheme: Object,
  data: Object | Array<{}>,
  error: boolean | string | Node,
  hasMoreData: boolean,
  isLoading: boolean,
};

class InfiniteScrollBase extends Component<Props, State> {
  // declare ref types
  scrollContainer: ReactElementRef<typeof HTMLElement>;

  // define static properties
  static displayName = 'InfiniteScroll';
  static defaultProps = {
    context: createEmptyContext(),
    fetchData() {},
    theme: null,
    themeId: IDENTIFIERS.INFINITE_SCROLL,
    themeOverrides: {},
    threshold: 250,
  };

  constructor(props: Props) {
    super(props);
    const { context, themeId, theme, themeOverrides } = props;

    // refs
    this.scrollContainer = React.createRef();

    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      data: [],
      error: false,
      isLoading: false,
      hasMoreData: true,
    };
  }

  componentDidMount() {
    const { scrollContainer } = this;

    this._handleFetchData();
    if (!scrollContainer.current) return;
    scrollContainer.current.addEventListener('scroll', this._handleScroll);
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  // calls user's fetchData function from props
  _handleFetchData = () => this.props.fetchData(this.setState.bind(this));

  // scroll event listener attached to scrollContainer element
  _handleScroll = () => {
    const { error, isLoading, hasMoreData } = this.state;

    // return early for error, loading, or lack of future data
    if (error || isLoading || !hasMoreData) {
      return;
    }
    return this._checkForScrollBottom();
  };

  // prevents new data fetch until user has scrolled near bottom of existing data
  _checkForScrollBottom = () => {
    const {
      scrollContainer,
      props: { threshold },
    } = this;
    if (!scrollContainer.current) return;
    const { offsetHeight, scrollTop, scrollHeight } = scrollContainer.current;

    if (offsetHeight + scrollTop >= scrollHeight - threshold) {
      return this._handleFetchData();
    }
  };

  _isFunction = (renderProp: ?Function) =>
    renderProp && typeof renderProp === 'function';

  render() {
    const {
      props: { className, context, renderItems, skin, themeId },
      state: { composedTheme, data, error, hasMoreData, isLoading },
      scrollContainer,
    } = this;

    if (!this._isFunction(renderItems)) {
      return null;
    }
    const InfiniteScrollSkin =
      skin || context.skins[IDENTIFIERS.INFINITE_SCROLL];

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
  }
}

export const InfiniteScroll = withTheme(InfiniteScrollBase);
