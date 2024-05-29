// @ts-nocheck
import React, { Component } from 'react';
import type { Node } from 'react';
import { pickBy } from 'lodash';
// components
import { Base } from './Base';
// utilities
import { createEmptyContext, withTheme } from '../HOC/withTheme';
import {
  composeTheme,
  addThemeId,
  didThemePropsChange,
} from '../../utils/themes';
// constants
import { IDENTIFIERS } from '..';
import type { ThemeContextProp } from '../HOC/withTheme';

type Props = {
  alignItems?: string;
  className?: string;
  center?: boolean;
  children?: Node;
  column?: boolean;
  columnReverse?: boolean;
  context: ThemeContextProp;
  justifyContent?: string;
  row?: boolean;
  rowReverse?: boolean;
  theme: Record<string, any> | null | undefined;
  themeId: string;
  themeOverrides: Record<string, any>;
};
type State = {
  composedTheme: Record<string, any>;
};

class FlexBase extends Component<Props, State> {
  // define static properties
  static displayName = 'Flex';
  static defaultProps = {
    context: createEmptyContext(),
    theme: null,
    themeId: IDENTIFIERS.FLEX,
    themeOverrides: {},
  };

  constructor(props: Props) {
    super(props);
    const { context, themeId, theme, themeOverrides } = props;
    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
    };
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  _getActiveClasses = ({ center, column, columnReverse, row, rowReverse }) => {
    const activeClasses = ['container'];
    const activeProps = pickBy({
      center,
      column,
      columnReverse,
      row,
      rowReverse,
    });
    return [...activeClasses, ...Object.keys(activeProps)].filter((val) => val);
  };
  _assembleFlexTheme = (activeClasses: Array<string>) => {
    const theme = this.state.composedTheme[this.props.themeId];
    return activeClasses.reduce((reducedTheme, activeClass) => {
      if (Object.hasOwnProperty.call(theme, activeClass)) {
        reducedTheme[activeClass] = theme[activeClass];
      }

      return reducedTheme;
    }, {});
  };

  renderChildren(theme: Record<string, any>) {
    return React.Children.map(this.props.children, (child) => {
      if (child.type.displayName === 'FlexItem') {
        return React.cloneElement(child, {
          theme,
        });
      }

      return child;
    });
  }

  render() {
    const {
      alignItems,
      className,
      justifyContent,
      themeId,
      ...directionProps
    } = this.props;
    const inlineStyles = pickBy({
      alignItems,
      justifyContent,
    });

    const activeClasses = this._getActiveClasses(directionProps);

    const flexTheme = this._assembleFlexTheme(activeClasses);

    const fullTheme = this.state.composedTheme[themeId];
    return (
      <Base
        activeClasses={activeClasses}
        className={className}
        inlineStyles={inlineStyles}
        stylesToAdd={flexTheme}
      >
        {this.renderChildren(fullTheme)}
      </Base>
    );
  }
}

export const Flex = withTheme(FlexBase);
