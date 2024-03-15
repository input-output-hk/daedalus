// @ts-nocheck
import React, { Component } from 'react';
// components
import { Base } from './Base';

type Props = {
  alignSelf?: string;
  children?: Node | null | undefined;
  className?: string;
  flex?: number;
  order?: number;
  theme?: Record<string, any>;
};
export class FlexItem extends Component<Props> {
  // define static properties
  static displayName = 'FlexItem';
  static defaultProps = {
    theme: {},
  };

  render() {
    const { children, className, alignSelf, flex, order, theme } = this.props;
    return (
      <Base
        activeClasses={['item']}
        className={className}
        inlineStyles={{
          order,
          alignSelf,
          flex,
        }}
        stylesToAdd={theme}
      >
        {children}
      </Base>
    );
  }
}
