// @ts-nocheck
import React, { Component } from 'react';
import type { Node } from 'react';
import classnames from 'classnames';
// styles
import baseStyles from '../../themes/helpers/Base.scss';
// utilities
import { composeBaseStyles } from '../../utils/layout';

type Props = {
  activeClasses: Array<any>;
  children?: Node | null | undefined;
  className?: string;
  inlineStyles: Record<string, any>;
  stylesToAdd?: Record<string, any>;
};
type State = {
  composedStyles: Record<string, any>;
};
export class Base extends Component<Props, State> {
  // define static properties
  static displayName = 'Base';
  static defaultProps = {
    inlineStyles: {},
    stylesToAdd: {},
  };

  constructor(props: Props) {
    super(props);
    const { activeClasses, stylesToAdd } = props;
    this.state = {
      composedStyles: composeBaseStyles(baseStyles, stylesToAdd, activeClasses),
    };
  }

  render() {
    const { className, children, inlineStyles } = this.props;
    const { composedStyles } = this.state;
    return (
      <div
        style={inlineStyles}
        className={classnames([className, composedStyles.base])}
      >
        {children}
      </div>
    );
  }
}
