import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './BorderedBox.scss' or its cor... Remove this comment to see the full error message
import styles from './BorderedBox.scss';

type Props = {
  children?: Node;
  className?: string;
  fullHeight?: boolean;
  onMouseEnter?: (...args: Array<any>) => any;
  onMouseLeave?: (...args: Array<any>) => any;
};

@observer
class BorderedBox extends Component<Props> {
  render() {
    const {
      children,
      className,
      fullHeight,
      onMouseEnter,
      onMouseLeave,
    } = this.props;
    const componentClasses = classnames([
      styles.component,
      fullHeight ? styles.fullHeight : null,
      className,
    ]);
    return (
      <div
        className={componentClasses}
        onMouseEnter={onMouseEnter}
        onMouseLeave={onMouseLeave}
      >
        {children}
      </div>
    );
  }
}

export default BorderedBox;
