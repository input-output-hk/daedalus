// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './BorderedBox.scss';

type Props = {
  children?: Node,
  className?: string,
  fullHeight?: boolean,
  onMouseEnter?: Function,
  onMouseLeave?: Function,
};

@observer
export default class BorderedBox extends Component<Props> {
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
