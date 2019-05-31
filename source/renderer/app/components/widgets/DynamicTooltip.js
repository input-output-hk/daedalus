// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './DynamicTooltip.scss';

type Props = {
  isVisible: boolean,
  children: any,
  event: any,
};

@observer
export default class DynamicTooltip extends Component<Props> {
  render() {
    const { isVisible, children, event } = this.props;

    console.log('event', event);

    const componentClassnames = classnames([
      styles.component,
      isVisible ? styles.isVisible : null,
    ]);

    return <div className={componentClassnames}>{children}</div>;
  }
}
