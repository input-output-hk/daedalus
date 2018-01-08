// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarCategory.scss';

type Props = {
  icon: string,
  active: boolean,
  onClick: Function,
  className: string,
};

@observer
export default class SidebarCategory extends Component<Props> {
  render() {
    const { icon, active, onClick, className } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      className
    ]);
    return (
      <button className={componentStyles} onClick={onClick}>
        <SvgInline svg={icon} className={styles.icon} />
      </button>
    );
  }

}
