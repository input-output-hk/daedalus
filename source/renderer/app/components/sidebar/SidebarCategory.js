// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
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
    const { icon, active, className, onClick } = this.props;
    const componentStyles = classNames([
      className,
      styles.component,
      active ? styles.active : null,
      styles[className] ? styles[className] : null,
    ]);

    const iconStyles = classNames({
      [styles.supportRequestIcon]: className === 'supportRequest',
      [styles.decentralizationStartIcon]:
        className === 'staking-with-delegation-countdown',
      [styles.decentralizationProgressIcon]:
        className === 'staking-without-delegation-countdown',
      [styles.icon]: className !== 'supportRequest',
    });

    return (
      <button className={componentStyles} onClick={onClick}>
        <SVGInline svg={icon} className={iconStyles} />
      </button>
    );
  }
}
