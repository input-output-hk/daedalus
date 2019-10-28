// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { camelCase } from 'lodash';
import { CATEGORY_TYPES } from '../../config/sidebarConfig.js';
import type { Category } from '../../config/sidebarConfig';
import styles from './SidebarCategory.scss';

type Props = {
  category: Category,
  isActive: boolean,
  onClick: Function,
};

@observer
export default class SidebarCategory extends Component<Props> {
  render() {
    const { category, isActive, onClick } = this.props;
    const { name, icon, route, type = CATEGORY_TYPES.LINK_TYPE } = category;
    const className = camelCase(name);
    const componentStyles = classNames(
      styles.component,
      className,
      styles[className],
      {
        [styles.active]: isActive,
      }
    );

    const onClickFn =
      type === CATEGORY_TYPES.LINK_TYPE ? () => onClick(route) : null;

    const iconClassName = classNames(styles.icon, styles[`${className}Icon`]);

    return (
      <button className={componentStyles} onClick={onClickFn}>
        <SVGInline svg={icon} className={iconClassName} />
      </button>
    );
  }
}
