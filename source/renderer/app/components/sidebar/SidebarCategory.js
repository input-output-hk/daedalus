// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { camelCase } from 'lodash';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';
import styles from './SidebarCategory.scss';

type Props = {
  category: SidebarCategoryInfo,
  isActive: boolean,
  onClick: Function,
  content?: Node,
};

@observer
export default class SidebarCategory extends Component<Props> {
  render() {
    const { category, isActive, onClick, content } = this.props;
    const { name, icon, route } = category;
    const className = camelCase(name);
    const componentStyles = classNames(
      styles.component,
      className,
      styles[className],
      {
        [styles.active]: isActive,
      }
    );

    const iconClassName = classNames(styles.icon, styles[`${className}Icon`]);

    return (
      <button className={componentStyles} onClick={() => onClick(route)}>
        <SVGInline svg={icon} className={iconClassName} />
        {content}
      </button>
    );
  }
}
