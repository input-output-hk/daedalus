import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { camelCase } from 'lodash';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SidebarCategory.scss' or its... Remove this comment to see the full error message
import styles from './SidebarCategory.scss';

type Props = {
  category: SidebarCategoryInfo;
  isActive: boolean;
  onClick: (...args: Array<any>) => any;
  content?: Node;
};

@observer
class SidebarCategory extends Component<Props> {
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

export default SidebarCategory;
