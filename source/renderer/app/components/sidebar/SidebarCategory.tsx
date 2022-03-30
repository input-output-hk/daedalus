import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { camelCase } from 'lodash';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl } from 'react-intl';
import type { SidebarCategoryInfo } from '../../config/sidebarConfig';
import styles from './SidebarCategory.scss';
import { messages } from './SidebarCategory.messages';
import type { Intl } from '../../types/i18nTypes';

type Props = {
  category: SidebarCategoryInfo;
  intl: Intl;
  isActive: boolean;
  onClick: (...args: Array<any>) => any;
  content?: Node;
};

function SidebarCategory({
  category,
  intl,
  isActive,
  onClick,
  content,
}: Props) {
  const { name, icon, route, tooltipTextId } = category;
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
    <PopOver
      delay={[100, 500]}
      offset={[0, -20]}
      content={tooltipTextId && intl.formatMessage(messages[tooltipTextId])}
      placement="bottom"
    >
      <button className={componentStyles} onClick={() => onClick(route)}>
        <SVGInline svg={icon} className={iconClassName} />
        {content}
      </button>
    </PopOver>
  );
}

export default injectIntl(observer(SidebarCategory));
