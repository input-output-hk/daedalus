// @flow
import React from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { FormattedHTMLMessage } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import { useDiscreetModeFeature } from '../../features/discreet-mode';
import NotificationDot from '../widgets/notification-dot/NotificationDot';
import { messages } from './SidebarCategoryWrapper.messages';
import styles from './SidebarCategoryWrapper.scss';

type Props = {
  children: Node,
  categoryName: string,
};

const SidebarCategoryWrapper = ({ children, categoryName }: Props) => {
  const { isNotificationEnabled } = useDiscreetModeFeature();
  if (
    categoryName === CATEGORIES_BY_NAME.SETTINGS.name &&
    isNotificationEnabled
  ) {
    return (
      <PopOver
        placement="right"
        className={styles.popOver}
        content={
          <div className={styles.content}>
            <FormattedHTMLMessage {...messages.label} />
          </div>
        }
      >
        <NotificationDot
          enabled={isNotificationEnabled}
          dotClassName={styles.dot}
        >
          {children}
        </NotificationDot>
      </PopOver>
    );
  }

  return children;
};

export default observer(SidebarCategoryWrapper);
