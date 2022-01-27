import React from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import NotificationDot from '../widgets/notification-dot/NotificationDot';
import DiscreetModeFeatureInject from '../../features/discreet-mode/ui/DiscreetModeFeatureInject';
import { messages } from './SidebarCategoryWrapper.messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SidebarCategoryWrapper.scss'... Remove this comment to see the full error message
import styles from './SidebarCategoryWrapper.scss';

type Props = {
  children: Node;
  categoryName: string;
};

const SidebarCategoryWrapper = ({ children, categoryName }: Props) => {
  if (categoryName === CATEGORIES_BY_NAME.SETTINGS.name) {
    const popOverContent = (
      <div className={styles.content}>
        <FormattedHTMLMessage {...messages.label} />
      </div>
    );
    return (
      <DiscreetModeFeatureInject>
        {({ isNotificationEnabled }) => (
          <PopOver
            placement="right"
            className={styles.popOver}
            content={isNotificationEnabled ? popOverContent : null}
          >
            <NotificationDot
              enabled={isNotificationEnabled}
              dotClassName={styles.dot}
            >
              {children}
            </NotificationDot>
          </PopOver>
        )}
      </DiscreetModeFeatureInject>
    );
  }

  return children;
};

export default SidebarCategoryWrapper;
