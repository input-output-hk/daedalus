// @flow
import React from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { injectIntl, FormattedHTMLMessage } from 'react-intl';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import NotificationPopOver from '../widgets/notification-popover/NotificationPopOver';
import { useDiscreetModeFeature } from '../../features/discreet-mode';
import { messages } from './SidebarCategoryWrapper.messages';
import type { Intl } from '../../types/i18nTypes';
import styles from './SidebarCategoryWrapper.scss';

type Props = {
  children: Node,
  categoryName: string,
  intl: Intl,
};

const SidebarCategoryWrapper = ({ children, categoryName, intl }: Props) => {
  const discreetModeFeature = useDiscreetModeFeature();

  if (categoryName === CATEGORIES_BY_NAME.SETTINGS.name) {
    return (
      <NotificationPopOver
        className={styles.popOver}
        visible={discreetModeFeature.isSettingsTooltipEnabled}
        dismissLabel={intl.formatMessage(messages.dismissLabel)}
        content={<FormattedHTMLMessage {...messages.label} />}
        placement="right"
        offset={[-10, -5]}
        onDismiss={() =>
          discreetModeFeature.setDiscreetModeSettingsTooltip(false)
        }
      >
        {children}
      </NotificationPopOver>
    );
  }

  return children;
};

export default injectIntl(observer(SidebarCategoryWrapper));
