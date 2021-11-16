// @flow
import React from 'react';
import type { Node } from 'react';
import { CATEGORIES_BY_NAME } from '../../config/sidebarConfig';
import NotificationPopOver from '../widgets/notification-popover/NotificationPopOver';
import { useDiscreetModeFeature } from '../../features/discreet-mode';

type Props = {
  children: Node,
  categoryName: string,
};

const SidebarCategoryWrapper = ({ children, categoryName }: Props) => {
  const discreetModeFeature = useDiscreetModeFeature();

  if (categoryName === CATEGORIES_BY_NAME.SETTINGS.name) {
    return (
      <NotificationPopOver
        visible={discreetModeFeature.isSettingsTooltipEnabled}
        content="New discreet mode settings are available in Security tab"
        placement="right"
        offset={[0, -5]}
        onDismiss={() => {
          discreetModeFeature.setDiscreetModeSettingsTooltip(false);
        }}
      >
        {children}
      </NotificationPopOver>
    );
  }

  return children;
};

export default SidebarCategoryWrapper;
