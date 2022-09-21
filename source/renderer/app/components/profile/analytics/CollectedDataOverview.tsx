import React, { FC } from 'react';
import { injectIntl } from 'react-intl';
import { CollapsibleSection } from '../../widgets/collapsible-section/CollapsibleSection';
import styles from './CollectedDataOverview.scss';
import { messages } from './CollectedDataOverview.messages';

export const CollectedDataOverview: FC = injectIntl(({ intl }) => {
  return (
    <CollapsibleSection
      header={intl.formatMessage(messages.title)}
      expandMessage={messages.expandButton}
      collapseMessage={messages.collapseButton}
      expandButtonStyle="link"
      headerFontStyle="light"
    >
      <ol className={styles.dataCollectionList}>
        <li>
          <h3>{intl.formatMessage(messages.userBehaviorTitle)}</h3>
          <p>{intl.formatMessage(messages.userBehaviorText)}</p>
        </li>
        <li>
          <h3>{intl.formatMessage(messages.deviceInfoTitle)}</h3>
          <p>{intl.formatMessage(messages.deviceInfoText)}</p>
        </li>
      </ol>
    </CollapsibleSection>
  );
});
