import React from 'react';
import moment from 'moment';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DRepDirectoryBanner.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title banner for DRep directory',
  },
  refresh: {
    id: 'governance.drepDirectory.refresh',
    defaultMessage: '!!!Refresh',
    description: 'Refresh button label',
  },
  lastUpdated: {
    id: 'governance.drepDirectory.lastUpdated',
    defaultMessage: '!!!Last updated {time}',
    description: 'Last updated timestamp label',
  },
});

interface Props {
  lastFetchedAt: number | null;
  onRefresh: () => void;
  isRefreshing: boolean;
  intl: intlShape.isRequired;
}

function DRepDirectoryBanner({
  lastFetchedAt,
  onRefresh,
  isRefreshing,
  intl,
}: Props) {
  const timeAgo = lastFetchedAt ? moment(lastFetchedAt).fromNow() : null;

  return (
    <div className={styles.banner}>
      <div className={styles.headerRow}>
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <Button
          label={intl.formatMessage(messages.refresh)}
          onClick={onRefresh}
          disabled={isRefreshing}
          skin={ButtonSkin}
        />
      </div>
      {lastFetchedAt && timeAgo !== null && (
        <p className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
        </p>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryBanner);
