import React from 'react';
import { intlShape } from 'react-intl';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_SNAPSHOT_SELECTOR_HEADING_ID } from './accessibilityIds';
import {
  formatSnapshotDate,
  formatSnapshotSize,
  truncateDigest,
} from './snapshotFormatting';
import styles from './MithrilSnapshotSelector.scss';

interface Props {
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  isFetchingSnapshots: boolean;
  onSelectSnapshot: (arg: string | null) => void;
}

type SnapshotOption = {
  value: string;
  label: string;
};

interface Context {
  intl: Intl;
}

const LATEST_OPTION_VALUE = 'latest';

const buildSnapshotLabel = (snapshot: MithrilSnapshotItem, locale: string) => {
  const parts = [truncateDigest(snapshot.digest)];
  const createdAt = formatSnapshotDate(snapshot.createdAt, locale);
  const size = formatSnapshotSize(snapshot.size);

  if (createdAt) {
    parts.push(createdAt);
  }

  if (size) {
    parts.push(size);
  }

  return parts.join(' • ');
};

function MithrilSnapshotSelector(props: Props, { intl }: Context) {
  const { snapshots, selectedDigest, isFetchingSnapshots, onSelectSnapshot } =
    props;

  const options: Array<SnapshotOption> = [
    {
      value: LATEST_OPTION_VALUE,
      label: intl.formatMessage(messages.snapshotLatest),
    },
    ...snapshots.map((snapshot) => ({
      value: snapshot.digest,
      label: buildSnapshotLabel(snapshot, intl.locale),
    })),
  ];

  return (
    <div
      className={styles.selectorRow}
      role="group"
      aria-label={intl.formatMessage(messages.snapshotSelectorGroupLabel)}
    >
      <h2
        className={styles.label}
        id={MITHRIL_SNAPSHOT_SELECTOR_HEADING_ID}
        tabIndex={-1}
      >
        {intl.formatMessage(messages.selectLabel)}
      </h2>
      <div>
        <Select
          skin={SelectSkin}
          className={styles.selectInput}
          options={options}
          value={selectedDigest || LATEST_OPTION_VALUE}
          disabled={isFetchingSnapshots}
          onChange={(value) =>
            onSelectSnapshot(
              value === LATEST_OPTION_VALUE || value == null ? null : value
            )
          }
        />
      </div>
    </div>
  );
}

MithrilSnapshotSelector.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilSnapshotSelector;
