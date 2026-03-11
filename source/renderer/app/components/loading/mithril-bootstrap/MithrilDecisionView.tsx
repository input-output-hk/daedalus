import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import MithrilSnapshotDetails from './MithrilSnapshotDetails';
import MithrilSnapshotSelector from './MithrilSnapshotSelector';
import styles from './MithrilDecisionView.scss';

interface Props {
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  isFetchingSnapshots: boolean;
  onSelectSnapshot: (...args: [string | null]) => void;
  onAccept(): void;
  onDecline(): void;
}

interface Context {
  intl: Intl;
}

function MithrilDecisionView(props: Props, { intl }: Context) {
  const {
    snapshots,
    selectedDigest,
    selectedSnapshot,
    isFetchingSnapshots,
    onSelectSnapshot,
    onAccept,
    onDecline,
  } = props;

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1>{intl.formatMessage(messages.title)}</h1>
        <p>{intl.formatMessage(messages.description)}</p>
      </div>

      <MithrilSnapshotSelector
        snapshots={snapshots}
        selectedDigest={selectedDigest}
        isFetchingSnapshots={isFetchingSnapshots}
        onSelectSnapshot={onSelectSnapshot}
      />

      <MithrilSnapshotDetails selectedSnapshot={selectedSnapshot} />

      <div className={styles.actions}>
        <Button
          className={styles.primaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.accept)}
          onClick={onAccept}
        />
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.decline)}
          onClick={onDecline}
        />
      </div>
    </div>
  );
}

MithrilDecisionView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilDecisionView;
