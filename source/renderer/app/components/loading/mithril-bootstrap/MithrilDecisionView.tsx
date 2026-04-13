import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import chainStorageMessages from '../../chain-storage/ChainStorage.messages';
import { getManagedChainDisplayPath } from '../../chain-storage/chainStorageUtils';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_DECISION_HEADING_ID } from './accessibilityIds';
import MithrilSnapshotDetails from './MithrilSnapshotDetails';
import MithrilSnapshotSelector from './MithrilSnapshotSelector';
import styles from './MithrilDecisionView.scss';

interface Props {
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  isFetchingSnapshots: boolean;
  isStorageLocationApplying?: boolean;
  pendingChainPath?: string | null;
  customChainPath?: string | null;
  defaultChainPath?: string | null;
  onSelectSnapshot: (arg: string | null) => void;
  onReturnToStorageLocation?(): void;
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
    isStorageLocationApplying,
    pendingChainPath,
    customChainPath,
    defaultChainPath,
    onSelectSnapshot,
    onReturnToStorageLocation,
    onAccept,
    onDecline,
  } = props;
  const currentStoragePath =
    getManagedChainDisplayPath(
      pendingChainPath !== undefined ? pendingChainPath : customChainPath,
      defaultChainPath
    ) || intl.formatMessage(chainStorageMessages.defaultLocationLabel);
  const isDecisionDisabled = Boolean(isStorageLocationApplying);

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_DECISION_HEADING_ID}>
          {intl.formatMessage(messages.title)}
        </h1>
        <p>{intl.formatMessage(messages.description)}</p>
      </div>

      {onReturnToStorageLocation && (
        <div className={styles.locationContext}>
          <div className={styles.locationSummary}>
            <span className={styles.locationLabel}>
              {intl.formatMessage(chainStorageMessages.directoryLabel)}
            </span>
            <span className={styles.locationValue}>{currentStoragePath}</span>
          </div>
          {!isDecisionDisabled && (
            <Link
              className={styles.locationAction}
              skin={LinkSkin}
              isUnderlined={false}
              underlineOnHover
              label={intl.formatMessage(chainStorageMessages.changeLocation)}
              onClick={onReturnToStorageLocation}
            />
          )}
        </div>
      )}

      <MithrilSnapshotSelector
        snapshots={snapshots}
        selectedDigest={selectedDigest}
        isFetchingSnapshots={isFetchingSnapshots || isDecisionDisabled}
        onSelectSnapshot={onSelectSnapshot}
      />

      <MithrilSnapshotDetails selectedSnapshot={selectedSnapshot} />

      <div className={styles.actions}>
        <Button
          className={styles.primaryAction}
          skin={ButtonSkin}
          disabled={isDecisionDisabled}
          label={intl.formatMessage(messages.accept)}
          onClick={onAccept}
        />
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          disabled={isDecisionDisabled}
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
