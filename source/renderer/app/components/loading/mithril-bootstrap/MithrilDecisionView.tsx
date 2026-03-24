import React, { useEffect, useRef } from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import type { Intl } from '../../../types/i18nTypes';
import chainStorageMessages from '../../chain-storage/ChainStorage.messages';
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
    customChainPath,
    defaultChainPath,
    onSelectSnapshot,
    onReturnToStorageLocation,
    onAccept,
    onDecline,
  } = props;
  const currentStoragePath =
    customChainPath ||
    defaultChainPath ||
    intl.formatMessage(chainStorageMessages.defaultLocationLabel);
  const headingRef = useRef<HTMLHeadingElement>(null);

  useEffect(() => {
    headingRef.current?.focus();
  }, []);

  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_DECISION_HEADING_ID} ref={headingRef} tabIndex={-1}>
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
          <Link
            className={styles.locationAction}
            skin={LinkSkin}
            isUnderlined={false}
            underlineOnHover
            label={intl.formatMessage(chainStorageMessages.changeLocation)}
            onClick={onReturnToStorageLocation}
          />
        </div>
      )}

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
