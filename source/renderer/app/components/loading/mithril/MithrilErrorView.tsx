import React from 'react';
import classNames from 'classnames';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import type {
  MithrilBootstrapError,
  MithrilBootstrapErrorStage,
  MithrilPartialSyncError,
} from '../../../../../common/types/watchdog.types';
import { CollapsibleSection } from '../../widgets/collapsible-section/CollapsibleSection';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_ERROR_HEADING_ID } from './accessibilityIds';
import styles from './MithrilErrorView.scss';

interface Props {
  error?: MithrilBootstrapError | MithrilPartialSyncError | null;
  onOpenExternalLink?: (arg: string) => void;
  title?: string;
  hint?: string | null;
  hintAsBody?: boolean;
  // A rebuild removed the old blockchain data before it failed
  chainRemoved?: boolean;
  // Retry is the only action, so the removal note offers no sync choice
  retryOnly?: boolean;
  actions?: Array<{
    label: string;
    onClick(): void;
    variant?: 'primary' | 'secondary';
  }>;
  // Opt-in right-alignment of the actions footer; default keeps it left-aligned in caller order.
  rightAlignActions?: boolean;
}

interface Context {
  intl: Intl;
}

type ErrorCopy = {
  title: keyof typeof messages;
  hint?: keyof typeof messages;
};

const ERROR_COPY_BY_STAGE: Partial<
  Record<MithrilBootstrapErrorStage, ErrorCopy>
> = {
  download: {
    title: 'errorDownloadTitle',
    hint: 'errorDownloadHint',
  },
  verify: {
    title: 'errorVerifyTitle',
    hint: 'errorVerifyHint',
  },
  convert: {
    title: 'errorConvertTitle',
    hint: 'errorConvertHint',
  },
  'node-start': {
    title: 'errorNodeStartTitle',
    hint: 'startFailureHint',
  },
};

type CodeHint = {
  hint: keyof typeof messages;
  // Used once a rebuild has removed the old data: the same message without
  // its statement that the blockchain data was not changed
  afterRemoval?: keyof typeof messages;
};

// Codes of the errors a rebuild reports before or while it replaces the chain
// data. Each has its own message, which takes precedence over the stage copy.
const ERROR_HINT_BY_CODE: Record<string, CodeHint> = {
  PROBE_FAILED: {
    hint: 'errorProbeFailedHint',
    afterRemoval: 'errorProbeFailedRemovedHint',
  },
  INVALID_CHAIN_PATH: { hint: 'errorInvalidChainPathHint' },
  CHAIN_DIR_NOT_REPLACEABLE: {
    hint: 'errorChainDirNotReplaceableHint',
    afterRemoval: 'errorChainDirNotReplaceableRemovedHint',
  },
  CHAIN_DIR_OTHER_NETWORK: {
    hint: 'errorChainDirOtherNetworkHint',
    afterRemoval: 'errorChainDirOtherNetworkRemovedHint',
  },
  WIPE_FAILED: { hint: 'errorWipeFailedHint' },
  INSUFFICIENT_SPACE: {
    hint: 'errorInsufficientSpaceHint',
    afterRemoval: 'errorInsufficientSpaceRemovedHint',
  },
  REBUILD_MARKER_FAILED: {
    hint: 'errorRebuildMarkerFailedHint',
    afterRemoval: 'errorRebuildMarkerFailedRemovedHint',
  },
  MARKER_FAILED: { hint: 'errorMarkerFailedHint' },
};

const getLogPathHref = (logPath: string) => {
  const normalizedPath = logPath.replace(/\\/g, '/');

  if (/^[A-Za-z]:\//.test(normalizedPath)) {
    return encodeURI(`file:///${normalizedPath}`);
  }

  if (/^[a-zA-Z][a-zA-Z\d+.-]*:/.test(logPath)) return logPath;

  const filePath = normalizedPath.startsWith('/')
    ? normalizedPath
    : `/${normalizedPath}`;

  return encodeURI(`file://${filePath}`);
};

function MithrilErrorView(props: Props, { intl }: Context) {
  const {
    error,
    onOpenExternalLink,
    title,
    hint,
    hintAsBody,
    chainRemoved,
    retryOnly,
    actions,
    rightAlignActions,
  } = props;
  const copy =
    (error?.stage && ERROR_COPY_BY_STAGE[error.stage]) ||
    ({ title: 'errorTitle' } as ErrorCopy);
  const codeCopy = error?.code ? ERROR_HINT_BY_CODE[error.code] : undefined;
  const codeHint =
    chainRemoved && codeCopy?.afterRemoval
      ? codeCopy.afterRemoval
      : codeCopy?.hint;
  let resolvedHint = hint;
  if (resolvedHint === undefined) {
    const hintKey = codeHint ?? copy.hint;
    resolvedHint = hintKey ? intl.formatMessage(messages[hintKey]) : null;
  }
  const hasTechnicalDetails = Boolean(error?.message || error?.code);
  const logPath = error?.logPath;
  const resolvedActions = actions || [];
  // Actions render verbatim in caller order; any reordering is the caller's responsibility.

  return (
    <div className={styles.root} role="alert">
      <div className={styles.header}>
        <h1 id={MITHRIL_ERROR_HEADING_ID}>
          {title || intl.formatMessage(messages[copy.title])}
        </h1>
        {resolvedHint &&
          (hintAsBody ? (
            <p className={styles.hintBody}>{resolvedHint}</p>
          ) : (
            <div className={styles.errorHint}>{resolvedHint}</div>
          ))}
        {chainRemoved && (
          <p className={styles.hintBody}>
            {intl.formatMessage(
              retryOnly
                ? messages.errorChainRemovedRetryHint
                : messages.errorChainRemovedHint
            )}
          </p>
        )}
        {logPath && onOpenExternalLink && (
          <Link
            className={styles.logPathLink}
            label={logPath}
            skin={LinkSkin}
            onClick={() => onOpenExternalLink(getLogPathHref(logPath))}
          />
        )}
      </div>

      {hasTechnicalDetails && (
        <div className={styles.detailsSection}>
          <CollapsibleSection
            header={intl.formatMessage(messages.errorDetailsHeader)}
            headerFontStyle="light"
            expandButtonStyle="link"
          >
            <div className={styles.detailsBody}>
              {error?.code && (
                <div className={styles.errorCode}>{error.code}</div>
              )}
              {error?.message && (
                <pre className={styles.errorMessage}>{error.message}</pre>
              )}
              {logPath && onOpenExternalLink && (
                <Link
                  className={styles.logPathLink}
                  label={logPath}
                  skin={LinkSkin}
                  onClick={() => onOpenExternalLink(getLogPathHref(logPath))}
                />
              )}
            </div>
          </CollapsibleSection>
        </div>
      )}

      <div
        className={classNames([
          styles.actions,
          rightAlignActions && styles.actionsRightAligned,
        ])}
      >
        {resolvedActions.map((action, index) => (
          <Button
            key={`${action.label}-${index}`}
            className={
              action.variant === 'primary'
                ? styles.primaryAction
                : styles.secondaryAction
            }
            skin={ButtonSkin}
            label={action.label}
            onClick={action.onClick}
          />
        ))}
      </div>
    </div>
  );
}

MithrilErrorView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilErrorView;
