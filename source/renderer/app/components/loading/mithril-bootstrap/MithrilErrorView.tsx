import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import type {
  MithrilBootstrapError,
  MithrilBootstrapErrorStage,
} from '../../../../../common/types/mithril-bootstrap.types';
import { CollapsibleSection } from '../../widgets/collapsible-section/CollapsibleSection';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_ERROR_HEADING_ID } from './accessibilityIds';
import styles from './MithrilErrorView.scss';

interface Props {
  error?: MithrilBootstrapError | null;
  onOpenExternalLink?: (arg: string) => void;
  onWipeRetry(): void;
  onDecline(): void;
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
  const { error, onOpenExternalLink, onWipeRetry, onDecline } = props;
  const copy =
    (error?.stage && ERROR_COPY_BY_STAGE[error.stage]) ||
    ({ title: 'errorTitle' } as ErrorCopy);
  const hint = copy.hint ? intl.formatMessage(messages[copy.hint]) : null;
  const detailsHeader = error?.message || error?.code || '';
  const logPath = error?.logPath;

  return (
    <div className={styles.root} role="alert">
      <div className={styles.header}>
        <h1 id={MITHRIL_ERROR_HEADING_ID}>
          {intl.formatMessage(messages[copy.title])}
        </h1>
        {error?.message && <p>{error.message}</p>}
        {hint && <div className={styles.errorHint}>{hint}</div>}
        {logPath && onOpenExternalLink && (
          <Link
            className={styles.logPathLink}
            label={logPath}
            skin={LinkSkin}
            onClick={() => onOpenExternalLink(getLogPathHref(logPath))}
          />
        )}
      </div>

      {detailsHeader && (
        <div className={styles.detailsSection}>
          <CollapsibleSection
            header={detailsHeader}
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

      <div className={styles.actions}>
        <Button
          className={styles.primaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.wipeAndRetry)}
          onClick={onWipeRetry}
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

MithrilErrorView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilErrorView;
