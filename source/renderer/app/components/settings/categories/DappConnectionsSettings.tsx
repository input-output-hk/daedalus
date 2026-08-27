import React from 'react';
import { injectIntl } from 'react-intl';
import type { DappConnectionScope } from '../../../../../common/ipc/api';
import type { DappGrant } from '../../../../../common/types/dapp.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './DappConnectionsSettings.messages';
import styles from './DappConnectionsSettings.scss';

export type DappConnectionRow = Readonly<{
  grant: DappGrant;
  walletName: string;
}>;

export type DappConnectionsSettingsProps = Readonly<{
  connections: readonly DappConnectionRow[];
  corrupt: boolean;
  loading: boolean;
  failed: boolean;
  onDisconnect: (grant: DappGrant) => void;
  onForget: (grant: DappGrant) => void;
  onRevoke: (grant: DappGrant, scope: DappConnectionScope) => void;
  onRepair: () => void;
}>;
type Props = DappConnectionsSettingsProps & Readonly<{ intl: Intl }>;
const elevatedScopes: readonly DappConnectionScope[] = [
  'governance-key-disclosure',
  'account-public-key-disclosure',
];

export function DappConnectionsSettings({
  intl,
  connections,
  corrupt,
  loading,
  failed,
  onDisconnect,
  onForget,
  onRevoke,
  onRepair,
}: Props) {
  const scopeLabel = (scope: DappConnectionScope) =>
    intl.formatMessage(
      scope === 'governance-key-disclosure'
        ? messages.cip95
        : messages.cip104Legacy
    );

  return (
    <section className={styles.root} aria-labelledby="dapp-connections-title">
      <h1 id="dapp-connections-title">{intl.formatMessage(messages.title)}</h1>
      <ul className={styles.explanation}>
        <li>{intl.formatMessage(messages.closeDescription)}</li>
        <li>{intl.formatMessage(messages.disconnectDescription)}</li>
        <li>{intl.formatMessage(messages.forgetDescription)}</li>
      </ul>
      <p>{intl.formatMessage(messages.cip104Unavailable)}</p>

      {failed && (
        <p className={styles.status} role="status">
          {intl.formatMessage(messages.failed)}
        </p>
      )}

      {corrupt && (
        <div className={styles.corrupt} role="alert">
          <p>{intl.formatMessage(messages.corrupt)}</p>
          <button type="button" disabled={loading} onClick={onRepair}>
            {intl.formatMessage(messages.repair)}
          </button>
        </div>
      )}
      {!corrupt && connections.length === 0 && (
        <p className={styles.status} role="status">
          {intl.formatMessage(messages.empty)}
        </p>
      )}
      {!corrupt && connections.length > 0 && (
        <ul className={styles.connections}>
          {connections.map(({ grant, walletName }) => {
            const activeScopes = elevatedScopes.filter((scope) =>
              grant.readScopes.includes(scope)
            );
            return (
              <li
                className={styles.connection}
                key={`${grant.walletId}:${grant.networkGenesis}:${
                  grant.origin
                }:${JSON.stringify(grant.launch)}`}
              >
                <h2>{grant.origin}</h2>
                <dl>
                  <dt>{intl.formatMessage(messages.wallet)}</dt>
                  <dd>{walletName}</dd>
                  <dt>{intl.formatMessage(messages.networkMagic)}</dt>
                  <dd>{grant.networkMagic}</dd>
                  <dt>{intl.formatMessage(messages.source)}</dt>
                  <dd>
                    {intl.formatMessage(
                      grant.launch.kind === 'catalog'
                        ? messages.catalog
                        : messages.diagnostics
                    )}
                  </dd>
                  <dt>{intl.formatMessage(messages.granted)}</dt>
                  <dd>
                    {new Date(grant.grantedAt).toLocaleDateString(intl.locale)}
                  </dd>
                </dl>
                <p>{intl.formatMessage(messages.baseAccess)}</p>
                {activeScopes.map((scope) => (
                  <div className={styles.scope} key={scope}>
                    <span>{scopeLabel(scope)}</span>
                    <button
                      type="button"
                      disabled={loading}
                      onClick={() => onRevoke(grant, scope)}
                    >
                      {intl.formatMessage(messages.revoke, {
                        origin: grant.origin,
                        scope: scopeLabel(scope),
                      })}
                    </button>
                  </div>
                ))}
                <div className={styles.actions}>
                  <button
                    type="button"
                    disabled={loading}
                    onClick={() => onDisconnect(grant)}
                  >
                    {intl.formatMessage(messages.disconnect, {
                      origin: grant.origin,
                    })}
                  </button>
                  <button
                    type="button"
                    disabled={loading}
                    onClick={() => onForget(grant)}
                  >
                    {intl.formatMessage(messages.forget, {
                      origin: grant.origin,
                    })}
                  </button>
                </div>
              </li>
            );
          })}
        </ul>
      )}
    </section>
  );
}

export default injectIntl(DappConnectionsSettings);
