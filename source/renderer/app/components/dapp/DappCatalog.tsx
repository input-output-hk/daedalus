import React, { ReactNode } from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type { Intl } from '../../types/i18nTypes';
import messages from './DappCatalog.messages';
import styles from './DappCatalog.scss';

const iconClasses: Record<string, string> = {
  liqwid: styles.liqwidIcon,
  unfrack: styles.unfrackIcon,
  strike: styles.strikeIcon,
  fluidtokens: styles.fluidtokensIcon,
  steelswap: styles.steelswapIcon,
};

export type DappCatalogEntry = {
  id: string;
  name: string;
  description: string;
  iconAsset: string;
};

export type DappCatalogProps = {
  entries: readonly DappCatalogEntry[];
  beforeEntries?: ReactNode;
  available: boolean;
  ready: boolean;
  isOpen: boolean;
  launchingId: string | null;
  onLaunch: (id: string) => void;
  onClose: () => void;
};

type Props = DappCatalogProps & { intl: Intl };

export function DappCatalog({
  intl,
  beforeEntries,
  entries,
  available,
  ready,
  isOpen,
  launchingId,
  onLaunch,
  onClose,
}: Props) {
  return (
    <section className={styles.component} aria-labelledby="dapp-catalog-title">
      <header className={styles.header}>
        <h1 id="dapp-catalog-title">{intl.formatMessage(messages.title)}</h1>
      </header>

      {!available ? (
        <p className={styles.unavailable} role="status">
          {intl.formatMessage(messages.unavailable)}
        </p>
      ) : (
        <>
          <p className={styles.disclaimer} role="note">
            {intl.formatMessage(messages.disclaimer)}
          </p>
          {beforeEntries}

          {!ready && !isOpen && (
            <p className={styles.status} role="status">
              {intl.formatMessage(messages.notReady)}
            </p>
          )}

          {isOpen && (
            <div className={styles.openStatus}>
              <span role="status">{intl.formatMessage(messages.open)}</span>
              <Button
                className={styles.closeButton}
                skin={ButtonSkin}
                label={intl.formatMessage(messages.close)}
                onClick={onClose}
              />
            </div>
          )}

          <ul className={styles.entries}>
            {entries.map((entry) => (
              <li className={styles.entry} key={entry.id}>
                <div className={styles.icon} aria-hidden="true">
                  {iconClasses[entry.iconAsset] ? (
                    <span className={iconClasses[entry.iconAsset]} />
                  ) : (
                    <span className={styles.fallbackIcon}>?</span>
                  )}
                </div>
                <div className={styles.details}>
                  <h2>{entry.name}</h2>
                  <p>{entry.description}</p>
                </div>
                <Button
                  className={styles.launchButton}
                  skin={ButtonSkin}
                  label={intl.formatMessage(
                    launchingId === entry.id
                      ? messages.launching
                      : messages.launch
                  )}
                  disabled={!ready || launchingId !== null}
                  onClick={() => onLaunch(entry.id)}
                />
              </li>
            ))}
          </ul>
        </>
      )}
    </section>
  );
}

export default injectIntl(DappCatalog);
