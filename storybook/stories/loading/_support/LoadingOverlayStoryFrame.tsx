import React from 'react';
import styles from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss';

type Props = {
  children: React.ReactNode;
};

export default function LoadingOverlayStoryFrame({ children }: Props) {
  return (
    <div className={styles.component}>
      <div className={styles.backdrop} />
      <div className={styles.content}>
        <div className={styles.card} role="dialog" aria-modal="true">
          {children}
        </div>
      </div>
    </div>
  );
}
