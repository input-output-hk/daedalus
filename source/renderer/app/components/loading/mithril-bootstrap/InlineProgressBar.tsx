import classNames from 'classnames';
import React from 'react';
import { formatTransferSize } from './snapshotFormatting';
import styles from './InlineProgressBar.scss';

type Props = {
  label: string;
  percent: number;
  downloaded?: number;
  total?: number;
  details?: string;
};

function InlineProgressBar({
  label,
  percent,
  downloaded,
  total,
  details,
}: Props) {
  const clamped = Math.min(100, Math.max(0, percent));
  const roundedPercent = Math.round(clamped);
  const downloadedStr = formatTransferSize(downloaded) ?? '\u2014';
  const totalStr = formatTransferSize(total) ?? '\u2014';
  const footerText = details ?? `${downloadedStr} / ${totalStr}`;

  return (
    <div className={styles.inlineBar}>
      <div
        role="progressbar"
        aria-valuenow={roundedPercent}
        aria-valuemin={0}
        aria-valuemax={100}
        aria-label={`${label}: ${roundedPercent}%`}
      >
        <div className={styles.inlineBarMeta}>
          <span>{label}</span>
          <span>{roundedPercent}%</span>
        </div>
        <div className={styles.inlineBarTrack}>
          <div
            className={classNames(styles.inlineBarFill, {
              [styles.inlineBarFillActive]: clamped < 100,
              [styles.inlineBarFillComplete]: clamped >= 100,
            })}
            style={{ width: `${clamped}%` }}
          />
        </div>
      </div>
      <div className={styles.inlineBarMeta}>
        <span>{footerText}</span>
      </div>
    </div>
  );
}

export default InlineProgressBar;
