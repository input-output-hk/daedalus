import React from 'react';
import DRepInfoIcon from './DRepInfoIcon';
import styles from './DRepSectionHeading.scss';

interface Props {
  title: string;
  explanation: string;
}

/**
 * A section heading that says where the section's contents came from.
 *
 * The detail view already separates what the ledger reports from what the DRep
 * published about itself into two boxes. Naming each box is a clearer way to
 * carry that distinction than repeating a small provenance label beside
 * individual fields, which said nothing their neighbours did not share.
 */
function DRepSectionHeading({ title, explanation }: Props) {
  return (
    <h2 className={styles.heading}>
      <span>{title}</span>
      <DRepInfoIcon explanation={explanation} />
    </h2>
  );
}

export default DRepSectionHeading;
