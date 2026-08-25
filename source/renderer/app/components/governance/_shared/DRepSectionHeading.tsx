import React from 'react';
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
 *
 * The explanation reads as text under the heading rather than sitting on an
 * icon. It is the only statement on the page that everything in the off-chain
 * box is the DRep's own claim, and it governs every field below it, so a
 * reader who never hovers an icon still has to meet it. Caveats that qualify
 * one field are a different matter and stay on their icons.
 */
function DRepSectionHeading({ title, explanation }: Props) {
  return (
    <div className={styles.headingBlock}>
      <h2 className={styles.heading}>{title}</h2>
      <p className={styles.explanation}>{explanation}</p>
    </div>
  );
}

export default DRepSectionHeading;
