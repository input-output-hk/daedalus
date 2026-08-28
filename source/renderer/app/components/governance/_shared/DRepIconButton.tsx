import React from 'react';
import SVGInline from 'react-svg-inline';
import styles from './DRepIconButton.scss';

interface Props {
  icon: string;
  label: string;
  onClick: () => void;
  disabled?: boolean;
}

/**
 * An icon-only button.
 *
 * A worded button competes for width with whatever it sits beside, and a
 * full-size one claims more attention than a small repeatable action deserves.
 * The wording moves to the accessible name and the tooltip, where it still
 * reaches anyone who needs it.
 */
function DRepIconButton({ icon, label, onClick, disabled = false }: Props) {
  return (
    <button
      type="button"
      className={styles.iconButton}
      onClick={onClick}
      disabled={disabled}
      aria-label={label}
      title={label}
    >
      <SVGInline svg={icon} />
    </button>
  );
}

export default DRepIconButton;
