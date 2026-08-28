import React from 'react';
import SVGInline from 'react-svg-inline';
// @ts-ignore inline svg module
import infoIcon from '../../../assets/images/info-icon.inline.svg';
// @ts-ignore inline svg module
import warningIcon from '../../../assets/images/warning-triangle.inline.svg';
import styles from './DRepInfoIcon.scss';

interface Props {
  explanation: string;
  // 'warning' says the explanation is not neutral. The glyph and its colour
  // are the only difference: the affordance, the wording and the behaviour
  // stay the same, so a reader learns one control rather than two.
  variant?: 'info' | 'warning';
  className?: string;
}

/**
 * A caveat that is available without being in the way.
 *
 * Provenance and claim warnings are worth stating, but stating them inline
 * between data points pushes the data apart and makes the caveat read as
 * content. They live on an icon beside the thing they qualify instead.
 */
function DRepInfoIcon({ explanation, variant = 'info', className }: Props) {
  return (
    // A button rather than a span with a tabindex: this is focusable and is
    // read as a control, which a decorative image is not.
    <button
      type="button"
      className={[styles.info, styles[variant], className]
        .filter(Boolean)
        .join(' ')}
      aria-label={explanation}
      title={explanation}
    >
      <SVGInline svg={variant === 'warning' ? warningIcon : infoIcon} />
    </button>
  );
}

export default DRepInfoIcon;
