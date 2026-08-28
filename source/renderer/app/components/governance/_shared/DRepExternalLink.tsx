import React from 'react';
import SVGInline from 'react-svg-inline';
// @ts-ignore inline svg module
import externalLinkIcon from '../../../assets/images/external-link-ic.inline.svg';
import styles from './DRepExternalLink.scss';

interface Props {
  url: string;
  label?: string | null;
  onOpenExternalLink: (url: string) => void;
}

/**
 * A link to somewhere outside Daedalus, marked as one.
 *
 * A real anchor rather than the app's Link component, because the destination
 * has to stay inspectable. These URLs are written by DReps, and a reference
 * renders its label instead of its URI whenever one is supplied, so the words
 * a reader sees need not match where the link goes. An anchor keeps the
 * browser's own defences against that: the status bar on hover, copy link
 * address, and open in a new window to look before committing.
 *
 * The icon is the marker the app already uses for a destination outside itself.
 * Until Daedalus warns before following a third-party link, it is the only
 * signal a reader gets that they are about to leave.
 */
function DRepExternalLink({ url, label, onOpenExternalLink }: Props) {
  return (
    <a
      className={styles.externalLink}
      href={url}
      target="_blank"
      rel="noopener noreferrer"
      onClick={(event: React.MouseEvent<HTMLAnchorElement>) => {
        event.preventDefault();
        onOpenExternalLink(url);
      }}
    >
      <span className={styles.label}>{label ?? url}</span>
      <SVGInline svg={externalLinkIcon} className={styles.icon} />
    </a>
  );
}

export default DRepExternalLink;
