import React from 'react';
// @ts-ignore inline svg module
import copyIcon from '../../../assets/images/copy.inline.svg';
import DRepIconButton from './DRepIconButton';

interface Props {
  onClick: () => void;
  label: string;
}

/**
 * Copy, as an icon.
 *
 * A worded button next to a long monospace value competes with it for width
 * and pushes past the edge of its box. The icon is the convention everywhere
 * else, and the wording moves to the accessible name and the tooltip.
 */
function DRepCopyButton({ onClick, label }: Props) {
  return <DRepIconButton icon={copyIcon} label={label} onClick={onClick} />;
}

export default DRepCopyButton;
