// @flow
import React from 'react';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './ExternalLinkButton.scss';

type Props = {
  label: string,
  onClick: Function,
};

export function ExternalLinkButton({ label, onClick }: Props) {
  return (
    <Link
      label={label}
      skin={LinkSkin}
      isUnderlined={false}
      themeOverrides={styles}
      onClick={onClick}
    />
  );
}
