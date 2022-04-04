import React from 'react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import SVGInline from 'react-svg-inline';
import externalLinkIcon from '../../assets/images/external-link-ic.inline.svg';
import styles from './ExternalLinkButton.scss';

type Props = {
  label: string;
  onClick: (...args: Array<any>) => any;
};
export function ExternalLinkButton({ label, onClick }: Props) {
  const buttonStyles = classnames(['flat', styles.overrideButton]);
  return (
    <Button
      label={
        <div className={styles.labelBlock}>
          {label}
          <SVGInline
            svg={externalLinkIcon}
            className={styles.externalLinkIcon}
          />
        </div>
      }
      className={buttonStyles}
      onClick={onClick}
    />
  );
}
