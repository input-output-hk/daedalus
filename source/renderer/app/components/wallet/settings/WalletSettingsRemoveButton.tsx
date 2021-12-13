import React from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './RemoveWalletButton.scss' or ... Remove this comment to see the full error message
import styles from './RemoveWalletButton.scss';

type Props = {
  onClick: (...args: Array<any>) => any;
  label: string;
  disabled?: boolean;
};

const WalletSettingsRemoveButton = ({
  onClick,
  label,
  disabled = false,
}: Props) => (
  <Button
    className="flat"
    disabled={disabled}
    label={label}
    onClick={onClick}
    skin={ButtonSkin}
    themeOverrides={styles}
  />
);

export default WalletSettingsRemoveButton;
