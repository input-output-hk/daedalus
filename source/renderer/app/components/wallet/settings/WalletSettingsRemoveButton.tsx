import React from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './RemoveWalletButton.scss';

type Props = {
  onClick: (...args: Array<any>) => any;
  label: string;
  disabled?: boolean;
};

function WalletSettingsRemoveButton({
  onClick,
  label,
  disabled = false,
}: Props) {
  return (
    <Button
      className="flat"
      disabled={disabled}
      label={label}
      onClick={onClick}
      skin={ButtonSkin}
      themeOverrides={styles}
    />
  );
}

export default WalletSettingsRemoveButton;
