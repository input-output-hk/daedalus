// @flow
import React from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DeleteWalletButton.scss';

type Props = {
  onClick: Function,
  label: string,
};

const WalletSettingsActionButton = ({ onClick, label }: Props) => (
  <Button
    className="flat"
    disabled={false}
    label={label}
    onClick={onClick}
    skin={ButtonSkin}
    themeOverrides={styles}
  />
);

export default WalletSettingsActionButton;
