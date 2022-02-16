import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NormalSwitch.scss' or its co... Remove this comment to see the full error message
import styles from './NormalSwitch.scss';

type Props = {
  checked?: boolean;
  label?: string;
  onChange?: (...args: Array<any>) => any;
};
export default function NormalSwitch(props: Props) {
  return (
    <Checkbox
      className={styles.component}
      themeId={IDENTIFIERS.SWITCH}
      skin={SwitchSkin}
      checked={props.checked}
      onChange={(newValue) => {
        if (typeof newValue === 'boolean' && props.checked !== newValue)
          props.onChange(newValue);
      }}
      label={props.label}
    />
  );
}
