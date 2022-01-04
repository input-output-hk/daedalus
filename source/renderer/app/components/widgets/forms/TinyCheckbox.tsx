import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TinyCheckbox.scss' or its co... Remove this comment to see the full error message
import styles from './TinyCheckbox.scss';

type Props = {
  checked?: boolean;
  label?: string;
  onChange?: (...args: Array<any>) => any;
};
export default class TinyCheckbox extends Component<Props> {
  render() {
    return (
      <Checkbox
        className={styles.component}
        themeId={IDENTIFIERS.CHECKBOX}
        skin={CheckboxSkin}
        {...this.props}
      />
    );
  }
}
