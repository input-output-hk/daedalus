import React, { Component } from 'react';
import { Checkbox } from '@react-polymorph/components/Checkbox';
import { CheckboxSkin } from '@react-polymorph/skins/simple/CheckboxSkin';
import { IDENTIFIERS } from '@react-polymorph/themes/API';
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
