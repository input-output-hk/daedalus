// @flow
import React, { Component } from 'react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinyCheckbox.scss';

type Props = {|
  checked?: boolean,
  label?: string,
  onChange?: Function,
|};

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
