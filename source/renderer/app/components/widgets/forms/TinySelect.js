// @flow
import React, { Component } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinySelect.scss';

type Props = {
  allowBlank: boolean,
  autoFocus: boolean,
  context: ThemeContextProp,
  error?: string | Element<any>,
  label?: string | Element<any>,
  isOpeningUpward: boolean,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  optionRenderer?: Function,
  options: Array<any>,
  placeholder?: string,
  selectionRenderer?: Function,
  theme: ?Object, // will take precedence over theme in context if passed
  themeOverrides: Object,
  value: string,
};

export default class TinySelect extends Component<Props> {
  render() {
    return (
      <Select
        className={styles.component}
        themeId={IDENTIFIERS.SELECT}
        skin={SelectSkin}
        {...this.props}
      />
    );
  }
}
