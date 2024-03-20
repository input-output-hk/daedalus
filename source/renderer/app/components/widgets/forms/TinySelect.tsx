import React, { Component } from 'react';
import { Select } from '@react-polymorph/components/Select';
import { SelectSkin } from '@react-polymorph/skins/simple/SelectSkin';
import { IDENTIFIERS } from '@react-polymorph/themes/API';
import styles from './TinySelect.scss';

type Props = {
  value: string;
  onChange: (...args: any) => void;
  placeholder?: string;
  options: Array<{
    label: string;
    value: string;
  }>;
};
export default class TinySelect extends Component<Props> {
  render() {
    return (
      <div className={styles.component}>
        <Select
          themeId={IDENTIFIERS.SELECT}
          skin={SelectSkin}
          optionHeight={33}
          {...this.props}
        />
      </div>
    );
  }
}
