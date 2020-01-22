// @flow
import React, { Component } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinySelect.scss';

type Props = {
  value: string,
};

export default class TinySelect extends Component<Props> {
  render() {
    return (
      <div className={styles.component}>
        <Select
          themeId={IDENTIFIERS.SELECT}
          skin={SelectSkin}
          {...this.props}
        />
      </div>
    );
  }
}
