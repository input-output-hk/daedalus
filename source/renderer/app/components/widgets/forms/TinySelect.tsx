import React, { Component } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TinySelect.scss' or its corr... Remove this comment to see the full error message
import styles from './TinySelect.scss';

type Props = {
  value: string;
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
