// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import type { Node } from 'react';
import { Radio } from 'react-polymorph/lib/components/Radio';
import { RadioSkin } from 'react-polymorph/lib/skins/simple/RadioSkin';
import { observer } from 'mobx-react';
import styles from './RadioSet.scss';
import stylesOverride from './RadioOverride.scss';

type Props = {
  label?: Node,
  items: Array<any>,
  verticallyAligned?: boolean,
};

type RadioProps = $Exact<{
  key: string,
  disabled?: boolean,
  label?: Node,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  selected: boolean,
  skin?: Node,
  theme: ?Object,
  themeOverrides: Object,
}>;

@observer
export default class RadioSet extends Component<Props> {
  render() {
    const { label, items, verticallyAligned } = this.props;
    const radiosContainerStyles = classnames([
      styles.radiosContainer,
      verticallyAligned ? styles.verticallyAligned : null,
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <div className={radiosContainerStyles}>
          {items.map(({ key, ...item }: RadioProps) => (
            <Radio
              skin={RadioSkin}
              key={key}
              {...item}
              themeOverrides={stylesOverride}
            />
          ))}
        </div>
      </div>
    );
  }
}
