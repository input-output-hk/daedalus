import React, { Component } from 'react';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { Radio } from '@react-polymorph/components/Radio';
import { RadioSkin } from '@react-polymorph/skins/simple/RadioSkin';
import { observer } from 'mobx-react';
import styles from './RadioSet.scss';
import stylesOverride from './RadioOverride.scss';

type Props = {
  label?: Node;
  items: Array<any>;
  verticallyAligned?: boolean;
};
type RadioProps = {
  key: string;
  disabled?: boolean;
  label?: Node;
  onBlur?: (...args: Array<any>) => any;
  onChange?: (...args: Array<any>) => any;
  onFocus?: (...args: Array<any>) => any;
  selected: boolean;
  skin?: Node;
  theme: Record<string, any> | null | undefined;
  themeOverrides: Record<string, any>;
};

class RadioSet extends Component<Props> {
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

export default observer(RadioSet);
