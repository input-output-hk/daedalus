// @flow
import React, { Component } from 'react';
// $FlowFixMe
import type { Element } from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinyButton.scss';

type Props = $Exact<{
  className?: string,
  disabled?: boolean,
  label?: string | Element<any>,
  loading: boolean,
  onClick?: Function,
}>;

export default class TinyButton extends Component<Props> {
  render() {
    return (
      <div className={styles.component}>
        <Button
          themeId={IDENTIFIERS.BUTTON}
          skin={ButtonSkin}
          {...this.props}
        />
      </div>
    );
  }
}
