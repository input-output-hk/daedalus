// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import type { Element } from 'react';
import styles from './TinyButton.scss';

type Props = $Exact<{
  containerClassName?: string,
  className?: string,
  disabled?: boolean,
  label?: string | Element<any>,
  loading?: boolean,
  onClick?: Function,
}>;

export default class TinyButton extends Component<Props> {
  render() {
    const { containerClassName, loading, ...buttonProps } = this.props;
    const componentClassnames = classnames([
      styles.component,
      containerClassName,
    ]);
    return (
      <div className={componentClassnames}>
        <Button
          themeId={IDENTIFIERS.BUTTON}
          skin={ButtonSkin}
          loading={loading}
          {...buttonProps}
        />
      </div>
    );
  }
}
