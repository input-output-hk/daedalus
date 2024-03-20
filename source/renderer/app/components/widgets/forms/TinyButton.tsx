import React, { Component } from 'react';
import classnames from 'classnames';
import { Button } from '@react-polymorph/components/Button';
import { ButtonSkin } from '@react-polymorph/skins/simple/ButtonSkin';
import { IDENTIFIERS } from '@react-polymorph/themes/API';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element } from 'react';
import styles from './TinyButton.scss';

type Props = {
  containerClassName?: string;
  className?: string;
  disabled?: boolean;
  label?: string | Element<any>;
  loading?: boolean;
  onClick?: (...args: Array<any>) => any;
};
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
