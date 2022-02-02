import React, { Component } from 'react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element } from 'react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TinyButton.scss' or its corr... Remove this comment to see the full error message
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
