/* eslint-disable */
import React from 'react';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element } from 'react';
import { omit } from 'lodash';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/exclama... Remove this comment to see the full error message
import exclamationPointIcon from '../../../assets/images/exclamation-point.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './FormFieldSkinTooltip.scss' o... Remove this comment to see the full error message
import styles from './FormFieldSkinTooltip.scss';
type Props = {
  className: string;
  disabled: boolean;
  error: string | Element<any>;
  focusChild: (...args: Array<any>) => any;
  label: string | Element<any>;
  onChange: (...args: Array<any>) => any;
  render: (...args: Array<any>) => any;
  setError: (...args: Array<any>) => any;
  theme: Record<string, any>;
  themeId: string;
};
export const FormFieldSkin = (props: Props) => (
  <div
    className={classnames([
      styles.component,
      props.className,
      props.theme[props.themeId].root,
      props.disabled ? props.theme[props.themeId].disabled : null,
      props.error ? props.theme[props.themeId].errored : null,
    ])}
  >
    {props.label && (
      <label
        role="presentation"
        aria-hidden
        className={props.theme[props.themeId].label}
        onClick={props.focusChild}
      >
        {props.label}
        {props.error && (
          <PopOver content={props.error} key="tooltip" placement="bottom">
            <SVGInline
              svg={exclamationPointIcon}
              className={styles.exclamationPointIcon}
            />
          </PopOver>
        )}
      </label>
    )}
    <div className={props.theme[props.themeId].inputWrapper}>
      {props.render(omit(props, ['themeId']))}
    </div>
  </div>
);
