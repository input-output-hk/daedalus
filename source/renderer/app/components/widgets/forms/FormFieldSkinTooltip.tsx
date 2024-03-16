// @ts-nocheck
/* eslint-disable */
import React from 'react';
import type { Element } from 'react';
import { omit } from 'lodash';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import exclamationPointIcon from '../../../assets/images/exclamation-point.inline.svg';
import styles from './FormFieldSkinTooltip.scss';
import { PopOver } from 'react-polymorph/lib/components/PopOver';

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
