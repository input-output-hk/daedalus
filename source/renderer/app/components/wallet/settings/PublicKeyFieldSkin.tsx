import React from 'react';
import type { ElementRef } from 'react';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { FormField } from 'react-polymorph/lib/components/FormField';
import type { InputProps } from 'react-polymorph/lib/components/Input';
import { pickDOMProps } from 'react-polymorph/lib/utils/props';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/copy.in... Remove this comment to see the full error message
import copyImage from '../../../assets/images/copy.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PublicKeyField.scss' or its ... Remove this comment to see the full error message
import styles from './PublicKeyField.scss';

type Props = InputProps & {
  inputRef: ElementRef<'input'>;
  theme: Record<string, any>;
  themeId: string;
  tooltip: Node;
  valueVisible: boolean;
  onCopyValue: (...args: Array<any>) => any;
};
export default (props: Props) => {
  const renderInput = () => (
    <input
      ref={props.inputRef}
      {...pickDOMProps(props)}
      className={classnames([
        props.theme[props.themeId].input,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.error || props.showErrorState
          ? props.theme[props.themeId].errored
          : null,
      ])}
      readOnly={props.readOnly}
      onFocus={() => {
        if (props.onFocus) {
          props.onFocus();
        }

        if (props.inputRef && props.inputRef.current) {
          props.inputRef.current.select();
        }

        props.onCopyValue();
      }}
    />
  );

  const render = () =>
    props.valueVisible ? (
      <PopOver content={props.tooltip}>
        <div>
          {renderInput()}
          <CopyToClipboard
            text={props.value}
            onCopy={() => {
              if (props.inputRef && props.inputRef.current) {
                props.inputRef.current.select();
              }

              props.onCopyValue();
            }}
          >
            <Button
              className={classnames([
                styles.imageButton,
                styles.copyButton,
                'flat',
              ])}
              label={<SVGInline svg={copyImage} />}
            />
          </CopyToClipboard>
        </div>
      </PopOver>
    ) : (
      <span>
        <div>{renderInput()}</div>
      </span>
    );

  return (
    <FormField
      className={props.className}
      disabled={props.disabled}
      label={props.label}
      error={props.error}
      inputRef={props.inputRef}
      theme={props.theme}
      render={render}
    />
  );
};
