import React from 'react';
import classnames from 'classnames';
import styles from './TextInputSkin.scss';

export default (props) => (
  <div
    className={classnames([
      styles.component,
      props.disabled ? styles.disabled : null,
      props.error ? styles.errored : null,
    ])}
  >
    {props.label && <label className={styles.label}>{props.label}</label>}
    {props.error && <div className={styles.error}>{props.error}</div>}
    <input
      className={styles.input}
      ref={input => props.registerSkinInputElement(input)}
      value={props.value}
      type={props.type || 'text'}
      placeholder={props.placeholder}
      onChange={props.onChange}
      onFocus={props.onFocus}
      onBlur={props.onBlur}
      onKeyPress={props.onKeyPress}
      disabled={props.disabled}
    />
  </div>
);
