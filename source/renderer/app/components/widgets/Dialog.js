// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import classnames from 'classnames';
import type { Node, Element } from 'react';
import { Modal } from 'react-polymorph/lib/components/Modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { ModalSkin } from 'react-polymorph/lib/skins/simple/ModalSkin';
import styles from './Dialog.scss';
import dialogOverridesStyles from './DialogOverride.scss';

export type DialogActionItems = Array<DialogActionItem>;

export type DialogActionItem = {
  className?: ?string,
  label: string,
  primary?: boolean,
  disabled?: boolean,
  onClick?: Function,
  onDisabled?: Function,
  isLink?: boolean,
  hasIconAfter?: boolean,
  hasIconBefore?: boolean,
};

type ActionDirection = 'row' | 'column';

export type DialogActionOptions = {
  items: DialogActionItems,
  direction?: ActionDirection,
};

type Props = {
  title?: string,
  subtitle?: string | Node,
  children?: Node,
  footer?: Node,
  actions?: DialogActionItems | DialogActionOptions,
  closeButton?: ?Element<any>,
  backButton?: Node,
  className?: string,
  defaultThemeOverrides?: boolean,
  customThemeOverrides?: Object,
  onClose?: Function,
  closeOnOverlayClick?: boolean,
  primaryButtonAutoFocus?: boolean,
};

const defaultActionOptions = {
  direction: 'row',
  items: [],
};

export default class Dialog extends Component<Props> {
  render() {
    const {
      title,
      subtitle,
      children,
      footer,
      actions,
      closeOnOverlayClick,
      onClose,
      className,
      closeButton,
      backButton,
      primaryButtonAutoFocus,
      defaultThemeOverrides,
      customThemeOverrides,
    } = this.props;

    const { items, direction } = Array.isArray(actions)
      ? {
          ...defaultActionOptions,
          items: actions,
        }
      : {
          ...defaultActionOptions,
          ...actions,
        };

    const themeOverrides = defaultThemeOverrides
      ? dialogOverridesStyles
      : customThemeOverrides || '';

    const classActionsClasses = classnames([
      styles.actions,
      styles[`${direction}Direction`],
    ]);

    return (
      <Modal
        isOpen
        triggerCloseOnOverlayClick={closeOnOverlayClick}
        onClose={onClose}
        skin={ModalSkin}
        themeOverrides={themeOverrides}
      >
        <div className={classnames([styles.dialogWrapper, className])}>
          {title && (
            <div className={styles.title}>
              <h1>{title}</h1>
            </div>
          )}

          {subtitle && (
            <div className={styles.subtitle}>
              <h1>{subtitle}</h1>
            </div>
          )}

          {children && <div className={styles.content}>{children}</div>}
          {footer && <div className={styles.footer}>{footer}</div>}

          {items && (
            <div className={classActionsClasses}>
              {map(items, (action, key) => {
                const buttonClasses = classnames([
                  action.className ? action.className : null,
                  action.primary ? 'primary' : 'flat',
                ]);
                return !action.isLink ? (
                  <Button
                    key={key}
                    className={buttonClasses}
                    label={action.label}
                    onClick={action.onClick}
                    disabled={action.disabled}
                    skin={ButtonSkin}
                    autoFocus={action.primary ? primaryButtonAutoFocus : false}
                  />
                ) : (
                  <Link
                    className={action.className}
                    onClick={action.onClick}
                    label={action.label}
                    skin={LinkSkin}
                    hasIconAfter={action.hasIconAfter}
                    hasIconBefore={action.hasIconBefore}
                  />
                );
              })}
            </div>
          )}

          {closeButton ? React.cloneElement(closeButton, { onClose }) : null}
          {backButton}
        </div>
      </Modal>
    );
  }
}
