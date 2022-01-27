import React, { Component } from 'react';
import { map } from 'lodash';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node, Element, ElementRef } from 'react';
import { Modal } from 'react-polymorph/lib/components/Modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { ModalSkin } from 'react-polymorph/lib/skins/simple/ModalSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Dialog.scss' or its correspo... Remove this comment to see the full error message
import styles from './Dialog.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DialogOverride.scss' or its ... Remove this comment to see the full error message
import dialogOverrides from './DialogOverride.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DialogFullSizeOverride.scss'... Remove this comment to see the full error message
import dialogFullSizeOverride from './DialogFullSizeOverride.scss';

export type DialogActionItems = Array<DialogActionItem>;
export type DialogActionItem = {
  className?: string | null | undefined;
  label: string | Node;
  primary?: boolean;
  disabled?: boolean;
  onClick?: (...args: Array<any>) => any;
  onDisabled?: (...args: Array<any>) => any;
  isLink?: boolean;
  hasIconAfter?: boolean;
  hasIconBefore?: boolean;
};
type ActionDirection = 'row' | 'column';
export type DialogActionOptions = {
  items: DialogActionItems;
  direction?: ActionDirection;
};
export type DialogActions = DialogActionItems | DialogActionOptions;
type Props = {
  title?: string | Element<any>;
  subtitle?: string | Node;
  children?: Node;
  footer?: Node;
  actions?: DialogActions;
  closeButton?: Element<any> | null | undefined;
  backButton?: Node;
  className?: string;
  defaultThemeOverrides?: boolean;
  onClose?: (...args: Array<any>) => any;
  closeOnOverlayClick?: boolean;
  primaryButtonAutoFocus?: boolean;
  fullSize?: boolean;
  scrollWrapperRef?: ElementRef<any> | null | undefined;
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
      fullSize,
      scrollWrapperRef,
    } = this.props;
    const { items, direction } = Array.isArray(actions)
      ? { ...defaultActionOptions, items: actions }
      : { ...defaultActionOptions, ...actions };
    let themeOverrides;
    if (defaultThemeOverrides) themeOverrides = dialogOverrides;
    else if (fullSize) themeOverrides = dialogFullSizeOverride;
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

          {children && (
            <div
              className={styles.contentWrapper}
              ref={(ref) => {
                // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
                if (scrollWrapperRef) scrollWrapperRef.current = ref;
              }}
            >
              <div className={styles.content}>{children}</div>
            </div>
          )}
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
                    key={key}
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

          {closeButton
            ? React.cloneElement(closeButton, {
                onClose,
              })
            : null}
          {backButton}
        </div>
      </Modal>
    );
  }
}
