// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import classnames from 'classnames';
import type { Node, Element } from 'react';
import { Modal } from 'react-polymorph/lib/components/Modal';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { ModalSkin } from 'react-polymorph/lib/skins/simple/ModalSkin';
import styles from './Dialog.scss';
import dialogOverrides from './DialogOverrides.scss';

export type DialogAction = {
  className?: ?string,
  label: string,
  primary?: boolean,
  disabled?: boolean,
  onClick?: Function,
  onDisabled?: Function,
};

type Props = {
  title?: string,
  subtitle?: string | Node,
  children?: Node,
  actions?: Array<DialogAction>,
  closeButton?: ?Element<any>,
  backButton?: Node,
  className?: string,
  onClose?: Function,
  closeOnOverlayClick?: boolean,
  primaryButtonAutoFocus?: boolean,
  isSetWalletPasswordDialog?: boolean,
};

export default class Dialog extends Component<Props> {
  render() {
    const {
      title,
      subtitle,
      children,
      actions,
      closeOnOverlayClick,
      onClose,
      className,
      closeButton,
      backButton,
      primaryButtonAutoFocus,
      isSetWalletPasswordDialog,
    } = this.props;

    return (
      <Modal
        isOpen
        triggerCloseOnOverlayClick={closeOnOverlayClick}
        onClose={onClose}
        skin={ModalSkin}
        themeOverrides={isSetWalletPasswordDialog ? dialogOverrides : null}
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

          {actions && (
            <div className={styles.actions}>
              {map(actions, (action, key) => {
                const buttonClasses = classnames([
                  action.className ? action.className : null,
                  action.primary ? 'primary' : 'flat',
                ]);
                return (
                  <Button
                    key={key}
                    className={buttonClasses}
                    label={action.label}
                    onClick={action.onClick}
                    disabled={action.disabled}
                    skin={ButtonSkin}
                    autoFocus={action.primary ? primaryButtonAutoFocus : false}
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
