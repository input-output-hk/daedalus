import React, { Component } from 'react';
import _ from 'lodash';
import classnames from 'classnames';
import type { Node } from 'react';
import Modal from 'react-polymorph/lib/components/Modal';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SimpleModalSkin from 'react-polymorph/lib/skins/simple/ModalSkin';
import styles from './Dialog.scss';

type Props = {
  title?: string,
  children?: Node,
  actions?: Node,
  closeButton?: Node,
  backButton?: Node,
  className?: string,
  onClose?: Function,
  closeOnOverlayClick?: boolean,
};

export default class Dialog extends Component<Props> {

  render() {
    const {
      title,
      children,
      actions,
      closeOnOverlayClick,
      onClose,
      className,
      closeButton,
      backButton,
    } = this.props;

    return (
      <Modal
        isOpen
        triggerCloseOnOverlayClick={closeOnOverlayClick}
        onClose={onClose}
        skin={<SimpleModalSkin />}
      >

        <div className={classnames([styles.dialogWrapper, className])}>
          {title &&
            <div className={styles.title}>
              <h1>{title}</h1>
            </div>
          }

          {children &&
            <div className={styles.content}>
              {children}
            </div>
          }

          {actions &&
            <div className={styles.actions}>
              {_.map(actions, (action, key) => {
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
                    skin={<SimpleButtonSkin />}
                  />
                );
              })}
            </div>
          }

          {closeButton ? React.cloneElement(closeButton, { onClose }) : null}
          {backButton}

        </div>
      </Modal>
    );
  }
}
