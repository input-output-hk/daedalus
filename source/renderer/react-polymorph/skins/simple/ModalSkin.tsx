// @ts-nocheck
import React from 'react';
import type { Node, Element } from 'react';
// external libraries
import ReactModal from 'react-modal';

type Props = {
  children?: Node | null | undefined;
  contentLabel: string | Element<any>;
  isOpen: boolean;
  onClose: (...args: Array<any>) => any;
  triggerCloseOnOverlayClick: boolean;
  theme: Record<string, any>;
  themeId: string;
};
export function ModalSkin(props: Props) {
  return (
    <ReactModal
      contentLabel={props.contentLabel}
      isOpen={props.isOpen}
      onRequestClose={props.onClose}
      shouldCloseOnOverlayClick={props.triggerCloseOnOverlayClick}
      className={props.theme[props.themeId].modal}
      overlayClassName={props.theme[props.themeId].overlay}
      ariaHideApp={false}
    >
      {props.children}
    </ReactModal>
  );
}
