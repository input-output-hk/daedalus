// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { inject } from 'mobx-react/index';
import ReactModal from 'react-modal';
import About from '../../components/static/About';
import styles from './AboutDialog.scss';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class AboutDialog extends Component<InjectedProps> {
  render() {
    return (
      <ReactModal
        isOpen
        shouldCloseOnOverlayClick
        onRequestClose={this.props.actions.dialogs.closeActiveDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <About />
      </ReactModal>
    );
  }
}
