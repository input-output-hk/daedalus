// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { inject } from 'mobx-react/index';
import ReactModal from 'react-modal';
import About from '../../components/static/About';
import styles from './AboutDialog.scss';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class AboutDialog extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { app } = this.props.stores;
    const { openExternalLink } = app;

    return (
      <ReactModal
        isOpen
        shouldCloseOnOverlayClick
        onRequestClose={this.props.actions.app.closeAboutDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <About onOpenExternalLink={openExternalLink} />
      </ReactModal>
    );
  }
}
