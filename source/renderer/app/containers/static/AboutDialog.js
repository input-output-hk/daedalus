// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
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
    const { openExternalLink, environment } = app;
    const { apiVersion, build, os, version } = environment;

    return (
      <ReactModal
        isOpen
        closeOnOverlayClick
        onRequestClose={this.props.actions.app.closeAboutDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <About
          apiVersion={apiVersion}
          build={build}
          onOpenExternalLink={openExternalLink}
          os={os}
          version={version}
        />
      </ReactModal>
    );
  }
}
