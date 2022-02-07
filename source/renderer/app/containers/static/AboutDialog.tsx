import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import About from '../../components/static/About';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AboutDialog.scss' or its cor... Remove this comment to see the full error message
import styles from './AboutDialog.scss';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class AboutDialog extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { app } = this.props.stores;
    const { openExternalLink, environment } = app;
    const { actions } = this.props;
    const { closeAboutDialog } = actions.app;
    const { apiVersion, nodeVersion, build, os, version } = environment;
    return (
      <ReactModal
        isOpen
        onRequestClose={closeAboutDialog.trigger}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <About
          apiVersion={apiVersion}
          nodeVersion={nodeVersion}
          build={build}
          onOpenExternalLink={openExternalLink}
          os={os}
          version={version}
          onClose={closeAboutDialog.trigger}
        />
      </ReactModal>
    );
  }
}

export default AboutDialog;
