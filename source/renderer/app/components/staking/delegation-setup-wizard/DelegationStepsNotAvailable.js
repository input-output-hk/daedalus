// @flow
import React, { Component } from 'react';
import { inject } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import styles from './DelegationStepsNotAvailable.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import attentionImage from '../../../assets/images/attention-dark.inline.svg';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

type State = {};

export default class DelegationStepsNotAvailable extends Component<
  Props,
  State
> {
  render() {
    const { onClose } = this.props;

    const actions = [
      {
        className: 'closeButton',
        label: 'Close',
        onClick: onClose,
        primary: true,
      },
    ];

    return (
      <Dialog
        title="Delegation Setup"
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsNotAvailableWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.content}>
          <SVGInline svg={attentionImage} className={styles.icon} />
          <p className={styles.msgLabel}>Delegation not available</p>
          <p className={styles.description}>
            A wallet with at least <span>1 ada</span> is required for delegation
            setup. Please restore a wallet with ada, or create a new one and
            fund it with ada in order to access delegation features.
          </p>
        </div>
      </Dialog>
    );
  }
}
