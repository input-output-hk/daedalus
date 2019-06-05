// @flow
import React, { Component } from 'react';
import { inject } from 'mobx-react';
import styles from './DelegationStepsChooseWallet.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

type Props = InjectedContainerProps;

type State = {};

export default class DelegationStepsChooseWallet extends Component<
  Props,
  State
> {
  render() {
    const { onClose, onContinue } = this.props;

    const actions = [
      {
        className: 'closeButton',
        label: 'Cancel',
        onClick: onClose,
      },
      {
        className: 'continueButton',
        label: 'Continue',
        onClick: onContinue,
        primary: true,
      },
    ];

    return (
      <Dialog
        title="Delegation Setup"
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsChooseWalletWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.content}>
          <p className={styles.description}>
            Follow next sequence of screens to configure delegation for your
            wallet. During this process, you will need to deposit and pay
            transaction fees.
          </p>
          <button
            onClick={() => {
              console.debug('Learn more');
            }}
          >
            Learn more
          </button>
          <div className={styles.stepsExplanation}>
            <p className={styles.label}>
              You will need to complete the following steps:
            </p>
            <ol>
              <li>
                <span>1.</span> Choose a wallet
              </li>
              <li>
                <span>2.</span> Choose a stake pool
              </li>
              <li>
                <span>3.</span> Choose a stake pool
              </li>
              <li>
                <span>4.</span> Move all of the ada to a new address{' '}
                <span className={styles.optional}>(optional)</span>
              </li>
            </ol>
          </div>
        </div>
      </Dialog>
    );
  }
}
