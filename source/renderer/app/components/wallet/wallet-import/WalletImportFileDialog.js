// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import ReactModal from 'react-modal';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import SVGInline from 'react-svg-inline';
import styles from './WalletImportFileDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import penIcon from '../../../assets/images/pen.inline.svg';

const messages = defineMessages({
  title: {
    id: 'wallet.import.file.dialog.title',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets dialog title',
  },
  description: {
    id: 'wallet.import.file.dialog.description',
    defaultMessage:
      '!!!<p>This feature enables you to import wallets from the production version of Daedalus, or from the Daedalus state directory. </p> <p>If you don’t have the complete state directory, then you will need either the ‘Secrets’ or ‘Secrets-1.0’ folder containing the ‘secret.key’ file to be able to import a wallet, although without the complete state directory Daedalus won’t be able to detect your wallet names. </p> <p>If you don’t have either the ‘Secrets’ or the ‘Secrets-1.0’ folder containing the ‘secret.key’ file, then you cannot import wallets using this feature.</p>',
    description:
      '<p>This feature enables you to import wallets from the production version of Daedalus, or from the Daedalus state directory. </p> <p>If you don’t have the complete state directory, then you will need either the ‘Secrets’ or ‘Secrets-1.0’ folder containing the ‘secret.key’ file to be able to import a wallet, although without the complete state directory Daedalus won’t be able to detect your wallet names. </p> <p>If you don’t have either the ‘Secrets’ or the ‘Secrets-1.0’ folder containing the ‘secret.key’ file, then you cannot import wallets using this feature.</p>',
  },
  stateFolderLabel: {
    id: 'wallet.import.file.dialog.stateFolderLabel',
    defaultMessage: '!!!Select Daedalus state folder:',
    description: 'Select Daedalus state folder:',
  },
  buttonLabel: {
    id: 'wallet.import.file.dialog.buttonLabel',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets',
  },
  linkLabel: {
    id: 'wallet.import.file.dialog.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'wallet.import.file.dialog.linkUrl',
    defaultMessage: '!!!http://cardano.org/',
    description: '"Learn more" link URL on the wallet import file dialog',
  },
});

type Props = {
  onConfirm: Function,
  onClose: Function,
  openExternalLink: Function,
  onSelectStateDirectory: Function,
  stateDirectoryPath: string,
};

export default class WalletImportFileDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  stateFolderInput: Input;
  search: string;

  confirm = () => {
    this.props.onConfirm();
  };

  render() {
    const { intl } = this.context;
    const {
      onConfirm,
      onClose,
      openExternalLink,
      onSelectStateDirectory,
      stateDirectoryPath,
    } = this.props;
    this.search = stateDirectoryPath;
    const title = intl.formatMessage(messages.title);
    const description = <FormattedHTMLMessage {...messages.description} />;
    const stateFolderLabel = intl.formatMessage(messages.stateFolderLabel);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const onLinkClick = () =>
      openExternalLink(intl.formatMessage(messages.linkUrl));

    return (
      <ReactModal
        isOpen
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onClose}
          />
          <div className={styles.backgroundContainer} />
          <div className={styles.content}>
            <div className={styles.title}>{title}</div>
            <div className={styles.description}>{description}</div>
            <div className={styles.stateFolderContainer}>
              <p className={styles.stateFolderLabel}>{stateFolderLabel}</p>
              <div className={styles.stateFolderInputWrapper}>
                <Input
                  type="text"
                  className={styles.stateFolderInput}
                  ref={input => {
                    this.stateFolderInput = input;
                  }}
                  skin={InputSkin}
                  value={this.search}
                />
                <button
                  className={styles.selectStateDirectoryButton}
                  onClick={onSelectStateDirectory}
                >
                  <SVGInline svg={penIcon} className={styles.penIcon} />
                </button>
              </div>
            </div>
            <div className={styles.action}>
              <Button
                className={styles.actionButton}
                label={buttonLabel}
                onClick={onConfirm}
                skin={ButtonSkin}
              />
            </div>
            <Link
              className={styles.learnMoreLink}
              onClick={onLinkClick}
              label={linkLabel}
              skin={LinkSkin}
            />
          </div>
        </div>
      </ReactModal>
    );
  }
}
