// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import ReactModal from 'react-modal';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './WalletImportFileDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import penIcon from '../../../assets/images/pen.inline.svg';
import LoadingSpinner from '../../widgets/LoadingSpinner';

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
  noWallets: {
    id: 'wallet.import.file.dialog.noWallets',
    defaultMessage:
      '!!!No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
    description:
      'No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
  },
  linkLabel: {
    id: 'wallet.import.file.dialog.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'wallet.import.file.dialog.linkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900000623463',
    description: '"Learn more" link URL on the wallet import file dialog',
  },
});

type Props = {
  exportErrors: string,
  isSubmitting: boolean,
  onOpen: Function,
  onConfirm: Function,
  onClose: Function,
  onOpenExternalLink: Function,
  onSelectExportSourcePath: Function,
  exportSourcePath: string,
};

@observer
export default class WalletImportFileDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  stateFolderInput: Input;

  UNSAFE_componentWillMount() {
    // Reset migration data
    this.props.onOpen();
  }

  render() {
    const { intl } = this.context;
    const {
      exportErrors,
      isSubmitting,
      onConfirm,
      onClose,
      onOpenExternalLink,
      onSelectExportSourcePath,
      exportSourcePath,
    } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = <FormattedHTMLMessage {...messages.description} />;
    const stateFolderLabel = intl.formatMessage(messages.stateFolderLabel);
    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.buttonLabel)
    ) : (
      <LoadingSpinner />
    );
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const noWalletError = intl.formatMessage(messages.noWallets);
    const onLinkClick = () =>
      onOpenExternalLink(intl.formatMessage(messages.linkUrl));

    const resetErrorCheck =
      this.stateFolderInput &&
      this.stateFolderInput.inputElement.current.value !== exportSourcePath;
    const error = !resetErrorCheck && exportErrors !== '';

    const inputClasses = classNames([
      styles.stateFolderInput,
      error ? styles.error : null,
    ]);

    const buttonClasses = classNames(styles.actionButton, [
      isSubmitting || error ? styles.disabled : null,
    ]);

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
                  className={inputClasses}
                  ref={input => {
                    this.stateFolderInput = input;
                  }}
                  skin={InputSkin}
                  value={exportSourcePath}
                />
                <Button
                  className={styles.selectStateDirectoryButton}
                  onClick={onSelectExportSourcePath}
                  label={<SVGInline svg={penIcon} className={styles.penIcon} />}
                  skin={ButtonSkin}
                />
              </div>
              {error && <p className={styles.noWalletError}>{noWalletError}</p>}
            </div>
            <div className={styles.action}>
              <Button
                className={buttonClasses}
                disabled={isSubmitting || error}
                label={buttonLabel}
                onClick={onConfirm}
                skin={ButtonSkin}
              />
            </div>
            <Link
              className={styles.learnMoreLink}
              disabled={isSubmitting}
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
