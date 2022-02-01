import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { get } from 'lodash';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletImportFileDialog.scss'... Remove this comment to see the full error message
import styles from './WalletImportFileDialog.scss';
import RadioSet from '../../widgets/RadioSet';
import DialogCloseButton from '../../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/pen.inl... Remove this comment to see the full error message
import penIcon from '../../../assets/images/pen.inline.svg';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { ImportFromOptions } from '../../../types/walletExportTypes';
import type { ImportFromOption } from '../../../types/walletExportTypes';
import Dialog from '../../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';

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
  secretFileLabel: {
    id: 'wallet.import.file.dialog.secretFileLabel',
    defaultMessage: "!!!Select Daedalus 'secret.key' file:",
    description: "Select Daedalus 'secret.key' file:",
  },
  buttonLabel: {
    id: 'wallet.import.file.dialog.buttonLabel',
    defaultMessage: '!!!Import wallets',
    description: 'Import wallets',
  },
  stateDirNoWallets: {
    id: 'wallet.import.file.dialog.stateDirNoWallets',
    defaultMessage:
      '!!!No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
    description:
      'No wallets found. Make sure you have selected a Daedalus state directory which contains the ‘Secrets’ or `Secrets-1.0` folder with a `secret.key` file inside.',
  },
  secretFileNoWallets: {
    id: 'wallet.import.file.dialog.secretFileNoWallets',
    defaultMessage:
      '!!!No wallets found. Make sure you have selected a valid `secret.key` file.',
    description:
      'No wallets found. Make sure you have selected a valid `secret.key` file.',
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
  importFromLabel: {
    id: 'wallet.import.file.dialog.importFromLabel',
    defaultMessage: '!!!Import from:',
    description: 'Import from:',
  },
  stateDirOptionLabel: {
    id: 'wallet.import.file.dialog.stateDirOptionLabel',
    defaultMessage: '!!!Daedalus state directory',
    description: 'Daedalus state directory',
  },
  secretFileOptionLabel: {
    id: 'wallet.import.file.dialog.secretFileOptionLabel',
    defaultMessage: "!!!Daedalus 'secret.key' file",
    description: "Daedalus 'secret.key' file",
  },
});
type Props = {
  exportErrors: string;
  isSubmitting: boolean;
  onOpen: (...args: Array<any>) => any;
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onSelectExportSourcePath: (...args: Array<any>) => any;
  onResetExportSourcePath: (...args: Array<any>) => any;
  exportSourcePath: string;
  defaultExportSourcePath: string;
};
type State = {
  importFrom: ImportFromOption;
};

@observer
class WalletImportFileDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    importFrom: ImportFromOptions.STATE_DIR,
  };
  importPathInput: Input;

  componentDidMount() {
    this.props.onOpen();
  }

  onSetImportFromOption = (importFrom: ImportFromOption) => {
    if (this.state.importFrom !== importFrom) {
      this.props.onResetExportSourcePath();
      this.setState({
        importFrom,
      });
    }
  };

  get input() {
    const fallbackInput = {
      blur: () => {},
      focus: () => {},
    };
    return get(this, 'importPathInput.inputElement.current', fallbackInput);
  }

  isImportFromStateDir = (importFrom: ImportFromOption) =>
    importFrom === ImportFromOptions.STATE_DIR;
  isImportFromSecretFile = (importFrom: ImportFromOption) =>
    importFrom === ImportFromOptions.SECRET_FILE;

  render() {
    const { intl } = this.context;
    const { importFrom } = this.state;
    const {
      exportErrors,
      isSubmitting,
      onContinue,
      onClose,
      onOpenExternalLink,
      onSelectExportSourcePath,
      exportSourcePath,
      defaultExportSourcePath,
    } = this.props;
    const title = intl.formatMessage(messages.title);
    const description = <FormattedHTMLMessage {...messages.description} />;
    const stateFolderLabel = intl.formatMessage(messages.stateFolderLabel);
    const secretFileLabel = intl.formatMessage(messages.secretFileLabel);
    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.buttonLabel)
    ) : (
      <LoadingSpinner />
    );
    const linkLabel = intl.formatMessage(messages.linkLabel);
    const noWalletError = intl.formatMessage(
      messages[`${importFrom}NoWallets`]
    );

    const onLinkClick = () =>
      onOpenExternalLink(intl.formatMessage(messages.linkUrl));

    const error = exportErrors !== '';
    const inputClasses = classNames([
      styles.stateFolderInput,
      error ? styles.error : null,
    ]);
    const buttonClasses = classNames(styles.actionButton, [
      isSubmitting ||
      error ||
      (this.isImportFromSecretFile(importFrom) && !exportSourcePath)
        ? styles.disabled
        : null,
    ]);
    return (
      <Dialog
        className={styles.dialog}
        closeOnOverlayClick={false}
        onClose={onClose}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        shouldCloseOnEsc={false}
        ariaHideApp={false}
        defaultThemeOverrides
      >
        <div className={styles.component}>
          <DialogCloseButton
            icon={closeCrossThin}
            className={styles.closeButton}
            onClose={onClose}
          />
          <div className={styles.content}>
            <div className={styles.title}>{title}</div>
            <div className={styles.description}>{description}</div>
            <div className={styles.radioButtons}>
              <RadioSet
                label={intl.formatMessage(messages.importFromLabel)}
                items={Object.keys(ImportFromOptions).map((key: string) => {
                  const importFromOption: ImportFromOption =
                    ImportFromOptions[key];
                  return {
                    key: importFromOption,
                    label: intl.formatMessage(
                      messages[`${importFromOption}OptionLabel`]
                    ),
                    selected: importFrom === importFromOption,
                    onChange: () =>
                      this.onSetImportFromOption(importFromOption),
                  };
                })}
                verticallyAligned
              />
            </div>
            <div className={styles.stateFolderContainer}>
              <p className={styles.stateFolderLabel}>
                {this.isImportFromStateDir(importFrom)
                  ? stateFolderLabel
                  : secretFileLabel}
              </p>
              <div className={styles.stateFolderInputWrapper}>
                <Input
                  type="text"
                  className={inputClasses}
                  ref={(input) => {
                    this.importPathInput = input;
                  }}
                  skin={InputSkin}
                  value={
                    exportSourcePath ||
                    (this.isImportFromStateDir(importFrom)
                      ? defaultExportSourcePath
                      : '')
                  }
                  placeholder={
                    this.isImportFromStateDir(importFrom)
                      ? defaultExportSourcePath
                      : 'secret.key'
                  }
                  readOnly
                />
                <Button
                  className={styles.selectStateDirectoryButton}
                  onClick={() =>
                    onSelectExportSourcePath({
                      importFrom,
                    })
                  }
                  label={<SVGInline svg={penIcon} className={styles.penIcon} />}
                  skin={ButtonSkin}
                />
              </div>
              {error && <p className={styles.noWalletError}>{noWalletError}</p>}
            </div>
            <div className={styles.action}>
              <Button
                className={buttonClasses}
                disabled={
                  isSubmitting ||
                  error ||
                  (this.isImportFromSecretFile(importFrom) && !exportSourcePath)
                }
                label={buttonLabel}
                onClick={onContinue}
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
      </Dialog>
    );
  }
}

export default WalletImportFileDialog;
