// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import globalMessages from '../../../../i18n/global-messages';

import styles from './PaperWalletCreateCertificateCompletionDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.completion.dialog.headline',
    defaultMessage: '!!!Paper wallet certificate',
    description: 'Headline for the "Paper wallet create certificate completion dialog" headline.'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.completion.dialog.subtitle',
    defaultMessage: '!!!Now you can fold your paper wallet certificate and glue together all the parts. Keep certificate safe. To import wallet back crop glued certificate’s edges to reach inner part.',
    description: 'Headline for the "Paper wallet create certificate completion dialog" subtitle.'
  },
  linkInstructions: {
    id: 'paper.wallet.create.certificate.completion.dialog.linkInstructions',
    defaultMessage: '!!!You can use this link to opent the address in Cardano Explorer, save it in your browser bookmarks and share it with others to receive funds:',
    description: 'Headline for the "Paper wallet create certificate completion dialog" link instructions.'
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

@observer
export default class PaperWalletCreateCertificateCompletionDialog extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, onBack, onContinue } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificateCompletionDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        onClick: onContinue,
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >

        <div className={styles.securingPasswordContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.content}>

            <p className={styles.linkInstructions}>
              {intl.formatMessage(messages.linkInstructions)}
            </p>

            <div className={styles.cardanoExplorerLinkWrapper}>
              <a href="https://cardanoexplorer.com/address/DdzFFzCqrht3AggeFyqhiikmB3KRKjwnk6tfWSPQ8V229GbhCjuy9USHVWMVFa5oPcMrSqGtH1wf2sGqpabnwdGD2MBR6gKjkvhLjsmZ">
                https://cardanoexplorer.com/address/DdzFFzCqrht3AggeFyqhiikmB3KRKjwnk6tfWSPQ8V229GbhCjuy9USHVWMVFa5oPcMrSqGtH1wf2sGqpabnwdGD2MBR6gKjkvhLjsmZ
              </a>
            </div>

          </div>
        </div>

      </Dialog>
    );
  }

}
