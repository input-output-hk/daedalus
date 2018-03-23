// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import styles from './CompletionDialog.scss';

const shell = require('electron').shell;

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
    defaultMessage: '!!!You can use this link to open the address in Cardano Explorer, save it in your browser bookmarks and share it with others to receive funds:',
    description: 'Headline for the "Paper wallet create certificate completion dialog" link instructions.'
  },
  finishButtonLabel: {
    id: 'paper.wallet.create.certificate.completion.dialog.finishButtonLabel',
    defaultMessage: '!!!Finish',
    description: '"Paper wallet create certificate completion dialog" finish button label.'
  },
});

type Props = {
  walletCertificateAddress: string,
  onFinish: Function,
};

@observer
export default class CompletionDialog extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onFinish, walletCertificateAddress } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'completionDialog',
    ]);

    const actions = [
      {
        className: 'finishButton',
        label: intl.formatMessage(messages.finishButtonLabel),
        primary: true,
        onClick: onFinish,
      }
    ];

    const cardanoExplorerLink = `https://cardanoexplorer.com/address/${walletCertificateAddress}`;

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
      >

        <div className={styles.securingPasswordContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.content}>

            <p className={styles.linkInstructions}>
              {intl.formatMessage(messages.linkInstructions)}
            </p>

            <div className={styles.cardanoExplorerLinkWrapper}>
              <a
                href={cardanoExplorerLink}
                onClick={this.openCardanoExplorer.bind(this, cardanoExplorerLink)}
              >
                {cardanoExplorerLink}
              </a>
            </div>

          </div>
        </div>

      </Dialog>
    );
  }

  openCardanoExplorer = (link: string, e: Object) => {
    e.preventDefault();
    shell.openExternal(link);
  };
}
