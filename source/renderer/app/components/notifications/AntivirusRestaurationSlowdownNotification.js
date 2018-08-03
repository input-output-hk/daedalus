// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import styles from './AntivirusRestaurationSlowdownNotification.scss';
import closeCrossWhite from '../../assets/images/close-cross-white.inline.svg';
import globalMessages from '../../i18n/global-messages';

const messages = defineMessages({
  note: {
    id: 'wallet.statusMessages.antivirusRestaurationSlowdownNotification.note',
    defaultMessage: '!!!<strong>Note:</strong> Antivirus software, like Windows Defender, can cause slow wallet restoration times.',
    description: 'Warning about antivirus software slowing down restoration process on Windows.'
  },
  faqLinkText: {
    id: 'wallet.statusMessages.antivirusRestaurationSlowdownNotification.faqLinkText',
    defaultMessage: '!!!see the FAQ',
    description: '"see the FAQ" link text in the antivirus restoration slowdown notification',
  },
  faqLink: {
    id: 'wallet.statusMessages.antivirusRestaurationSlowdownNotification.faqLink',
    defaultMessage: '!!!Please {faqLink} on the Daedalus website for more information.',
    description: 'Warning about antivirus software slowing down restoration process on Windows.'
  },
});

type Props = {
  onDiscard: () => void,
  onFaqLinkClick: (event: MouseEvent) => void,
};

@observer
export default class AntivirusRestaurationSlowdownNotification extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { onFaqLinkClick } = this.props;
    const { intl } = this.context;

    const faqLink = (
      <a
        href={intl.formatMessage(globalMessages.faqLinkUrl)}
        onClick={event => onFaqLinkClick(event)}
      >
        {intl.formatMessage(messages.faqLinkText)}
      </a>
    );

    const notificationClasses = classnames([
      styles.component,
      'AntivirusRestaurationSlowdownNotification',
    ]);

    return (
      <div className={notificationClasses}>
        <div className={styles.text}>
          <FormattedHTMLMessage {...messages.note} />
          <br />
          <FormattedMessage {...messages.faqLink} values={{ faqLink }} />
        </div>
        <button className={styles.closeButton} onClick={this.props.onDiscard}>
          <SVGInline className={styles.closeCross} svg={closeCrossWhite} />
        </button>
      </div>
    );
  }
}
