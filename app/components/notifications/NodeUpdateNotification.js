import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import styles from './NodeUpdateNotification.scss';
import arrow from '../../assets/images/arrow.svg';

const messages = defineMessages({
  acceptLabel: {
    id: 'cardano.node.update.notification.accept.button.label',
    defaultMessage: '!!!Accept',
    description: 'Label "Accept" on the Cardano node update notification.'
  },
  denyLabel: {
    id: 'cardano.node.update.notification.postpone.button.label',
    defaultMessage: '!!!Ask me later',
    description: 'Label "Ask me later" on the Cardano node update notification.'
  }
});

@observer
export default class NodeUpdateNotification extends Component {

  static propTypes = {
    title: PropTypes.string.isRequired,
    message: PropTypes.string, // TODO: make this required after it is implemented on the backend
    onAccept: PropTypes.func.isRequired,
    onPostpone: PropTypes.func.isRequired,
    onToggleExpanded: PropTypes.func.isRequired,
    isExpanded: PropTypes.bool.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { title, message, onAccept, onPostpone, onToggleExpanded, isExpanded } = this.props;
    const arrowClasses = classnames([
      isExpanded ? styles.arrow : styles.arrowCollapsed
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.titleBar}>
          <div className={styles.title}>{title}</div>
          <button
            onClick={onToggleExpanded}
            className={styles.toggleButton}
          >
            <img className={arrowClasses} src={arrow} role="presentation" />
          </button>
        </div>
        {isExpanded && message && (
          // eslint-disable-next-line react/no-danger
          <div className={styles.message} dangerouslySetInnerHTML={{ __html: message }} />
        )}
        {isExpanded && (
          <div className={styles.actions}>
            <button
              className={styles.acceptButton}
              onClick={onAccept}
            >
              {intl.formatMessage(messages.acceptLabel)}
            </button>
            <button
              className={styles.denyButton}
              onClick={onPostpone}
            >
              {intl.formatMessage(messages.denyLabel)}
            </button>
          </div>
        )}

      </div>
    );
  }
}
