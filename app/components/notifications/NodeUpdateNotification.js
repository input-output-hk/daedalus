// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Button from 'react-polymorph/lib/components/Button';
import SimpleButtonSkin from 'react-polymorph/lib/skins/simple/ButtonSkin';
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
    defaultMessage: '!!!Postpone until restart',
    description: 'Label "Ask me later" on the Cardano node update notification.'
  }
});

@observer
export default class NodeUpdateNotification extends Component {

  props: {
    title: string,
    message?: string, // TODO: make this required after it is implemented on the backend
    onAccept: Function,
    onPostpone: Function,
    onToggleExpanded: Function,
    isExpanded: boolean,
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

            <Button
              className={styles.acceptButton}
              label={intl.formatMessage(messages.acceptLabel)}
              onClick={onAccept}
              skin={<SimpleButtonSkin />}
            />

            <Button
              className={styles.denyButton}
              label={intl.formatMessage(messages.denyLabel)}
              onClick={onPostpone}
              skin={<SimpleButtonSkin />}
            />

          </div>
        )}

      </div>
    );
  }
}
