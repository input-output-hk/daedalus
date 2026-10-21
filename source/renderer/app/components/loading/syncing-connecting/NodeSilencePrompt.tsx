import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
// The card of the Mithril prompt, which this prompt replaces while both apply.
import styles from './SyncingConnectingMithrilPrompt.scss';

const messages = defineMessages({
  title: {
    id: 'loading.nodeSilence.title',
    defaultMessage: '!!!Cardano node is not reporting progress',
    description:
      'Title of the prompt on the loading screen when Cardano node has written no output for several minutes while it starts',
  },
  body: {
    id: 'loading.nodeSilence.body',
    defaultMessage:
      '!!!Cardano node has reported nothing for {minutes, plural, one {# minute} other {# minutes}} while starting. It may still be working. If it is stuck, restarting it can help.',
    description:
      'Body of the prompt on the loading screen when Cardano node has written no output for the given number of minutes while it starts',
  },
  restart: {
    id: 'loading.nodeSilence.restart',
    defaultMessage: '!!!Restart Cardano node',
    description:
      'Button on the prompt about Cardano node writing no output while it starts, which restarts Cardano node',
  },
  keepWaiting: {
    id: 'loading.nodeSilence.keepWaiting',
    defaultMessage: '!!!Keep waiting',
    description:
      'Button on the prompt about Cardano node writing no output while it starts, which hides the prompt until Cardano node is silent again',
  },
});

type Props = {
  // How long Cardano node has written no output, in seconds
  silentSecs: number;
  onRestart: () => void;
  onKeepWaiting: () => void;
};

export default class NodeSilencePrompt extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { silentSecs, onRestart, onKeepWaiting } = this.props;
    const { intl } = this.context;
    const minutes = Math.max(1, Math.round(silentSecs / 60));

    return (
      <div className={styles.component}>
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <p className={styles.body}>
          {intl.formatMessage(messages.body, { minutes })}
        </p>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            label={intl.formatMessage(messages.keepWaiting)}
            onClick={onKeepWaiting}
            skin={ButtonSkin}
          />
          <Button
            className={styles.primaryAction}
            label={intl.formatMessage(messages.restart)}
            onClick={onRestart}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }
}
