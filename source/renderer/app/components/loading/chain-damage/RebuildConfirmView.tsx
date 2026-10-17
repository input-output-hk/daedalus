import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import messages from './ChainDamage.messages';
import styles from './ChainDamageView.scss';

type Props = {
  // A newly chosen location the rebuild is for; it is applied together with
  // the rebuild, so the confirmation names it
  location?: string | null;
  onRebuild: () => void;
  onBack: () => void;
};

type State = {
  isStarted: boolean;
};

// The confirmation that precedes every rebuild of the blockchain data. It is
// the only caller of onRebuild, so a rebuild is never sent without it.
export default class RebuildConfirmView extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state: State = {
    isStarted: false,
  };

  // Once sent, the buttons stay disabled until the Mithril progress view
  // replaces this one, so a second click cannot send it again.
  handleRebuild = () => {
    if (this.state.isStarted) return;
    this.setState({ isStarted: true });
    try {
      this.props.onRebuild();
    } catch (error) {
      // Nothing was started, so the buttons must work again.
      this.setState({ isStarted: false });
      throw error;
    }
  };

  render() {
    const { intl } = this.context;
    const { location, onBack } = this.props;
    const { isStarted } = this.state;

    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.confirmTitle)}</h1>
          <p>
            {location
              ? intl.formatMessage(messages.confirmBodyInLocation, { location })
              : intl.formatMessage(messages.confirmBody)}
          </p>
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.confirmBack)}
            disabled={isStarted}
            onClick={onBack}
          />
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.confirmRebuild)}
            disabled={isStarted}
            onClick={this.handleRebuild}
          />
        </div>
      </div>
    );
  }
}
