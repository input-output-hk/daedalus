import React, { Component } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DisplaySettings.scss' or its... Remove this comment to see the full error message
import styles from './DisplaySettings.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeIncentivizedTestnetPreview from '../../../assets/images/themes/incentivized-testnet.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeCardanoPreview from '../../../assets/images/themes/cardano.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeDarkBluePreview from '../../../assets/images/themes/dark-blue.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeDarkCardanoPreview from '../../../assets/images/themes/dark-cardano.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeFlightCandidatePreview from '../../../assets/images/themes/flight-candidate.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeLightBluePreview from '../../../assets/images/themes/light-blue.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeShelleyTestnetPreview from '../../../assets/images/themes/shelley-testnet.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeYellowPreview from '../../../assets/images/themes/yellow.png';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/themes/... Remove this comment to see the full error message
import themeWhitePreview from '../../../assets/images/themes/white.png';
import { THEMES } from '../../../themes/index';

const messages = defineMessages({
  themeLabel: {
    id: 'settings.display.themeLabel',
    defaultMessage: '!!!Theme',
    description:
      'Label for the "Theme" selection on the display settings page.',
  },
  themeIncentivizedTestnet: {
    id: 'settings.display.themeNames.incentivizedTestnet',
    defaultMessage: '!!!Incentivized Testnet',
    description:
      'Name of the "Incentivized Testnet" theme on the display settings page.',
  },
  themeLightBlue: {
    id: 'settings.display.themeNames.lightBlue',
    defaultMessage: '!!!Light blue',
    description: 'Name of the "Light blue" theme on the display settings page.',
  },
  themeCardano: {
    id: 'settings.display.themeNames.cardano',
    defaultMessage: '!!!Cardano',
    description: 'Name of the "Cardano" theme on the display settings page.',
  },
  themeDarkBlue: {
    id: 'settings.display.themeNames.darkBlue',
    defaultMessage: '!!!Dark blue',
    description: 'Name of the "Dark blue" theme on the display settings page.',
  },
  themeDarkCardano: {
    id: 'settings.display.themeNames.darkCardano',
    defaultMessage: '!!!Dark Cardano',
    description:
      'Name of the "Dark cardano" theme on the display settings page.',
  },
  themeFlightCandidate: {
    id: 'settings.display.themeNames.flightCandidate',
    defaultMessage: '!!!Flight Candidate',
    description:
      'Name of the "Flight Candidate" theme on the display settings page.',
  },
  themeShelleyTestnet: {
    id: 'settings.display.themeNames.shelleyTestnet',
    defaultMessage: '!!!Shelley Testnet',
    description:
      'Name of the "Shelley Testnet" theme on the display settings page.',
  },
  themeYellow: {
    id: 'settings.display.themeNames.yellow',
    defaultMessage: '!!!Yellow',
    description: 'Name of the "Yellow" theme on the display settings page.',
  },
  themeWhite: {
    id: 'settings.display.themeNames.white',
    defaultMessage: '!!!White',
    description: 'Name of the "White" theme on the display settings page.',
  },
});
type Props = {
  theme: string;
  selectTheme: (...args: Array<any>) => any;
};

@observer
class DisplaySettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { theme, selectTheme } = this.props;
    const { intl } = this.context;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
    const { isFlight, environment } = global;
    const { isDev } = environment;
    const themeIncentivizedTestnetClasses = classnames([
      theme === THEMES.INCENTIVIZED_TESTNET ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeLightBlueClasses = classnames([
      theme === THEMES.LIGHT_BLUE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeCardanoClasses = classnames([
      theme === THEMES.CARDANO ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeDarkBlueClasses = classnames([
      theme === THEMES.DARK_BLUE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeDarkCardanoClasses = classnames([
      theme === THEMES.DARK_CARDANO ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeFlightCandidateClasses = classnames([
      theme === THEMES.FLIGHT_CANDIDATE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeShelleyTestnetClasses = classnames([
      theme === THEMES.SHELLEY_TESTNET ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeYellowClasses = classnames([
      theme === THEMES.YELLOW ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    const themeWhiteClasses = classnames([
      theme === THEMES.WHITE ? styles.active : styles.inactive,
      styles.themeImageWrapper,
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.label}>
          {intl.formatMessage(messages.themeLabel)}
        </div>

        <div className={styles.themesRowWrapper}>
          <button
            className={themeLightBlueClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.LIGHT_BLUE,
            })}
          >
            <img
              src={themeLightBluePreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeLightBlue)}</span>
          </button>

          <button
            className={themeCardanoClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.CARDANO,
            })}
          >
            <img
              src={themeCardanoPreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeCardano)}</span>
          </button>

          <button
            className={themeWhiteClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.WHITE,
            })}
          >
            <img
              src={themeWhitePreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeWhite)}</span>
          </button>
        </div>

        <div className={styles.themesRowWrapper}>
          <button
            className={themeDarkBlueClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.DARK_BLUE,
            })}
          >
            <img
              src={themeDarkBluePreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeDarkBlue)}</span>
          </button>

          <button
            className={themeDarkCardanoClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.DARK_CARDANO,
            })}
          >
            <img
              src={themeDarkCardanoPreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeDarkCardano)}</span>
          </button>

          <button
            className={themeYellowClasses}
            onClick={selectTheme.bind(this, {
              theme: THEMES.YELLOW,
            })}
          >
            <img
              src={themeYellowPreview}
              role="presentation"
              draggable="false"
            />
            <span>{intl.formatMessage(messages.themeYellow)}</span>
          </button>
        </div>

        <div className={styles.themesRowWrapper}>
          {isDev && (
            <button
              className={themeIncentivizedTestnetClasses}
              onClick={selectTheme.bind(this, {
                theme: THEMES.INCENTIVIZED_TESTNET,
              })}
            >
              <img
                src={themeIncentivizedTestnetPreview}
                role="presentation"
                draggable="false"
              />
              <span>
                {intl.formatMessage(messages.themeIncentivizedTestnet)}
              </span>
            </button>
          )}

          {(isDev || isFlight) && (
            <button
              className={themeFlightCandidateClasses}
              onClick={selectTheme.bind(this, {
                theme: THEMES.FLIGHT_CANDIDATE,
              })}
            >
              <img
                src={themeFlightCandidatePreview}
                role="presentation"
                draggable="false"
              />
              <span>{intl.formatMessage(messages.themeFlightCandidate)}</span>
            </button>
          )}

          {isDev && (
            <button
              className={themeShelleyTestnetClasses}
              onClick={selectTheme.bind(this, {
                theme: THEMES.SHELLEY_TESTNET,
              })}
            >
              <img
                src={themeShelleyTestnetPreview}
                role="presentation"
                draggable="false"
              />
              <span>{intl.formatMessage(messages.themeShelleyTestnet)}</span>
            </button>
          )}
        </div>
      </div>
    );
  }
}

export default DisplaySettings;
