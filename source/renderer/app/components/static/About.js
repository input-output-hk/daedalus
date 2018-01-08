// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { ipcRenderer } from 'electron';
import { defineMessages, intlShape } from 'react-intl';
import styles from './About.scss';
import daedalusIcon from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
import cardanoIcon from '../../assets/images/cardano-logo.inline.svg';

const messages = defineMessages({
  aboutWindowTitle: {
    id: 'window.about.title',
    defaultMessage: '!!!About Daedalus',
    description: 'About Window "title"',
  },
  aboutTitle: {
    id: 'static.about.title',
    defaultMessage: '!!!Daedalus',
    description: 'About "title"',
  },
  aboutReleaseVersion: {
    id: 'static.about.release.version',
    defaultMessage: '!!!0.8.2',
    description: 'Label for "App Release Version"',
  },
  aboutContentDaedalusHeadline: {
    id: 'static.about.content.daedalus.headline',
    defaultMessage: '!!!Daedalus Team:',
    description: 'About page daedalus team headline',
  },
  aboutContentCardanoHeadline: {
    id: 'static.about.content.cardano.headline',
    defaultMessage: '!!!Cardano Team:',
    description: 'About page cardano team headline',
  },
  aboutContentDaedalusMembers: {
    id: 'static.about.content.daedalus.members',
    defaultMessage: '!!!Alexander Rukin, Charles Hoskinson, Darko Mijić, Dominik Guzei, Jeremy Wood, Nikola Glumac, Richard Wild, Tomislav Horaček',
    description: 'About page daedalus team members',
  },
  aboutContentCardanoMembers: {
    id: 'static.about.content.cardano.members',
    defaultMessage: '!!!Alexander Sukhoverkhov, Alexander Vieth, Alexandre Rodrigues Baldé, Alfredo Di Napoli, Anastasiya Besman, Andrzej Rybczak, Ante Kegalj, Anton Belyy, Anupam Jain, Arseniy Seroka, Artyom Kazak, Carlos D\'Agostino, Charles Hoskinson, Dan Friedman, Denis Shevchenko, Dmitry Kovanikov, Dmitry Mukhutdinov, Dmitry Nikulin, Domen Kožar, Duncan Coutts, Edsko de Vries, Eileen Fitzgerald, George Agapov, Hiroto Shioi, Ilya Lubimov, Ilya Peresadin, Ivan Gromakovskii, Jake Mitchell, Jane Wild, Jens Krause, Jeremy Wood, Joel Mislov Kunst, Jonn Mostovoy, Konstantin Ivanov, Kristijan Šarić, Lars Brünjes, Laurie Wang, Lionel Miller, Michael Bishop, Mikhail Volkhov, Niklas Hambüchen, Peter Gaži, Philipp Kant, Serge Kosyrev, Vincent Hanquez',
    description: 'About page cardano team members',
  },
  aboutCopyright: {
    id: 'static.about.copyright',
    defaultMessage: '!!!Input Output HK Limited. Licensed under',
    description: 'About "copyright"',
  },
  licenseLink: {
    id: 'static.about.license',
    defaultMessage: '!!!MIT licence',
    description: 'About page license name',
  },
});

export default class About extends Component<any> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillMount() {
    ipcRenderer.send('about-window-title', this.context.intl.formatMessage(messages.aboutWindowTitle));
  }

  render() {
    const { intl } = this.context;

    return (
      <div className={styles.container}>

        <div className={styles.headerWrapper}>

          <SvgInline svg={daedalusIcon} className={styles.daedalusIcon} />

          <div className={styles.daedalusTitleVersion}>
            <div className={styles.daedalusTitle}>
              {intl.formatMessage(messages.aboutTitle)}
            </div>
            <div className={styles.daedalusVersion}>
              {intl.formatMessage(messages.aboutReleaseVersion)}
            </div>
          </div>

          <SvgInline svg={cardanoIcon} className={styles.cardanoIcon} />
        </div>

        <div className={styles.contentText}>

          <h2>{intl.formatMessage(messages.aboutContentDaedalusHeadline)}</h2>

          <div className={styles.contentDaedalus}>
            {intl.formatMessage(messages.aboutContentDaedalusMembers)}
          </div>

          <h2>{intl.formatMessage(messages.aboutContentCardanoHeadline)}</h2>

          <div className={styles.contentCardanoMembers}>
            {intl.formatMessage(messages.aboutContentCardanoMembers)}
          </div>

        </div>

        <div className={styles.footerWrapper}>
          <a href="http://daedaluswallet.io">http://daedaluswallet.io</a>
          <div className={styles.copyright}>
            {intl.formatMessage(messages.aboutCopyright)}&nbsp;
            <a href="https://github.com/input-output-hk/daedalus/blob/master/LICENSE">
              {intl.formatMessage(messages.licenseLink)}
            </a>
          </div>
        </div>

      </div>
    );
  }
}
