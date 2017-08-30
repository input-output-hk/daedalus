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
    defaultMessage: '!!!2017.0.1 x64 Release',
    description: 'Label for "App Release Version"',
  },
  aboutContentText: {
    id: 'static.about.content.text',
    defaultMessage: '!!!Charles Hoskinson, Jeremy Wood, Aggelos Kiayias, Eileen Fitzgerald, Philip Wadler, Elias Koutsoupias, Mario Larangeira, Bernardo David, Peter Gaži, Rafael Dowsley, Roman Oliynykov, Dmitry Shtukenberg, Duncan Coutts, Lars Brünjes, Philipp Kant, Peter Thompson, Darryl McAdams, Ante Kegalj, Jens Krause, Kristijan Šarić, Denis Shevchenko, Alfredo Di Napoli, Jonn Mostovoy, Arseniy Seroka, Alexander Vieth, Mikhail Volkhov, George Agapov, Ivan Gromakovskii, Alexandre Baldé, Artyom Kazak, Dmitry Kovanikov, Alan McSherry, Alan Verbner, Nicolas Tallar, Lukasz Gasior, Adam Smolarek, Radek Tkaczyk, Alexander Chepurnoy, Dmitry Meshkov, Jan Kotek, Darko Mijić, Dominik Guzei, Nikola Glumac, Tomislav Horaček, Domen Kožar, Jacob Mitchell, Serge Kosyrev, Michael Bishop, Christian Lindgren, Reslav Hollos, Daniel Friedman, Alejandro Garcia, Dmytro Kaidalov, Andrii Nastenko, Mariia Rodinko, Oleksiy Shevtsov, Richard Wild, Tomas Vrana, Alexander Rukin, Jonny Smillie, Jane Wild, Carlo Vicari, Christian Seberino, Laurie Wang, Leonidas Tsagkalias, Costas Saragkas, Tamara Haasen, Naho Nagahara',
    description: 'About page main  text',
  },
  aboutCopyright: {
    id: 'static.about.copyright',
    defaultMessage: '!!!2016–2017 IOHK. All rights reserved.',
    description: 'About "copyright"',
  },
});

export default class About extends Component {

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
          {intl.formatMessage(messages.aboutContentText)}
        </div>

        <div className={styles.footerWrapper}>
          <a href="http://daedaluswallet.io">http://daedaluswallet.io</a>
          <div className={styles.copyright}>{intl.formatMessage(messages.aboutCopyright)}</div>
        </div>

      </div>
    );
  }
}
