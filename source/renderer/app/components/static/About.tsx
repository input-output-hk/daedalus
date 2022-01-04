import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DialogCloseButton from '../widgets/DialogCloseButton';
import globalMessages from '../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './About.scss' or its correspon... Remove this comment to see the full error message
import styles from './About.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/close-cros... Remove this comment to see the full error message
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/daedalus-l... Remove this comment to see the full error message
import daedalusIcon from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/cardano-lo... Remove this comment to see the full error message
import cardanoIcon from '../../assets/images/cardano-logo.inline.svg';

const messages = defineMessages({
  aboutTitle: {
    id: 'static.about.title',
    defaultMessage: '!!!Daedalus',
    description: 'About "title"',
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
    defaultMessage:
      '!!!Alan McNicholas, Aleksandar Djordjevic, Alexander Rukin, Brian McKenna, Charles Hoskinson, Danilo Prates, Darko Mijić, Dominik Guzei, Elin Liu, Gabriela Ponce, Jane Wild, Jeremy Wood, Juli Sudi, Junko Oda, Laurie Wang, Manus McCole, Michael Bishop, Mior Sufian, Nikola Glumac, Piotr Stachyra, Rhys Bartels-Waller, Richard Wild, Robert Moore, Rodney Lorrimar, Sam Jeston, Samuel Leathers, Serge Kosyrev, Tatyana Valkevych, Tomas Vrana, Tomislav Horaček, Yakov Karavelov',
    description: 'About page daedalus team members',
  },
  aboutContentCardanoMembers: {
    id: 'static.about.content.cardano.members',
    defaultMessage:
      "!!!Alexander Sukhoverkhov, Alexander Vieth, Alexandre Rodrigues Baldé, Alfredo Di Napoli, Anastasiya Besman, Andrzej Rybczak, Ante Kegalj, Anton Belyy, Anupam Jain, Arseniy Seroka, Artyom Kazak, Carlos D'Agostino, Charles Hoskinson, Dan Friedman, Denis Shevchenko, Dmitry Kovanikov, Dmitry Mukhutdinov, Dmitry Nikulin, Domen Kožar, Duncan Coutts, Edsko de Vries, Eileen Fitzgerald, George Agapov, Hiroto Shioi, Ilya Lubimov, Ilya Peresadin, Ivan Gromakovskii, Jake Mitchell, Jane Wild, Jens Krause, Jeremy Wood, Joel Mislov Kunst, Jonn Mostovoy, Konstantin Ivanov, Kristijan Šarić, Lars Brünjes, Laurie Wang, Lionel Miller, Michael Bishop, Mikhail Volkhov, Niklas Hambüchen, Peter Gaži, Philipp Kant, Serge Kosyrev, Vincent Hanquez",
    description: 'About page cardano team members',
  },
  aboutCopyright: {
    id: 'static.about.copyright',
    defaultMessage: '!!!Input Output HK Limited. Licensed under',
    description: 'About "copyright"',
  },
  licenseLink: {
    id: 'static.about.license',
    defaultMessage: '!!!Apache 2.0 license',
    description: 'About page license name',
  },
  aboutBuildInfo: {
    id: 'static.about.buildInfo',
    defaultMessage: '!!!MacOS build 3769, with Cardano 1.0.4',
    description: 'About page build information',
  },
});
type Props = {
  apiVersion: string;
  nodeVersion: string;
  build: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  os: string;
  version: string;
  onClose: (...args: Array<any>) => any;
};
export default class About extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      apiVersion,
      nodeVersion,
      build,
      onOpenExternalLink,
      os,
      version,
      onClose,
    } = this.props;
    const apiName = intl.formatMessage(globalMessages.apiName);
    const apiIcon = cardanoIcon;
    const apiHeadline = intl.formatMessage(
      messages.aboutContentCardanoHeadline
    );
    const apiMembers = intl.formatMessage(messages.aboutContentCardanoMembers);
    return (
      <div className={styles.container}>
        <DialogCloseButton
          className={styles.closeButton}
          icon={closeCrossThin}
          onClose={onClose}
        />
        <div className={styles.headerWrapper}>
          <SVGInline svg={daedalusIcon} className={styles.daedalusIcon} />

          <div className={styles.daedalusTitleVersion}>
            <div className={styles.daedalusTitle}>
              {intl.formatMessage(messages.aboutTitle)}
              <span className={styles.daedalusVersion}>{version}</span>
            </div>
            <div className={styles.daedalusBuildInfo}>
              <FormattedHTMLMessage
                {...messages.aboutBuildInfo}
                values={{
                  platform: os,
                  build,
                  apiName,
                  apiVersion,
                  nodeVersion,
                }}
              />
            </div>
          </div>

          <SVGInline svg={apiIcon} className={styles.apiIcon} />
        </div>

        <div className={styles.contentText}>
          <h2>{intl.formatMessage(messages.aboutContentDaedalusHeadline)}</h2>

          <div className={styles.contentDaedalus}>
            {intl.formatMessage(messages.aboutContentDaedalusMembers)}
          </div>

          <h2>{apiHeadline}</h2>

          <div className={styles.apiMembers}>{apiMembers}</div>
        </div>

        <div className={styles.footerWrapper}>
          <Link
            className={styles.link}
            onClick={() => onOpenExternalLink('https://daedaluswallet.io')}
            label="http://daedaluswallet.io"
            skin={LinkSkin}
          />

          <div className={styles.copyright}>
            {intl.formatMessage(messages.aboutCopyright)}&nbsp;
            <Link
              className={styles.link}
              onClick={() =>
                onOpenExternalLink(
                  'https://github.com/input-output-hk/daedalus/blob/master/LICENSE'
                )
              }
              label={intl.formatMessage(messages.licenseLink)}
              skin={LinkSkin}
            />
          </div>
        </div>
      </div>
    );
  }
}
