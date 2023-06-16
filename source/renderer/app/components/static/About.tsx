import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DialogCloseButton from '../widgets/DialogCloseButton';
import globalMessages from '../../i18n/global-messages';
import styles from './About.scss';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import daedalusIcon from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
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
      '!!!Alan McNicholas, Aleksandar Djordjevic, Alexander Rukin, Brian McKenna, Charles Hoskinson, Daniel Main, Danilo Prates, Darko Mijić, Dmitrii Gaico, Dominik Guzei, Elin Liu, Gabriela Ponce, Jane Wild, Jeremy Wood, Juli Sudi, Junko Oda, Laurie Wang, Lucas Araujo, Manus McCole, Marcin Mazurek, Michael Bishop, Michael Chappell, Mior Sufian, Nikola Glumac, Piotr Stachyra, Przemysław Włodek, Renan Ferreira, Rhys Bartels-Waller, Richard Wild, Robert Moore, Rodney Lorrimar, Sam Jeston, Samuel Leathers, Serge Kosyrev, Szymon Masłowski, Tatyana Valkevych, Tomas Vrana, Tomislav Horaček, Yakov Karavelov',
    description: 'About page daedalus team members',
  },
  aboutContentCardanoMembers: {
    id: 'static.about.content.cardano.members',
    defaultMessage:
      '!!!Alan McNicholas, Alejandro Garcia, Alexander Diemand, Alexander Vieth, Anatoli Ivanou, Andreas Triantafyllos, Ante Kegalj, Armando Santos, Ben Ford, Charles Hoskinson, Dan Friedman, Deepak Kapiswe, Denis Shevchenko, Dorin Solomon, Duncan Coutts, Edsko de Vries, Erik de Castro Lopo, Gerard Moroney, Heinrich Apfelmus, Hiroto Shioi, Jane Wild, Jean-Christophe Mincke, Jeremy Wood, Johannes Lund, Jonathan Knowles, Jordan Millar, Karl Knutsson, Kristijan Šarić, Lars Brünjes, Laurie Wang, Liz Bancroft, Luke Nadur, Marc Fontaine, Marcin Szamotulski, Matt Parsons, Matthias Benkort, Michael Bishop, Michael Hueschen, Moritz Angermann, Neil Davis, Niamh Ahern, Nicholas Clarke, Nicolas Di Prima, Noel Rimbert, Paolo Veronelli, Patrick Kelly, Pawel Jakubas, Peter Gaži, Peter Thompson, Philipp Kant, Piotr Stachyra, Ravi Patel, Richard Wild, Rob Cohen, Rodney Lorrimar, Ryan Lemmer, Samuel Leathers, Serge Kosyrev, Tatyana Valkevych, Tom Flynn, Vasileios Gkoumas, Vincent Hanquez, Yuriy Lazaryev',
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

          <div>{apiMembers}</div>
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
