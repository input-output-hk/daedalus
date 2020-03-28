// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import daedalusIcon from '../../../assets/images/daedalus-logo-loading-grey.inline.svg';
import styles from './Splash.scss';

type Props = {
  onButtonClick: Function,
  onLinkClick: Function,
  title: string,
  subTitle1: string,
  subTitle2: string,
  description: Node,
  buttonLabel: string,
  linkLabel: string,
  isIncentivizedTestnetTheme?: boolean,
  backgroundImage?: string,
};

export default class SplashNetwork extends Component<Props> {
  render() {
    const {
      onButtonClick,
      onLinkClick,
      title,
      subTitle1,
      subTitle2,
      description,
      buttonLabel,
      linkLabel,
      isIncentivizedTestnetTheme,
      backgroundImage,
    } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.backgroundContainer}>
          {isIncentivizedTestnetTheme && backgroundImage && (
            <>
              <div className={styles.backgroundOverlay} />
              <SVGInline
                svg={backgroundImage}
                className={styles.backgroundImage}
              />
            </>
          )}
        </div>
        <div className={styles.content}>
          <SVGInline svg={daedalusIcon} className={styles.daedalusIcon} />
          <div className={styles.title}>{title}</div>
          <div className={styles.subTitle1}>{subTitle1}</div>
          <div className={styles.subTitle2}>{subTitle2}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.action}>
            <Button
              className={styles.actionButton}
              label={buttonLabel}
              onClick={onButtonClick}
              skin={ButtonSkin}
            />
          </div>
          <Link
            className={styles.learnMoreLink}
            onClick={onLinkClick}
            label={linkLabel}
            skin={LinkSkin}
          />
        </div>
      </div>
    );
  }
}
