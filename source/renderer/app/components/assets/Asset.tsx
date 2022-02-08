import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { defineMessages, intlShape } from 'react-intl';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Asset.scss' or its correspon... Remove this comment to see the full error message
import styles from './Asset.scss';
import { ellipsis } from '../../utils/strings';
import AssetContent from './AssetContent';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/asset-toke... Remove this comment to see the full error message
import settingsIcon from '../../assets/images/asset-token-settings-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/asset-toke... Remove this comment to see the full error message
import warningIcon from '../../assets/images/asset-token-warning-ic.inline.svg';
import { ASSET_TOKEN_DISPLAY_DELAY } from '../../config/timingConfig';
import type { Asset as AssetProps } from '../../api/assets/types';

const messages = defineMessages({
  fingerprintItem: {
    id: 'assets.assetToken.param.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" item.',
  },
  policyIdItem: {
    id: 'assets.assetToken.param.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" item.',
  },
  assetNameItem: {
    id: 'assets.assetToken.param.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" item.',
  },
  nameItem: {
    id: 'assets.assetToken.param.name',
    defaultMessage: '!!!Name',
    description: '"name" item.',
  },
  tickerItem: {
    id: 'assets.assetToken.param.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" item.',
  },
  descriptionItem: {
    id: 'assets.assetToken.param.description',
    defaultMessage: '!!!Description',
    description: '"description" item.',
  },
  blank: {
    id: 'assets.assetToken.param.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" item value.',
  },
  settingsCogPopOver: {
    id: 'assets.assetToken.settings.cogPopOver',
    defaultMessage:
      '!!!You can configure the number of decimal places for this native token.',
    description: 'Asset settings pop over content',
  },
  settingsWarningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  settingsWarningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
  },
});
type Props = {
  asset: AssetProps;
  small?: boolean;
  hidePopOver?: boolean;
  onCopyAssetParam?: (...args: Array<any>) => any;
  onClickSettings?: (...args: Array<any>) => any;
  assetSettingsDialogWasOpened?: boolean | null | undefined;
  anyAssetWasHovered?: boolean | null | undefined;
  fullFingerprint?: boolean | null | undefined;
  hasWarning?: boolean | null | undefined;
  className?: string;
  // In case it's not possible to calculate the container width
  // this props defines after how many characters the `metadata.name` text will cut off
  metadataNameChars?: number;
  hasError?: boolean;
};
type State = {
  isPillPopOverVisible: boolean;
  isHoveringSettingsIcon: boolean;
};

@observer
class Asset extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  copyNotificationTimeout: TimeoutID;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  displayDelayTimeout: TimeoutID;
  state = {
    isPillPopOverVisible: false,
    isHoveringSettingsIcon: false,
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  handleShowPillPopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPillPopOverVisible: true,
        });
      }
    }, ASSET_TOKEN_DISPLAY_DELAY);
  };
  handleHidePillPopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPillPopOverVisible: false,
        });
      }
    }, ASSET_TOKEN_DISPLAY_DELAY);
  };
  handleSettingsMouseEnter = () => {
    this.setState({
      isHoveringSettingsIcon: true,
    });
  };
  handleSettingsMouseLeave = () => {
    this.setState({
      isHoveringSettingsIcon: false,
    });
  };

  get isSettingsPopOverVisible() {
    const { assetSettingsDialogWasOpened, anyAssetWasHovered } = this.props;
    const { isHoveringSettingsIcon } = this.state;

    if (isHoveringSettingsIcon) {
      return true;
    }

    if (
      assetSettingsDialogWasOpened === false &&
      anyAssetWasHovered === false
    ) {
      return true;
    }

    return false;
  }

  renderPillContent() {
    const { intl } = this.context;
    const {
      asset,
      metadataNameChars,
      small = true,
      fullFingerprint,
      hasWarning,
      hasError,
    } = this.props;
    const { fingerprint, metadata, decimals, recommendedDecimals } = asset;
    const { name } = metadata || {};
    const contentStyles = classnames([
      styles.pill,
      hasError ? styles.error : null,
    ]);
    const [startCharAmount, endCharAmoout] = small ? [9, 4] : [12, 12];
    let warningPopOverMessage;

    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
    }

    return (
      <div className={contentStyles}>
        <div className={styles.fingerprint}>
          {fullFingerprint
            ? fingerprint
            : ellipsis(fingerprint || '', startCharAmount, endCharAmoout)}
        </div>
        {name && (
          <div className={styles.metadataName}>
            {metadataNameChars ? ellipsis(name, metadataNameChars) : name}
          </div>
        )}
        {hasWarning && (
          <div className={styles.warningIconWrapper}>
            <PopOver
              content={intl.formatMessage(warningPopOverMessage, {
                recommendedDecimals,
              })}
              className={styles.warningIconWrapper}
            >
              <SVGInline className={styles.warningIcon} svg={warningIcon} />
            </PopOver>
          </div>
        )}
      </div>
    );
  }

  renderPillPopOverContainer = () => {
    const { asset, onCopyAssetParam } = this.props;
    const pillContent = this.renderPillContent();
    const popOverContent = (
      <AssetContent
        asset={asset}
        onCopyAssetParam={onCopyAssetParam}
        className={styles.popOverContent}
        highlightFingerprint
      />
    );
    const { isPillPopOverVisible } = this.state;
    return (
      <div
        className={styles.popOverContainer}
        onMouseEnter={this.handleShowPillPopOver}
        onMouseLeave={this.handleHidePillPopOver}
      >
        <PopOver
          themeVariables={{
            '--rp-pop-over-bg-color':
              'var(--theme-widgets-asset-token-background-color)',
            '--rp-pop-over-text-color': 'var(--theme-bordered-box-text-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-width': '1px',
            '--rp-pop-over-border-style': 'solid',
            '--rp-pop-over-box-shadow':
              '0 5px 20px 0 var(--theme-widgets-asset-token-box-shadow)',
          }}
          contentClassName={styles.popOver}
          content={popOverContent}
          visible={isPillPopOverVisible}
          appendTo="parent"
          maxWidth={376}
          allowHTML
          interactive
        >
          {pillContent}
        </PopOver>
      </div>
    );
  };
  renderSettingsContent = () => {
    const { intl } = this.context;
    const { asset, onClickSettings, hasWarning } = this.props;
    if (!onClickSettings) return null;
    const {
      isSettingsPopOverVisible,
      handleSettingsMouseEnter,
      handleSettingsMouseLeave,
    } = this;

    const onClickSettingsBind = () => onClickSettings && onClickSettings(asset);

    const { decimals, recommendedDecimals } = asset;
    let warningPopOverMessage;

    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
    }

    return (
      <button className={styles.settingsButton} onClick={onClickSettingsBind}>
        <PopOver
          className={styles.test}
          content={intl.formatMessage(messages.settingsCogPopOver)}
          visible={isSettingsPopOverVisible}
        >
          <SVGInline
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ onMouseEnter: () => void; onMouseLeave: ()... Remove this comment to see the full error message
            onMouseEnter={handleSettingsMouseEnter}
            onMouseLeave={handleSettingsMouseLeave}
            className={styles.settingsIcon}
            svg={settingsIcon}
          />
        </PopOver>
        {hasWarning && (
          <PopOver
            content={intl.formatMessage(warningPopOverMessage, {
              recommendedDecimals,
            })}
          >
            <SVGInline className={styles.warningIcon} svg={warningIcon} />
          </PopOver>
        )}
      </button>
    );
  };

  render() {
    const { hidePopOver, className } = this.props;
    const content = hidePopOver
      ? this.renderPillContent()
      : this.renderPillPopOverContainer();
    const settingsContent = this.renderSettingsContent();
    const componentClassnames = classnames([styles.component, className]);
    return (
      <div className={componentClassnames}>
        {content}
        {settingsContent}
      </div>
    );
  }
}

export default Asset;
