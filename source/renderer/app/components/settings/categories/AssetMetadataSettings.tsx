import React, { Component } from 'react';
import { map } from 'lodash';
import { Select } from 'react-polymorph/lib/components/Select';
import { Link } from 'react-polymorph/lib/components/Link';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { getAssetMetadataSourceIdFromUrl } from '../../../utils/assets';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import styles from './AssetMetadataSettings.scss';
import {
  ASSET_METADATA_SERVERS_LIST,
  ASSET_METADATA_SOURCE_TYPES,
  ASSET_METADATA_URL_VALIDATOR,
} from '../../../config/assetsConfig';
import type { AssetMetadataSourceType } from '../../../types/assetTypes';
import LocalizableError from '../../../i18n/LocalizableError';
import ApiError from '../../../domains/ApiError';

const messages = defineMessages({
  description: {
    id: 'settings.assetMetadata.description',
    defaultMessage:
      '!!!Some tokens publish their name on the chain instead of to the Cardano token registry. Daedalus finds those records through an index and confirms each one against your own node before using it. The index you choose learns which of these tokens this wallet holds, so it is worth choosing deliberately. Names and decimal places from the {link} are fetched separately and this setting does not affect them.',
    description: 'description for the Asset Metadata settings page.',
  },
  descriptionLinkLabel: {
    id: 'settings.assetMetadata.descriptionLinkLabel',
    defaultMessage: '!!!Cardano token registry',
    description:
      'label of the registry link in the description on the Asset Metadata settings page.',
  },
  descriptionLinkUrl: {
    id: 'settings.assetMetadata.descriptionLinkUrl',
    defaultMessage: '!!!https://tokens.cardano.org/',
    description:
      'URL of the registry link in the description on the Asset Metadata settings page.',
  },
  sourceSelectLabel: {
    id: 'settings.assetMetadata.select.label',
    defaultMessage: '!!!On-chain metadata index',
    description:
      'label for the source selection on the Asset Metadata settings page.',
  },
  sourceSelectKoios: {
    id: 'settings.assetMetadata.select.koios',
    defaultMessage: '!!!Koios (recommended)',
    description:
      'Koios option for the source selection on the Asset Metadata settings page.',
  },
  sourceSelectCustom: {
    id: 'settings.assetMetadata.select.custom',
    defaultMessage: '!!!Custom index',
    description:
      'custom option for the source selection on the Asset Metadata settings page.',
  },
  sourceSelectDirect: {
    id: 'settings.assetMetadata.select.direct',
    defaultMessage: '!!!From my own chain data (not available yet)',
    description:
      'direct option for the source selection on the Asset Metadata settings page.',
  },
  descriptionKoios: {
    id: 'settings.assetMetadata.descriptionKoios',
    defaultMessage:
      '!!!{link} is a public, community-run index of the Cardano chain. It is the default and needs no configuration. Daedalus sends it token identifiers and nothing else, and every record it returns is confirmed against your own node before it is stored.',
    description:
      'description of the Koios option on the Asset Metadata settings page.',
  },
  descriptionKoiosLinkLabel: {
    id: 'settings.assetMetadata.descriptionKoiosLinkLabel',
    defaultMessage: '!!!Koios',
    description: 'label of the Koios link on the Asset Metadata settings page.',
  },
  descriptionKoiosLinkUrl: {
    id: 'settings.assetMetadata.descriptionKoiosLinkUrl',
    defaultMessage: '!!!https://koios.rest/',
    description: 'URL of the Koios link on the Asset Metadata settings page.',
  },
  descriptionCustom: {
    id: 'settings.assetMetadata.descriptionCustom',
    defaultMessage:
      '!!!An instance you run or trust, which keeps this wallet out of any public index. Daedalus checks that the address answers before saving it, and refuses one whose chain tip is far behind your own node.',
    description:
      'description of the custom option on the Asset Metadata settings page.',
  },
  descriptionDirect: {
    id: 'settings.assetMetadata.descriptionDirect',
    defaultMessage:
      '!!!The same records, read from the chain data you already hold, with no third party involved. This option is not available yet.',
    description:
      'description of the direct option on the Asset Metadata settings page.',
  },
  urlInputLabel: {
    id: 'settings.assetMetadata.url.input.label',
    defaultMessage: '!!!Index address',
    description:
      'label of the custom URL input on the Asset Metadata settings page.',
  },
  urlInputPlaceholder: {
    id: 'settings.assetMetadata.url.input.placeholder',
    defaultMessage: '!!!Enter the address of your own instance',
    description:
      'placeholder of the custom URL input on the Asset Metadata settings page.',
  },
  invalidUrl: {
    id: 'settings.assetMetadata.url.input.invalidUrl',
    defaultMessage: '!!!Invalid address',
    description:
      'message shown for an address that does not match the accepted shape on the Asset Metadata settings page.',
  },
  invalidUrlPrefix: {
    id: 'settings.assetMetadata.url.input.invalidUrlPrefix',
    defaultMessage: '!!!The address should start with "https://"',
    description:
      'message shown for an address that is not https on the Asset Metadata settings page.',
  },
});

type Props = {
  sourceUrl: string | null | undefined;
  sourceUrlError?: LocalizableError | ApiError | null | undefined;
  onSelectSourceUrl: (...args: Array<any>) => any;
  onResetSourceError: (...args: Array<any>) => any;
  isLoading: boolean;
  onOpenExternalLink: (...args: Array<any>) => any;
};

type State = {
  editingSourceUrl: string | null | undefined;
};

@observer
class AssetMetadataSettings extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    editingSourceUrl: this.props.sourceUrl,
  };

  componentWillUnmount() {
    this.props.onResetSourceError();
  }

  handleSubmit = (url: string) => {
    if (this.handleIsValid(url)) {
      this.setState({
        editingSourceUrl: url,
      });
      this.props.onSelectSourceUrl(url);
    }
  };

  /**
   * A preset carries its URL, so choosing one is the whole action. `custom`
   * carries none, so choosing it reveals the input and submits nothing until
   * the user has typed something.
   */
  handleOnSelectSourceType = (sourceType: AssetMetadataSourceType) => {
    const { onSelectSourceUrl, onResetSourceError } = this.props;
    onResetSourceError();
    let editingSourceUrl = '';

    if (sourceType !== ASSET_METADATA_SOURCE_TYPES.CUSTOM) {
      editingSourceUrl = ASSET_METADATA_SERVERS_LIST[sourceType]?.url || '';
      if (editingSourceUrl) onSelectSourceUrl(editingSourceUrl);
    }

    this.setState({
      editingSourceUrl,
    });
  };

  handleIsValid = (url: string) =>
    url === '' || ASSET_METADATA_URL_VALIDATOR.test(url);

  /**
   * What is wrong with the string the user typed, which is a different question
   * from what the instance answered. The probe's refusal arrives as
   * `sourceUrlError` and is rendered beside this one.
   */
  handleErrorMessage = (value: string) => {
    const { intl } = this.context;
    const errorMessage = /^https:\/\//i.test(value)
      ? messages.invalidUrl
      : messages.invalidUrlPrefix;
    return intl.formatMessage(errorMessage);
  };

  sourceSelectMessages = {
    koios: this.context.intl.formatMessage(messages.sourceSelectKoios),
    custom: this.context.intl.formatMessage(messages.sourceSelectCustom),
    direct: this.context.intl.formatMessage(messages.sourceSelectDirect),
  };

  renderSourceTypeDropdown = () => {
    const { intl } = this.context;
    const { editingSourceUrl } = this.state;
    const sourceType = getAssetMetadataSourceIdFromUrl(editingSourceUrl || '');
    const options = map(ASSET_METADATA_SOURCE_TYPES, (value) => ({
      label: this.sourceSelectMessages[value] || value,
      value,
      // Rendered rather than hidden, so the shape of the choice is visible
      // before the local scan exists. react-polymorph ignores a click on it and
      // skips it in keyboard navigation.
      isDisabled: value === ASSET_METADATA_SOURCE_TYPES.DIRECT,
    }));

    return (
      <Select
        label={intl.formatMessage(messages.sourceSelectLabel)}
        value={sourceType}
        options={options}
        onChange={this.handleOnSelectSourceType}
        optionHeight={50}
        selectionRenderer={({ label }: { label: string }) => (
          <div className={styles.selectionRenderer}>{label}</div>
        )}
      />
    );
  };

  renderCustomUrlInput = () => {
    const { sourceUrlError, isLoading } = this.props;
    const { intl } = this.context;
    const { editingSourceUrl } = this.state;
    const sourceType = getAssetMetadataSourceIdFromUrl(editingSourceUrl || '');

    if (sourceType !== ASSET_METADATA_SOURCE_TYPES.CUSTOM) {
      return null;
    }

    return (
      <InlineEditingInput
        className={styles.sourceUrl}
        label={intl.formatMessage(messages.urlInputLabel)}
        value={editingSourceUrl || ''}
        placeholder={intl.formatMessage(messages.urlInputPlaceholder)}
        onSubmit={this.handleSubmit}
        isValid={this.handleIsValid}
        valueErrorMessage={this.handleErrorMessage}
        errorMessage={
          sourceUrlError ? intl.formatMessage(sourceUrlError) : null
        }
        readOnly={isLoading}
        isLoading={isLoading}
        successfullyUpdated={false}
      />
    );
  };

  renderBottomContent = () => {
    const { onOpenExternalLink } = this.props;
    const { intl } = this.context;
    const { editingSourceUrl } = this.state;
    const sourceType = getAssetMetadataSourceIdFromUrl(editingSourceUrl || '');

    if (sourceType === ASSET_METADATA_SOURCE_TYPES.KOIOS) {
      return (
        <div className={styles.optionDescription}>
          <FormattedMessage
            {...messages.descriptionKoios}
            values={{
              link: (
                <Link
                  onClick={() =>
                    onOpenExternalLink(
                      intl.formatMessage(messages.descriptionKoiosLinkUrl)
                    )
                  }
                  label={intl.formatMessage(messages.descriptionKoiosLinkLabel)}
                />
              ),
            }}
          />
        </div>
      );
    }

    if (sourceType === ASSET_METADATA_SOURCE_TYPES.DIRECT) {
      return (
        <div className={styles.optionDescription}>
          {intl.formatMessage(messages.descriptionDirect)}
        </div>
      );
    }

    return (
      <div className={styles.optionDescription}>
        {intl.formatMessage(messages.descriptionCustom)}
      </div>
    );
  };

  render() {
    const { onOpenExternalLink } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <div className={styles.description}>
          <FormattedMessage
            {...messages.description}
            values={{
              link: (
                <Link
                  className={styles.link}
                  onClick={() =>
                    onOpenExternalLink(
                      intl.formatMessage(messages.descriptionLinkUrl)
                    )
                  }
                  label={intl.formatMessage(messages.descriptionLinkLabel)}
                />
              ),
            }}
          />
        </div>

        {this.renderSourceTypeDropdown()}
        {this.renderCustomUrlInput()}
        {this.renderBottomContent()}
      </div>
    );
  }
}

export default AssetMetadataSettings;
