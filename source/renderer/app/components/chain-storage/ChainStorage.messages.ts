import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  title: {
    id: 'chainStorage.locationPicker.title',
    defaultMessage: '!!!Select blockchain data location',
    description: 'Headline for the chain storage location picker screen.',
  },
  descriptionWithRequiredSpaceEstimate: {
    id: 'chainStorage.locationPicker.descriptionWithRequiredSpaceEstimate',
    defaultMessage:
      '!!!Choose where Daedalus should keep blockchain data before you continue. The latest available snapshot is about {requiredSpace}, so make sure this location has plenty of free space.',
    description:
      'Description shown on the chain storage location picker screen when an estimated required size is available.',
  },
  descriptionLargeRequirement: {
    id: 'chainStorage.locationPicker.descriptionLargeRequirement',
    defaultMessage:
      '!!!Choose where Daedalus should keep blockchain data before you continue. Blockchain data can require a large amount of free space, so choose a location with plenty of capacity.',
    description:
      'Description shown on the chain storage location picker screen when an estimated required size is not available yet.',
  },
  directoryLabel: {
    id: 'chainStorage.locationPicker.directoryLabel',
    defaultMessage: '!!!Blockchain data location',
    description: 'Label for the selected chain storage directory.',
  },
  chooseDirectory: {
    id: 'chainStorage.locationPicker.chooseDirectory',
    defaultMessage: '!!!Choose directory',
    description: 'Button label to open the chain storage directory picker.',
  },
  resetToDefault: {
    id: 'chainStorage.locationPicker.resetToDefault',
    defaultMessage: '!!!Reset to default',
    description:
      'Button label to reset chain storage back to the default path.',
  },
  continue: {
    id: 'chainStorage.locationPicker.continue',
    defaultMessage: '!!!Continue',
    description: 'Button label to continue after confirming chain storage.',
  },
  changeLocation: {
    id: 'chainStorage.locationPicker.changeLocation',
    defaultMessage: '!!!Change location',
    description:
      'Button label to return to the blockchain data location picker.',
  },
  updating: {
    id: 'chainStorage.locationPicker.updating',
    defaultMessage: '!!!Updating blockchain data location...',
    description:
      'Status message shown while Daedalus is applying a storage location change.',
  },
  availableSpaceSubtext: {
    id: 'chainStorage.locationPicker.availableSpaceSubtext',
    defaultMessage: '!!!Available disk space: {availableSpace}',
    description: 'Subtext shown below the storage location input.',
  },
  availableSpaceUnknown: {
    id: 'chainStorage.locationPicker.availableSpaceUnknown',
    defaultMessage: '!!!Unavailable',
    description:
      'Fallback value when available disk space is unavailable in the storage picker.',
  },
  defaultLocationLabel: {
    id: 'chainStorage.locationPicker.defaultLocationLabel',
    defaultMessage: '!!!Default location',
    description: 'Fallback label for the default chain storage location.',
  },
  validationPathNotFound: {
    id: 'chainStorage.locationPicker.validation.pathNotFound',
    defaultMessage: '!!!The selected directory does not exist.',
    description:
      'Validation message when the selected chain storage directory cannot be found.',
  },
  validationNotWritable: {
    id: 'chainStorage.locationPicker.validation.notWritable',
    defaultMessage: '!!!The selected path must be a writable directory.',
    description:
      'Validation message when the selected chain storage path cannot be written to.',
  },
  validationInsideStateDir: {
    id: 'chainStorage.locationPicker.validation.insideStateDir',
    defaultMessage:
      '!!!Choose a directory outside the Daedalus state directory.',
    description:
      'Validation message when the selected chain storage path is inside the state directory.',
  },
  validationInsufficientSpace: {
    id: 'chainStorage.locationPicker.validation.insufficientSpace',
    defaultMessage:
      '!!!The selected directory does not have enough free space for blockchain data.',
    description:
      'Validation message when the selected chain storage path does not have enough free space.',
  },
  validationUnknown: {
    id: 'chainStorage.locationPicker.validation.unknown',
    defaultMessage: '!!!Daedalus could not validate the selected directory.',
    description:
      'Fallback validation message when chain storage validation fails for an unknown reason.',
  },
});

export default messages;
