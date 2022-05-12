import { Resolution, ResolutionError } from '@unstoppabledomains/resolution';
import { defineMessages } from 'react-intl';
// import globalMessages from '../../renderer/app/i18n/global-messages';

const messages = defineMessages({
  unsupported: {
    id: 'wallet.transaction.send.domain.unsupported',
    defaultMessage: '!!!Unsupported Domain',
    description: 'Unsupported Domain Error',
  },
  notFound: {
    id: 'wallet.transaction.send.domain.recordNotFound',
    defaultMessage: '!!!No Cardano record found for this domain',
    description: 'Not Found domain error',
  },
  unregistered: {
    id: 'wallet.transaction.send.domain.unregistered',
    defaultMessage: '!!!Domain not registered',
    description: 'Domain not registered error',
  },
  networkLabel: {
    id: 'wallet.receive.pdf.networkLabel',
    defaultMessage: '!!!Cardano Network:',
    description: 'PDF networkLabel',
  },
});

// enum ErrorMessage {
//   NOT_FOUND = 'domainNotFound',
//   UNREGISTED = 'domainUnregistered',
//   UNSUPPORTED = 'domainUnsupported',
// }

export const resolveUNSAddress = async (unsAddress: string, intl: any) => {
  try {
    return await new Resolution().addr(unsAddress, 'ADA');
  } catch (error) {
    switch (error.code) {
      case 'UnsupportedDomain':
        throw new Error(
          intl.formatMessage(messages.wallet.transaction.send.domain.invalid)
        );
      case 'RecordNotFound':
        throw new Error(
          intl.formatMessage(
            messages.wallet.transaction.send.domain.recordNotFound
          )
        );
      case 'UnregisteredDomain':
        throw new Error(
          intl.formatMessage(
            messages.wallet.transaction.send.domain.unregistered
          )
        );
      default:
        throw new Error('Error resolving domain');
    }
  }
};
