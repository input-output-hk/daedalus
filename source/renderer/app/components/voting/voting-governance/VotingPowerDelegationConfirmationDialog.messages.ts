import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'voting.governance.confirmationDialog.title',
    defaultMessage: '!!!Confirm Transaction',
    description: 'Title of the voting power delegation confirmation dialog',
  },
  // Not "signed payload": what is signed is a CBOR certificate this dialog
  // never sees, and calling this that would name a thing it is not. It is a
  // readable statement of the delegation the certificate will carry.
  delegationCertificate: {
    id: 'voting.governance.confirmationDialog.delegationCertificate',
    defaultMessage: '!!!Delegation certificate',
    description: 'Heading above the delegation this transaction will record',
  },
  errorGeneric: {
    id: 'voting.governance.transactionError.generic',
    defaultMessage:
      '!!!Something went wrong with this transaction. Please try again in a few minutes.',
    description:
      'Shown when a delegation transaction fails to build or to submit',
  },
});
