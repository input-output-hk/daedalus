export const CREATE_WALLET_STEPS = [
  'instructions',
  'template',
  'mnemonics',
  'validate',
  'hashImage',
  'config',
];

// WALLET RECOVERY PHRASE CHECKING

export const MNEMONICS_CHECKING_WARNING = 150; // days
export const MNEMONICS_CHECKING_NOTIFICATION = 365; // days

// export const MNEMONICS_CHECKING_STATUS = {
//   neverCheckedOk: {
//     icon:
//   }
// }

// Case: Wallet created less than 150 days ago.
// Icon: ✅
// Message: “We recommend that you check your recovery phrase in {timeUntilWarning}”
// Tooltip: ...

// Case: Recovery phrase checked less than 150 days ago.
// Icon: ✅
// Message: “You confirmed that you still have the recovery phrase for this wallet {timeAgo}.”
// Tooltip: ...

// Case: Recovery phrase checked between 150 and 365 days ago.
// Icon: ⚠️
// Message: “You confirmed that you still have the recovery phrase for this wallet {timeAgo}.”
// Tooltip: ...

// Case: Recovery phrase checked more than 365 days ago.
// Icon: 🛑
// Message: “You confirmed that you still have the recovery phrase for this wallet {timeAgo}. We recommend that you check your recovery phrase again”
// Tooltip: ...

// Case: Wallet created more than 150 days ago and recovery phrase never checked
// Icon: ⚠️
// Message: “We recommend that you check your recovery phrase in {timeUntilNotification}”
// Tooltip: ...

// Case: Wallet created more than 365 days ago and recovery phrase never checked
// Icon: 🛑
// Message: “We recommend that you check your recovery phrase”
// Tooltip: ...
