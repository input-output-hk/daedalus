---
name: i18n-messaging
description: "Manage and maintain i18n messaging or copy for Daedalus multi-language support."
user-invocable: true
---

# i18n Messaging

**Manage and maintain i18n messaging for Daedalus multi-language support**

Handles message extraction, validation, localization workflows, and schema compliance for Daedalus's internationalization system. Supports English (en-US) and Japanese (ja-JP) locales using react-intl and Format.js.

---

## Message Schema

Messages in Daedalus follow a structured format using `defineMessages()` from react-intl:

```typescript
{
  messageKey: {
    id: "namespace.context.messageKey",          // Unique identifier with dot notation
    defaultMessage: "!!!Message text with placeholders",
    description: "Context/description for translators",
    values?: Record<string, any>                  // Runtime variable placeholders
  }
}
```

### ID Naming Convention

- **Format**: `namespace.context.messageKey` (dot-separated)
- **Prefix**: All messages start with `!!!` in defaultMessage
- **Examples**:
  - `global.errors.fieldIsRequired`
  - `api.errors.IncorrectPasswordError`
  - `global.ada.name`

### Variable Placeholders

Messages with dynamic content use curly braces:

```typescript
knownMnemonicWordCount: {
  id: 'global.info.knownMnemonicWordCount',
  defaultMessage: '!!!{actual} of {required} words entered',
  values: { actual: 5, required: 12 }
}
```

---

## Operations

### 1. Extract Messages

Extract all i18n messages from source code to generate message catalog.

```bash
yarn i18n:extract
```

**What it does**:
- Scans `source/**/*.{ts,tsx}` files
- Extracts `defineMessages()` and `<FormattedMessage />` calls
- Ignores `.d.ts` TypeScript declaration files
- Outputs to `translations/messages.json`
- Records source file locations

**When to use**:
- After adding new messages with `defineMessages()`
- Before running `i18n:check`
- As part of `i18n:manage` workflow

**Output format**:
```json
[
  {
    "path": "source/main/ipc/handlers.ts",
    "descriptors": [
      {
        "id": "global.errors.fieldIsRequired",
        "defaultMessage": "!!!This field is required.",
        "description": "Error message when required fields are left empty."
      }
    ]
  }
]
```

### 2. Check Translations

Validate translation files for consistency and completeness against extracted messages.

```bash
yarn i18n:check
```

**What it does**:
- Compares extracted messages with translation files
- Validates en-US and ja-JP locales
- Ensures all message IDs are present
- Detects missing or obsolete translations
- Uses `react-intl-translations-manager` to manage lifecycle

**When to use**:
- After extracting new messages
- Before committing translation changes
- To validate message format across locales

**Validates**:
- All en-US messages have corresponding ja-JP translations
- No orphaned message IDs
- Message structure consistency (id, defaultMessage, description)

### 3. Manage Translations

Combined operation: extract messages AND validate translations in one command.

```bash
yarn i18n:manage
```

**Equivalent to**: `yarn i18n:extract && yarn i18n:check`

**When to use**:
- Primary workflow for updating i18n content
- Part of `check:all` verification
- Before creating commits with messaging changes

---

## Supported Locales

| Locale  | Language | Directory                                  |
|---------|----------|-------------------------------------------|
| en-US   | English  | `source/renderer/app/i18n/locales/en-US.json`  |
| ja-JP   | Japanese | `source/renderer/app/i18n/locales/ja-JP.json`  |

### Locale Files Structure

```
source/renderer/app/i18n/locales/
├── defaultMessages.json          # All extracted messages (auto-generated)
├── en-US.json                    # English translations (English only)
├── ja-JP.json                    # Japanese translations
├── whitelist_en-US.json          # Approved en-US messages
├── whitelist_ja-JP.json          # Approved ja-JP messages
└── terms-of-use/                 # Locale-specific documents
```

---

## Best Practices

### Adding New Messages

1. Use `defineMessages()` or `<FormattedMessage />` in source code
2. Follow naming convention: `namespace.context.messageKey`
3. Include descriptive `description` for translators
4. Always prefix defaultMessage with `!!!`
5. Run `yarn i18n:extract` to register new messages

### Validating Message IDs

**Do**:
- Use hierarchical namespaces: `wallet.send.confirmButton`
- Keep IDs consistent across related messages
- Document placeholder variables in description
- Use lowercase with dots as separators

**Don't**:
- Use spaces or special characters in IDs
- Duplicate IDs across features
- Leave placeholders undocumented
- Change ID format mid-project

### Working with Placeholders

Messages with runtime values use curly braces:

```typescript
// Define message with placeholder
notEnoughFunds: {
  id: 'wallet.errors.notEnoughFunds',
  defaultMessage: '!!!Remaining balance: {balance} ADA',
  description: 'Error when insufficient funds'
}

// Use at runtime
<FormattedMessage
  id="wallet.errors.notEnoughFunds"
  defaultMessage="Remaining balance: {balance} ADA"
  values={{ balance: walletBalance }}
/>
```

### Translation Flow

1. **Extract**: Run `yarn i18n:extract` after code changes
2. **Check**: Run `yarn i18n:check` to validate
3. **Review**: Update localized JSON files as needed
4. **Verify**: Run `check:all` before committing
5. **Commit**: Include updated `translations/messages.json` and locale files

### Generated `!!!` Placeholders

- `yarn i18n:manage` seeds missing locale entries from `defaultMessage`, so new keys in `en-US.json` and `ja-JP.json` can be written with the `!!!` prefix.
- Treat `!!!` in locale files as a new or untranslated message marker, not polished release copy.
- This workflow does not remove the prefix automatically. If locale polish is in scope, manually replace new `en-US.json` entries with approved English copy and add real Japanese translations in `ja-JP.json` before commit.
- If the task only covers extraction or catalog sync, keeping generated `!!!` placeholders is acceptable, but document the follow-up translation work explicitly.

---

## Related Files

| File | Purpose |
|------|---------|
| `translations/translation-runner.ts` | Validates translations (manages mapping) |
| `translations/formatter.js` | Custom extraction format for Format.js |
| `translations/messages.json` | Extracted message catalog |
| `source/renderer/app/i18n/global-messages.ts` | Global message definitions |
| `source/renderer/app/i18n/errors.ts` | Error message definitions |
| `source/renderer/app/i18n/types.ts` | TypeScript types for messages |

---

## Troubleshooting

### Messages not extracted

- Verify `defineMessages()` is imported from `react-intl`
- Check files are in `source/**/*.{ts,tsx}` (not .d.ts)
- Run `yarn clear:translations` then `yarn i18n:extract`

### Translation validation fails

- Ensure all en-US messages have ja-JP equivalents
- Check message IDs match exactly (case-sensitive)
- Verify JSON format is valid in locale files

### Missing or duplicate IDs

- Check for conflicting message keys across files
- Remove obsolete messages from translation files manually
- Re-run `yarn i18n:manage` to refresh state

---

## Template: Adding New Message

```typescript
import { defineMessages } from 'react-intl';

// In your component file or messages.ts
export const componentMessages = defineMessages({
  newMessageKey: {
    id: 'feature.component.newMessageKey',
    defaultMessage: '!!!Default English text',
    description: 'Context for translators explaining where/how this message is used',
  },
  messageWithPlaceholder: {
    id: 'feature.component.messageWithPlaceholder',
    defaultMessage: '!!!You have {count} items',
    description: 'Message showing item count with placeholder',
  },
});
```

Then run: `yarn i18n:manage`

