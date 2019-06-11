import { defineParameterType } from 'cucumber';

// Add {bool} parameter type
defineParameterType({
  name: 'bool',
  regexp: /true|false/,
  transformer: b => b === 'true',
});
