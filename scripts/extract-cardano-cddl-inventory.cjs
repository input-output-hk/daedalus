const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

const argument = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0 || !process.argv[index + 1]) throw new Error(`Missing ${name}`);
  return process.argv[index + 1];
};

const sources = {
  conway: path.resolve(argument('--conway-cddl')),
  dijkstra: path.resolve(argument('--dijkstra-cddl')),
};
const output = path.resolve(argument('--output'));

const extractAllDefinitions = (source) => {
  const matches = [
    ...source.matchAll(
      /^([a-zA-Z_][a-zA-Z0-9_]*)(<[a-zA-Z0-9_,\s]*>)?\s*=/gm
    ),
  ];
  return Object.fromEntries(
    matches.map((match, index) => {
      const end = matches[index + 1] ? matches[index + 1].index : source.length;
      const definition = source
        .slice(match.index, end)
        .trimEnd()
        .split('\n')
        .map((line) => line.trimEnd())
        .join('\n');
      const name = `${match[1]}${(match[2] || '').replace(/\s/g, '')}`;
      return [name, definition];
    })
  );
};

const extractUnionDiscriminants = (definitions, unionName) => {
  const names = [...definitions[unionName].matchAll(/^\s*(?:\[|\/\/)\s*([a-z][a-z0-9_]*)/gm)]
    .map((match) => match[1])
    .filter((name) => definitions[name]);
  return names.map((name) => {
    const tag = definitions[name].match(/=\s*\(?\s*(\d+)/);
    return { name, tag: tag ? Number(tag[1]) : null };
  });
};

const extractNumericMap = (definition) => {
  const fields = [];
  for (const line of definition.split('\n')) {
    const match = line.match(
      /^\s*[,{]\s*(\?)?\s*(\d+)\s*:\s*([^;]+?)(?:\s*;\s*(.*))?\s*$/
    );
    if (match) {
      const type = match[3].trim();
      let cardinality = 'scalar-or-declared-union';
      if (
        type.includes('[*') ||
        type.includes('{*') ||
        type.includes('set<')
      ) {
        cardinality = 'zero-or-more';
      }
      if (
        type.includes('nonempty') ||
        type.includes('[+') ||
        type.includes('{+')
      ) {
        cardinality = 'nonempty';
      }
      fields.push({
        key: Number(match[2]),
        required: !match[1],
        type,
        cardinality,
        comment: match[4] ? match[4].trim() : null,
      });
    }
  }
  return fields;
};

const sha256 = (value) =>
  crypto.createHash('sha256').update(value).digest('hex');

const eras = {};
for (const [era, sourcePath] of Object.entries(sources)) {
  const source = fs.readFileSync(sourcePath, 'utf8');
  const definitions = extractAllDefinitions(source);
  eras[era] = {
    sourceSha256: sha256(source),
    definitions,
    discriminants: {
      certificates: extractUnionDiscriminants(definitions, 'certificate'),
      governanceActions: extractUnionDiscriminants(definitions, 'gov_action'),
      nativeScripts: extractUnionDiscriminants(definitions, 'native_script'),
    },
    maps: {
      transactionBody: extractNumericMap(definitions.transaction_body),
      babbageOutput: extractNumericMap(definitions.babbage_transaction_output),
      witnessSet: extractNumericMap(definitions.transaction_witness_set),
      auxiliaryDataMap: extractNumericMap(definitions.auxiliary_data_map),
      protocolParameterUpdate: extractNumericMap(
        definitions.protocol_param_update
      ),
      ...(era === 'dijkstra'
        ? {
            subTransactionBody: extractNumericMap(
              definitions.sub_transaction_body
            ),
          }
        : {}),
    },
  };
}

fs.writeFileSync(
  output,
  `${JSON.stringify({ schemaVersion: 1, eras }, null, 2)}\n`
);
