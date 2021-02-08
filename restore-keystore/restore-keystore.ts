#!/usr/bin/env -S node -r "ts-node/register"

import * as fs from 'fs';
import * as path from 'path';
import { decodeKeystore, prettyKeystore } from '.';

const [_1, _2, keystorePath] = process.argv;

const bytes = fs.readFileSync(path.isAbsolute(keystorePath)
  ? keystorePath
  : path.join(__dirname, keystorePath));

decodeKeystore(bytes)
  .then(prettyKeystore)
  .then(console.log)
  .catch(console.exception);
