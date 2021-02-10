#!/usr/bin/env -S node -r "ts-node/register"
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const path = require("path");
const _3 = require(".");
const [_1, _2, keystorePath] = process.argv;
const bytes = fs.readFileSync(path.isAbsolute(keystorePath)
    ? keystorePath
    : path.join(__dirname, keystorePath));
_3.decodeKeystore(bytes)
    .then(_3.prettyKeystore)
    .then(console.log)
    .catch(console.exception);
//# sourceMappingURL=restore-keystore.js.map