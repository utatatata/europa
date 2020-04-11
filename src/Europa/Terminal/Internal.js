"use strict";

const os = require("os");

exports.getEOL = () => os.EOL;

exports.rows = () => process.stdout.rows;

exports.columns = () => process.stdout.columns;

exports.write = str => () => process.stdout.write(str);

exports.cork = () => process.stdout.cork();

exports.uncork = () => process.stdout.uncork();
