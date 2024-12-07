#!/usr/bin/env node

import { readFileSync } from "fs";

const input = readFileSync(process.argv[2], "utf-8").split("\n\n");
const set = new Set(input[0].split("\n"));

const manuals = input[1]
  .trim()
  .split("\n")
  .map((s) => s.split(","))
  .filter((update) => {
    const sorted = update
      .slice()
      .sort((a, b) =>
        set.has(`${a}|${b}`) ? -1 : set.has(`${b}|${a}`) ? 1 : 0,
      );
    return sorted.join(",") === update.join(",");
  });

console.log(manuals.reduce((acc, a) => acc + +a[Math.floor(a.length / 2)], 0));
