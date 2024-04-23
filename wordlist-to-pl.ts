import { writeFileSync } from 'fs';

import { WORDS_TARGET } from './wordlist'

const prolog = WORDS_TARGET.map(word => `words('${word}').`).join('\n')
writeFileSync("wordlist.pl", prolog)
