import { writeFileSync } from 'fs';

import { WORDS_VALID } from './wordlist'

const prolog = [...WORDS_VALID].map(word => `words('${word}').`).join('\n')
writeFileSync("wordlist.pl", prolog)
