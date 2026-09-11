#!/usr/bin/env python3
"""Independent model for tests/vw09/FusionKeyCollision.hs and its control.

Both fixtures are the same program up to constructor NAMES, so one model
serves both.  `mkT n` walks n..1; every 4th node is the rarer constructor.
`bump` adds 1 to the rare one's field and 7 to the common one's.
"""
import sys


def answer(n):
    ns = [('rare', k + 1000) if k % 4 == 0 else ('common', k + 2000)
          for k in range(n, 0, -1)]
    b = [(c, v + 1 if c == 'rare' else v + 7) for c, v in ns]
    return "'#(%d %d %d)" % (len(b),
                             sum(v for c, v in b if c == 'rare'),
                             sum(v for c, v in b if c == 'common'))


if __name__ == '__main__':
    print(answer(int(sys.argv[1]) if len(sys.argv) > 1 else 40))
