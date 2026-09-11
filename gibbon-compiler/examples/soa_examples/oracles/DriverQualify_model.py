#!/usr/bin/env python3
"""Independent model for the DriverQualify fixture.

Mirrors the SOURCE semantics, not Gibbon's output: build n nodes carrying
(k mod 50) and (k mod 7), add 1 and 2 respectively, then sum both fields.
Run it to regenerate DriverQualify.expected.
"""
def total(n: int = 64) -> int:
    acc = 0
    for k in range(1, n + 1):
        a = (k % 50) + 1      # Int8 field, stays <= 50 so it cannot wrap
        b = (k % 7) + 2       # Int64 field
        acc += a + b
    return acc

if __name__ == "__main__":
    print("DRIVER-QUALIFY")
    print(total())
