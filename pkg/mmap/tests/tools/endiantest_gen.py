#!/usr/bin/env python
"""
Generates and outputs the test cases for endianness
aware mmap to stdout.

"""

from __future__ import print_function
import struct

TESTS = [
    # (mmap type, python struct type, test value)
    ('int16' , 'h', -16000),
    ('uint16', 'H', 16000),
    ('int32' , 'i', -1073741819),
    ('int64' , 'q', 1073741819),
    ('real32', 'f', 10.0),
    ('real64', 'd', 10.0)
]

FILE_TEMPLATE = """
library(mmap)

tmp <- tempfile()

assert.endianness <- function(endianness, bytes, type, expected) {
    writeBin(bytes, tmp, size=1L)

    m <- mmap(tmp, type, endianness=endianness)
    x <- m[1]
    if (is.list(expected)) {
        x <- x[[1]]
        expected <- expected[[1]]
    }
    if (x != expected)
        stop(paste("m[1] ==", x, "expected", expected))
    munmap(m)
}

"""

TEST_TEMPLATE = """##### {endianness} {type}() #####
test.endianness.{endianness}.{type} <- function() assert.endianness("{endianness}", {bytes}, {type}(), {expected})
test.endianness.{endianness}.struct.{type} <- function() assert.endianness("{endianness}", {bytes}, struct({type}()), list({expected}))
"""

def bytes(x):
    """
    Converts a Python string to the string representation
    of a R array of bytes.

    """
    return "c(" + ", ".join(str(ord(c))+"L" for c in x) + ")"

def main():
    print(FILE_TEMPLATE)

    for (rtype, stype, expected) in TESTS:
        big    = struct.pack('>' + stype, expected)
        print(TEST_TEMPLATE.format(
            endianness = "big",
            type       = rtype,
            bytes      = bytes(big),
            expected   = expected))

        little = struct.pack('<' + stype, expected)
        print(TEST_TEMPLATE.format(
            endianness = "little",
            type       = rtype,
            bytes      = bytes(little),
            expected   = expected))

    for (rtype, stype, expected) in TESTS:
        print("test.endianness.big.{0}()".format(rtype))
        print("test.endianness.big.struct.{0}()".format(rtype))
        print("test.endianness.little.{0}()".format(rtype))
        print("test.endianness.little.struct.{0}()".format(rtype))

if __name__ == '__main__':
    main()

