
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


##### big int16() #####
test.endianness.big.int16 <- function() assert.endianness("big", c(193L, 128L), int16(), -16000)
test.endianness.big.struct.int16 <- function() assert.endianness("big", c(193L, 128L), struct(int16()), list(-16000))

##### little int16() #####
test.endianness.little.int16 <- function() assert.endianness("little", c(128L, 193L), int16(), -16000)
test.endianness.little.struct.int16 <- function() assert.endianness("little", c(128L, 193L), struct(int16()), list(-16000))

##### big uint16() #####
test.endianness.big.uint16 <- function() assert.endianness("big", c(62L, 128L), uint16(), 16000)
test.endianness.big.struct.uint16 <- function() assert.endianness("big", c(62L, 128L), struct(uint16()), list(16000))

##### little uint16() #####
test.endianness.little.uint16 <- function() assert.endianness("little", c(128L, 62L), uint16(), 16000)
test.endianness.little.struct.uint16 <- function() assert.endianness("little", c(128L, 62L), struct(uint16()), list(16000))

##### big int32() #####
test.endianness.big.int32 <- function() assert.endianness("big", c(192L, 0L, 0L, 5L), int32(), -1073741819)
test.endianness.big.struct.int32 <- function() assert.endianness("big", c(192L, 0L, 0L, 5L), struct(int32()), list(-1073741819))

##### little int32() #####
test.endianness.little.int32 <- function() assert.endianness("little", c(5L, 0L, 0L, 192L), int32(), -1073741819)
test.endianness.little.struct.int32 <- function() assert.endianness("little", c(5L, 0L, 0L, 192L), struct(int32()), list(-1073741819))

##### big int64() #####
test.endianness.big.int64 <- function() assert.endianness("big", c(0L, 0L, 0L, 0L, 63L, 255L, 255L, 251L), int64(), 1073741819)
test.endianness.big.struct.int64 <- function() assert.endianness("big", c(0L, 0L, 0L, 0L, 63L, 255L, 255L, 251L), struct(int64()), list(1073741819))

##### little int64() #####
test.endianness.little.int64 <- function() assert.endianness("little", c(251L, 255L, 255L, 63L, 0L, 0L, 0L, 0L), int64(), 1073741819)
test.endianness.little.struct.int64 <- function() assert.endianness("little", c(251L, 255L, 255L, 63L, 0L, 0L, 0L, 0L), struct(int64()), list(1073741819))

##### big real32() #####
test.endianness.big.real32 <- function() assert.endianness("big", c(65L, 32L, 0L, 0L), real32(), 10.0)
test.endianness.big.struct.real32 <- function() assert.endianness("big", c(65L, 32L, 0L, 0L), struct(real32()), list(10.0))

##### little real32() #####
test.endianness.little.real32 <- function() assert.endianness("little", c(0L, 0L, 32L, 65L), real32(), 10.0)
test.endianness.little.struct.real32 <- function() assert.endianness("little", c(0L, 0L, 32L, 65L), struct(real32()), list(10.0))

##### big real64() #####
test.endianness.big.real64 <- function() assert.endianness("big", c(64L, 36L, 0L, 0L, 0L, 0L, 0L, 0L), real64(), 10.0)
test.endianness.big.struct.real64 <- function() assert.endianness("big", c(64L, 36L, 0L, 0L, 0L, 0L, 0L, 0L), struct(real64()), list(10.0))

##### little real64() #####
test.endianness.little.real64 <- function() assert.endianness("little", c(0L, 0L, 0L, 0L, 0L, 0L, 36L, 64L), real64(), 10.0)
test.endianness.little.struct.real64 <- function() assert.endianness("little", c(0L, 0L, 0L, 0L, 0L, 0L, 36L, 64L), struct(real64()), list(10.0))

test.endianness.big.int16()
test.endianness.big.struct.int16()
test.endianness.little.int16()
test.endianness.little.struct.int16()
test.endianness.big.uint16()
test.endianness.big.struct.uint16()
test.endianness.little.uint16()
test.endianness.little.struct.uint16()
test.endianness.big.int32()
test.endianness.big.struct.int32()
test.endianness.little.int32()
test.endianness.little.struct.int32()
test.endianness.big.int64()
test.endianness.big.struct.int64()
test.endianness.little.int64()
test.endianness.little.struct.int64()
test.endianness.big.real32()
test.endianness.big.struct.real32()
test.endianness.little.real32()
test.endianness.little.struct.real32()
test.endianness.big.real64()
test.endianness.big.struct.real64()
test.endianness.little.real64()
test.endianness.little.struct.real64()
