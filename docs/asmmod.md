# ASMMod file format
ASMMod is a file format used by the Callisto compiler for compiled modules. It contains
function defintions and top level code with their generated assembly

It is made up of sections, each one has a byte describing what kind of section it is:

| Byte | Meaning              |
| ---- | -------------------- |
| 0x00 | Top-level code       |
| 0x01 | Function definition  |
| 0x02 | Public import        |

Integers are big endian. Strings are null-terminated.

## Header
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 3            | ASCII magic bytes containing "MOD"                 |
| 3              | 2            | Version number, currently 0x0000                   |
| 5              | 2            | CPU architecture ID (see below)                    |
| 7              | 2            | Operating system ID (see below)                    |

### CPU architectures
"None" is used for transpiled languages (like when Callisto compiles to Lua)
| ID     | Name            |
| ------ | --------------- |
| 0x0000 | None            |
| 0x0001 | x86_64          |
| 0x0002 | x86 real mode   |
| 0x0003 | ARM64           |
| 0x0004 | Uxn             |
| 0x0005 | Fox32           |
| 0x0006 | Motorola 68000  |
| 0x0007 | RISC-V          |

### Operating systems
| ID     | Name            |
| ------ | --------------- |
| 0x0000 | None            |
| 0x0001 | Linux           |
| 0x0002 | macOS           |
| 0x0003 | Windows         |
| 0x0004 | MS-DOS          |
| 0x0005 | FreeBSD         |
| 0x0006 | Varvara         |
| 0x0007 | Fox32OS         |

# Sections
The following tables will include the type byte mentioned before

## Top level code
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x00                                 |
| 1              | 8            | Number of called functions                         |
| 9              | ?            | Array of strings containing called functions       |
| ?              | ?            | Assembly of top level code                         |

## Function definition
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x01                                 |
| 1              | 2            | Flags - see below                                  |
| 3              | 8            | Number of called functions                         |
| 11             | ?            | Array of strings containing called functions       |
| ?              | ?            | Assembly of function (includes label)              |

### Flags
Flags are OR'd together
| Number  | Description                      |
| ------- | -------------------------------- |
| 1       | Public                           |

# Public import
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x02                                 |
| 1              | ?            | String containing name of module                   |
