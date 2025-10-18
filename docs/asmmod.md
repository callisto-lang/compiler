# ASMMod file format
ASMMod is a file format used by the Callisto compiler for compiled modules. It contains
function defintions and top level code with their generated assembly

It is made up of sections, each one has a byte describing what kind of section it is:

| Byte | Meaning              |
| ---- | -------------------- |
| 0x00 | Top-level code       |
| 0x01 | Function definition  |
| 0x02 | Import               |
| 0x03 | Enable statement     |
| 0x04 | Const statement      |
| 0x05 | Enum statement       |
| 0x06 | Restrict statement   |
| 0x07 | Union statement      |
| 0x08 | Alias statement      |
| 0x09 | Implement statement  |
| 0x0A | Let statement        |
| 0x0B | Structure            |
| 0x0C | BSS section assembly |
| 0x0D | Data section         |
| 0x0E | Extern function      |

Integers are little endian. Strings are null-terminated.

## Header
| Offset (bytes) | Size (bytes) | Description                                             |
| -------------- | ------------ | ------------------------------------------------------- |
| 0              | 3            | ASCII magic bytes containing "MOD"                      |
| 3              | 1            | Flags                                                   |
| 4              | 8            | Version number, currently 0x0000                        |
| 12             | 8            | CPU architecture ID (see below)                         |
| 20             | 8            | Operating system ID (see below)                         |
| 28             | 8            | Number of sections                                      |
| 36             | 8            | String containing full path of the original source file |

#### Flags
| Bit | Value                                                      |
| --- | ---------------------------------------------------------- |
| 0   | 1 - this module is a stub, 0 - this is a full module       |
| 1   | 1 - this is the main module, 0 - it is not the main module |

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
| ?              | ?            | Function name                                      |
| ?              | 8            | Number of parameters                               |
| ?              | 8            | Number of return values                            |

### Flags
Flags are OR'd together
| Number  | Description                            |
| ------- | -------------------------------------- |
| 1       | Public                                 |
| 2       | Inline (the assembly is callisto code) |
| 4       | Throws errors                          |

# Import
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x02                                 |
| 1              | 1            | Boolean - public?
| 2              | ?            | String containing name of module                   |

## Enable statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x03                                 |
| 1              | ?            | String containing version to enable                |

## Const statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x04                                 |
| 1              | 8            | Const value                                        |
| 9              | ?            | String containing const name                       |

## Enum statement
### Enum header
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x05                                 |
| 1              | 8            | Number of enum values                              |
| 9              | ?            | Enum name                                          |
| ?              | ?            | String - enum type                                 |

Enum entries follow this header.

### Enum entry
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 8            | Enum value                                         |
| 8              | ?            | Enum name                                          |

## Restrict statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x06                                 |
| 1              | ?            | String containing version to restrict              |

## Union staement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x07                                 |
| 1              | ?            | Union name                                         |
| ?              | 8            | Number of types in union                           |
| ?              | ?            | String array with type names                       |

## Alias statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x08                                 |
| 1              | ?            | Original type name                                 |
| ?              | ?            | New type name                                      |

## Implement statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x09                                 |
| 1              | ?            | Type name                                          |
| ?              | ?            | Method name                                        |
| ?              | 8            | Number of called functions                         |
| ?              | ?            | Array of strings containing called functions       |
| ?              | ?            | Assembly of compiled implement statement           |

## Let statement
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x0A                                 |
| 1              | 1            | Boolean - is this an array?                        |
| 2              | 8            | Array size                                         |
| 10             | 1            | Boolean - is this a pointer?                       |
| 10             | ?            | Type                                               |
| ?              | ?            | Variable name                                      |

## Struct
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x0B                                 |
| 1              | ?            | Structure name                                     |
| ?              | ?            | String - parent structure, empty if none           |
| ?              | 8            | Number of structure entries                        |
| ?              | ?            | Structure entries                                  |

### Structure entries
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Bool - is pointer?                                 |
| 1              | ?            | Type name                                          |
| ?              | 1            | Bool - is array?                                   |
| ?              | 8            | Array length, if applicable                        |
| ?              | 8            | Offset in structure                                |
| ?              | ?            | Member name                                        |

## BSS Section assembly
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x0C                                 |
| 1              | ?            | Assembly                                           |

## Data section assembly
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x0D                                 |
| 1              | ?            | Assembly                                           |

## Extern function
| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Type - set to 0x0E                                 |
| 1              | 1            | Extern type - see below table                      |
| 2              | 4            | Number of return values                            |
| 4              | 4            | Number of parameters                               |
| 8              | ?            | String - symbol name                               |
| ?              | ?            | String - function name                             |

After this, there are the return values, and then the parameters. Both have this
format per value:

| Offset (bytes) | Size (bytes) | Description                                        |
| -------------- | ------------ | -------------------------------------------------- |
| 0              | 1            | Bool - is pointer?                                 |
| 1              | ?            | Name of type                                       |

Extern types:

| Value | Name        |
| ----- | ----------- |
| 0     | Callisto    |
| 1     | Raw         |
| 2     | C           |

Callisto externs are currently due to be replaced with a better version, and raw
externs are going to be removed. Do not use either.

C externs must have either no return values (void) or one return value.
