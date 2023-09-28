Libpq-Fortran is a Modern Fortran interface to the PostgreSQL `libpq` [C library](https://www.postgresql.org/docs/current/libpq.html).

This does not contain the `libpq` library; only the wrapper is included.

The source of this package is available on [GitHub - shinobuamasaki/libpq-fortran](https://github.com/ShinobuAmasaki/libpq-fortran).

## Features

### Current

Supported compilers are:

- GNU Compiler Collection: `gfortran`,
- Intel oneAPI Fortran Compiler `ifx`, Fortran Compiler Classic `ifort`.

Supported PostgreSQL version:

- PostgreSQL v15.4 (libpq v5.15)

Connections:

- Connection via DSN (data source name)
- Connection via PostgreSQL connection string
- UTF-8 client encoding

### Build

This package is managed by [Fortran Package Manager](https://fpm.fortran-lang.org/index.html) (`fpm`).

The `libpq` is required and you may need to add the path to the directory containing `libpq-fe.h` to the environment variable `FPM_CFLAGS`.

Add to fpm.toml in your project:

```toml
[build]
link = ["pq"]
[dependencies]
libpq-fortran = {git = "https://github.com/shinobuamasaki/libpq-fortran"}
```

### Test

This package is tested on the following environments:

- FreeBSD (Release 13.2, `gfortran` v13.1.0)
- Gentoo Linux (`gfortran` v12.3.1, `ifort` 2021.9.0, `ifx` 2023.1.0)
- Ubuntu 22.04 LTS (`gfortran` v11.4.0)
- Microsoft Windows 10 (`gfortran` MinGW-W64 v13.1.0)

### Goals

*Note that below does not represent the current state of this package.*

Libpq-Fortran aims to wrap libpq as documented in the PostgreSQL documentation, including all non-deprecated functionallity with its **explicit interfaces**.


### Non-Goals

This package will not:

- parse SQL,
- emit SQL,
- provide an interface handling transactions or cursors,
- provide abstractions over common SQL patterns.


## Licenses

### `libpq` Source and PostgreSQL Documentation


> Portions Copyright © 1996-2023, The PostgreSQL Global Development Group
> 
> Portions Copyright © 1994, The Regents of the University of California
>
> Permission to use, copy, modify, and distribute this software and its
> documentation for any purpose, without fee, and without a written agreement is
> hereby granted, provided that the above copyright notice and this
> paragraph and the following two paragraphs appear in all copies.
>
> IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
> DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
> LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
> DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE
> POSSIBILITY OF SUCH DAMAGE.

> THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
> INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
> AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
> ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO
> PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

### Everything Else
The license for the remainder of this package appears in [LICENSE](https://github.com/ShinobuAmasaki/libpq-fortran/blob/main/LICENSE).

## Acknowledgement
The creation of this package was inspired by a discussion in the [Fortran-jp](https://fortran-jp.org/) community.