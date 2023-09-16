# Libpq-fortran

Libpq-fortran is a Modern Fortran interface to the PostgreSQL `libpq` [C Library](https://www.postgresql.org/docs/current/libpq.html).

This does not contain the `libpq` library; only the wrapper is included.

## Features

### Current

- Build
   - Add to your `fpm.toml`
     ```toml
     [build]
     link = ["pq"]
     [dependencies]
     libpq-fortran = "https://github.com/shinobuamasaki/libpq-fortran"
     ```

   - Tested on Linux


- Supported Compilers
   - GNU Compiler Collection: `gfortran`,
   - Intel oneAPI HPC toolkit: `ifort`/`ifx`

- Supported PostgreSQL version
   - PostgreSQL v15.4 (libpq v5.15)


### Goals

*Note that below does not represent the current state of this package.*

- Libpq-fortran aims to wrap libpq as documented in the PostgreSQL documentation, including all non-deprecated functionallity with its *explicit* `interface`s.

### Non-Goals

This package will not:

- parse SQL
- emit SQL
- provide an interface handling transactions or cursors
- provide abstractions over common SQL patterns

### Dependencies

- Fortran Package Manager (`fpm`)
- the PostgreSQL `libpq` library

## Licenses

### `libpq` Source and PostgreSQL Documentation

```
Portions Copyright © 1996-2023, The PostgreSQL Global Development Group

Portions Copyright © 1994, The Regents of the University of California

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose, without fee, and without a written agreement is
hereby granted, provided that the above copyright notice and this
paragraph and the following two paragraphs appear in all copies.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
DOCUMENTATION, EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO
PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
```

### Everything Else
The license for the remainder of this package appears in [LICENSE](https://github.com/ShinobuAmasaki/libpq-fortran/blob/main/LICENSE).


## Acknowledgements
The creation of this package was inspired by iamed2's [LibPQ.jl](https://github.com/iamed2/LibPQ.jl) and a discussion in the [Fortran-jp](https://fortran-jp.org/) community.