Access PostgreSQL via Fortran.

Libpq-Fortran is a Modern Fortran interface to the PostgreSQL `libpq` [C Library](https://www.postgresql.org/docs/current/libpq.html)

@note
Work in progress
@endnote

## Feature

### Current

Supported compilers are following:

- GNU Compiler Collection `gfortran`,
- Intel Fortran Compiler `ifx`, Fortran Compiler Classic `ifort`.

Supported PostgreSQL version is

- PostgreSQL v15.4 (libpq v5.15)

### Build

This package needs,

-  [Fortran Package Manager](https://fpm.fortran-lang.org/index.html) `fpm`, and
-  [the PostgreSQL C Library](https://www.postgresql.org/docs/current/libpq.html) `libpq` (for Ubuntu, exec `sudo apt install libpq-dev`).

Add the path to the directory containing `libpq-fe.h` to the environment variable `FPM_CFLAGS`.


Next, add below to your `fpm.toml`:

```toml
[build]
link = ["pq"]
[dependencies]
libpq-fortran = {git ="https://github.com/shinobuamasaki/libpq-fortran"}
```


And then, do `fpm build` to build your project.

#### Tests

This package is tested on:

- FreeBSD Release 13.2 (`gfortran` v13.1.0)
- Gentoo Linux (`gfortran` v12.3.1, `ifort` 2021.9.0, `ifx` 2023.1.0)
- Ubuntu 22.04 LTS (`gfortran` v11.4.0)
- Microsoft Windows 10 (`MinGW-W64 gfortran`, v13.1.0)

#### Dependencies

This package depends on following applicaiton and libraries:

- Fortran Package Manager (`fpm`),
- The PostgreSQL `libpq` library
- [`uint-fortran`](https://github.com/ShinobuAmasaki/uint-fortran) (loaded by `fpm` automatically).

The top two need to be installed by the user themselves.

### Licenses

#### `libpq` Source and PostgreSQL Documentation


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
> 
> THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
> INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
> AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
> ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATIONS TO
> PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.


#### Everything Else

The license for the remainder of this package appears in [LICENSE](https://github.com/ShinobuAmasaki/libpq-fortran/blob/main/LICENSE).
