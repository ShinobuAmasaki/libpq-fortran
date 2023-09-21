# LibPQ-Fortran

LibPQ-Fortran is a Modern Fortran interface to the PostgreSQL `libpq` [C Library](https://www.postgresql.org/docs/current/libpq.html).

This does not contain the `libpq` library; only the wrapper is included.

The source of this package is available on [GitHub](https://github.com/ShinobuAmasaki/libpq-fortran).

## Features

### Build

This package needs [Fortran Package Manager (`fpm`)](https://fpm.fortran-lang.org/index.html).

- `libpq` is required
   - For Ubuntu, exec `sudo apt isntall libpq-dev`

- Add to your `fpm.toml`
   ```toml
   [build]
   link = ["pq"]
   [dependencies]
   libpq-fortran = "https://github.com/shinobuamasaki/libpq-fortran"
   ```

Tested on
   - FreeBSD
   - Linux
      - Gentoo Linux
      - Ubuntu 22.04 LTS

### Current

- Supported Compilers
   - GNU Compiler Collection: `gfortran`,
   - Intel oneAPI HPC toolkit: `ifort`/`ifx`,
   - LLVM Flang (version 15): `flang`/`flang15`.

Other versions of Flang are not tested.

- Supported PostgreSQL version
   - PostgreSQL v15.4 (libpq v5.15)


### Goals

*Note that below does not represent the current state of this package.*

- Libpq-fortran aims to wrap libpq as documented in the PostgreSQL documentation, including all non-deprecated functionallity with its *explicit interfaces*.

### Non-Goals

This package will not:

- parse SQL
- emit SQL
- provide an interface handling transactions or cursors
- provide abstractions over common SQL patterns

### Dependencies

This package depends on following applicaiton and libraries:

- Fortran Package Manager (`fpm`),
- The PostgreSQL `libpq` library
- `uint-fortran` (loaded by fpm automatically).

The top two need to be installed by the user themselves.

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
The creation of this package was inspired by a discussion in the [Fortran-jp](https://fortran-jp.org/) community.

## Appendix: Implemented Interface Functions List

Note: The order of the following headings and the functions contained within them follows [the PostgreSQL documentation](https://www.postgresql.org/docs/current/libpq.html).

### Database Connection Control Functions

- [x] `PQconnectdb`
- [x] `PQconnectdbParams`
- [x] `PQsetdbLogin`
- <s>`PQsetdb`</s>
- [x] `PQconnectStartParams`
- [x] `PQconnectStart`
- [x] `PQconnectPoll`
- [x] `PQconndefaults`
- [x] `PQconninfo`
- [x] `PQconninfoParse`
- [x] `PQfinish`
- [x] `PQreset`
- [x] `PQresetStart`
- [x] `PQresetPoll`
- [x] `PQpingParams`
- [x] `PQping`
- [ ] `PQsetSSLKeyPassHook_OpenSSL`
- [ ] `PQgetSSLKeyPassHook_OpenSSL`

### Connection Status Functions 

- [x] `PQdb`
- [x] `PQuser`
- [x] `PQpass`
- [x] `PQhost`
- [x] `PQhostaddr`
- [x] `PQport`
-  <s>`PQtty`</s>
- [x] `PQoptions`
- [x] `PQstatus`
- [x] `PQtransactionStatus`
- [x] `PQparameterStatus`
- [x] `PQprotocolVersion`
- [x] `PQserverVersion`
- [x] `PQerrorMessage`
- [x] `PQsocket`
- [x] `PQbackendPID`
- [x] `PQconnectionNeedsPassword`
- [x] `PQconnectionUsedPassword`
- [ ] `PQsslInUse`
- [ ] `PQsslAttribute`
- [ ] `PQsslAttributeNames`
- [ ] `PQsslStruct`
- [ ] `PQgetssl`
 
### Command Execution Functions

#### Main Functions
- [x] `PQexec`
- [ ] `PQexecParams`
- [ ] `PQprepare`
- [ ] `PQexecPrepared`
- [ ] `PQdescribePrepared`
- [ ] `PQdescribePortal`
- [x] `PQresultStatus`
- [x] `PQresStatus`
- [x] `PQresultErrorMessage`
- [x] `PQresultVerboseErrorMessage`
- [ ] `PQresultErrorField`
- [x] `PQclear`
 
#### Retrieving Query Result Information
- [x] `PQntuples`
- [x] `PQnfields`
- [x] `PQfname`
- [x] `PQfnumber`
- [x] `PQftable`
- [x] `PQftablecol`
- [x] `PQfformat`
- [x] `PQftype`
- [x] `PQfmod`
- [x] `PQfsize`
- [x] `PQbinaryTuples`
- [x] `PQgetvalue`
- [x] `PQgetisnull`
- [x] `PQgetlength`
- [x] `PQnparams`
- [x] `PQparamtype`
- <s>`PQprint`</s>

#### Retrieving Other Result Information
- [ ] `PQcmdStatus`
- [ ] `PQcmdTuples`
- [ ] `PQoidValue`
- [ ] `PQoidStatus`

#### Escaping String for Inclusion in SQL Commands
- [ ] `PQescapeLiteral`
- [ ] `PQescapeIdentifier`
- [ ] `PQescapeStringConn`
- [ ] `PQescapeString`
- [ ] `PQescapeByteaConn`
- [ ] `PQescapeBytea`
- [ ] `PQunescapeBytea`

### Asynchronous Command Processing
- [ ] `PQsendQuery`
- [ ] `PQsendQueryParams`
- [ ] `PQsendPrepare`
- [ ] `PQsendQueryPrepared`
- [ ] `PQsendDescribePrepared`
- [ ] `PQsendDescribePortal`
- [ ] `PQgetResult`
- [ ] `PQconsumeInput`
- [ ] `PQisBusy`
- [ ] `PQsetnonblocking`
- [ ] `PQisnonblocking`
- [ ] `PQflush`

### Pipeline Mode
- [ ] `PQpipelineStatus`
- [ ] `PQenterPipelineMode`
- [ ] `PQexitPipelineMode`
- [ ] `PQpipelineSync`
- [ ] `PQsendFlushRequest`

### Retrieving Query Result Row-by-Row
- [ ] `PQsetSingleRowMode`

### Canceling Queries in Progress
- [ ] `PQgetCancel`
- [ ] `PQfreeCancel`
- [ ] `PQcancel`
- [ ] `PQrequestCancel`

### Functions Associated with the COPY Command
- [ ] `PQnfields`
- [ ] `PQbinaryTuples`
- [ ] `PQfformat`
- [ ] `PQputCopyData`
- [ ] `PQputCopyEnd`
- [ ] `PQgetCopyData`
- <s>`PQgetline`</s>
- <s>`PQgetlineAsync`</s>
- <s>`PQputline`</s>
- <s>`PQputnbytes`</s>
- <s>`PQendcopy`</s>

### Control Functions
- [ ] `PQclientEncoding`
- [ ] `PQsetclientEncoding`
- [ ] `PQsetErrorVerbosity`
- [ ] `PQsetErrorContextVisibility`
- [ ] `PQtrace`
- [ ] `PQsettraceFlags`
- [ ] `PQuntrace`

### Miscellaneous Functions
- [x] `PQfreemem`
- [x] `PQconninfoFree`
- [ ] `PQencryptPasswordConn`
- [ ] `PQenctyptPassword`
- [ ] `PQmakeEmptyPGresult`
- [ ] `PQfireResultCreateEvents`
- [ ] `PQcopyResult`
- [ ] `PQsetResultAttrs`
- [ ] `PQsetvalue`
- [ ] `PQresultAlloc`
- [ ] `PQresultMemorySize`
- [x] `PQlibVersion`

### Event System
- [ ] `PGEventProc`
- [ ] `PQregisterEventProc`
- [ ] `PQsetInstanceData`
- [ ] `PQresultSetInstanceData`
- [ ] `PQresultInstanceData`

### SSL Support
- [ ] `PQinitOpenSSL`
- [ ] `PQinitSSL`

### Behavior in Threaded Programs
- [ ] `PQisthreadsafe`



