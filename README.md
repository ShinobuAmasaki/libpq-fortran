# Libpq-Fortran

Libpq-Fortran is a Modern Fortran interface to the PostgreSQL `libpq` [C Library](https://www.postgresql.org/docs/current/libpq.html).

This does not contain the `libpq` library; only the wrapper is included.

The source of this package is available on [GitHub](https://github.com/ShinobuAmasaki/libpq-fortran).

## Features

### Current

- Supported Compilers
   - GNU Compiler Collection: `gfortran`,
   - Intel Fortran Compiler, Fortran Compiler Classic `ifx`/`ifort`,

- Supported PostgreSQL version
   - PostgreSQL v15.4 (libpq v5.15)

- Connection
   - Connection via DSN (data source name)
   - Connection via PostgreSQL connection string
   - UTF-8 client encoding

### Build

This package needs [Fortran Package Manager (`fpm`)](https://fpm.fortran-lang.org/index.html).

- `libpq` is required.
   - Add the path to the directory containing "libpq-fe.h" to the environment variable `FPM_CFLAGS`.
   - For Ubuntu, exec `sudo apt install libpq-dev`

- Add to your `fpm.toml`
   ```toml
   [build]
   link = ["pq"]
   [dependencies]
   libpq-fortran = {git = "https://github.com/shinobuamasaki/libpq-fortran"}
   ```

### Test

Tested on
   - FreeBSD 
      - Release 13.2 (`gfortran` v13.1.0)
   - Linux
      - Gentoo Linux (`gfortran` v13.2.1, `ifort` 2021.9.0, `ifx` 2023.1.0)
   - Windows
      - Microsoft Windows 10 (`gfortran` MinGW-W64 13.1.0)

The unit tests contained in the `test/` directory can be executed using the command `fpm test` on your environment,
but a local PostgreSQL server is required for this operation. 

In that case, an appropriate configuration must be set in the following PostgreSQL configuration files:

- `postgresql.conf`
   - Ensure that `localhost` is included in the `listen_address` configuration.
- `pg_hba.conf`
   - Set the "METHOD" to `trust` for the following two connection methods:

     ```
      # TYPE  DATABASE        USER            ADDRESS                 METHOD
      local   all             all                                     trust
      host    all             all             127.0.0.1/32            trust
     ```

Note: These configurations entail security risks, and it is your responsibility to appropriately configure them your own discretion.

After updating these configurations, it is necessary to restart the PostgreSQL server. On Linux, use the command for OpenRC or systemd.
For Windows, execute the following command in Powershell with administrator privileges:

```powershell
PS> net stop postgresql-x64-15
PS> net start postgresql-x64-15
```


### Goals

*Note that below does not represent the current state of this package.*

- Libpq-Fortran aims to wrap libpq as documented in the PostgreSQL documentation, including all non-deprecated functionallity with its *explicit interfaces*.

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
- [`uint-fortran`](https://github.com/ShinobuAmasaki/uint-fortran) (loaded by `fpm` automatically).

The top two need to be installed by the user themselves.

## Documentation

The documentation is available at https://shinobuamasaki.github.io./libpq-fortran

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


## Acknowledgement
The creation of this package was inspired by a discussion in the [Fortran-jp](https://fortran-jp.org/) community.

## Appendix: Implemented Interface Functions List

Note:
 - The order of the following headings and the functions contained within them follows [the PostgreSQL documentation](https://www.postgresql.org/docs/current/libpq.html).
 - The function with a strikethrough is not planned to be implemented.

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
- [x] `PQsslInUse`
- [x] `PQsslAttribute`
- [x] `PQsslAttributeNames`
- [ ] `PQsslStruct`
- <s> `PQgetssl` </s>
 
### Command Execution Functions

#### Main Functions
- [x] `PQexec`
- [x] `PQexecParams` (only in text format)
- [x] `PQprepare`
- [x] `PQexecPrepared`
- [x] `PQdescribePrepared`
- [x] `PQdescribePortal`
- [x] `PQresultStatus`
- [x] `PQresStatus`
- [x] `PQresultErrorMessage`
- [x] `PQresultVerboseErrorMessage`
- [x] `PQresultErrorField`
- [x] `PQclear`
 
#### Retrieving Query Result Inform  ation
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
- [x] `PQcmdStatus`
- [x] `PQcmdTuples`
- [x] `PQoidValue`
- <s> `PQoidStatus`</s>

#### Escaping String for Inclusion in SQL Commands
- [x] `PQescapeLiteral` (plan to test)
- [x] `PQescapeIdentifier`
- [ ] `PQescapeStringConn`
- <s> `PQescapeString`</s>
- [ ] `PQescapeByteaConn`
- <s> `PQescapeBytea`</s>
- [ ] `PQunescapeBytea`

### Asynchronous Command Processing
- [x] `PQsendQuery`
- [x] `PQsendQueryParams`
- [x] `PQsendPrepare` (only in text format)
- [x] `PQsendQueryPrepared`
- [x] `PQsendDescribePrepared`
- [x] `PQsendDescribePortal` (not tested)
- [x] `PQgetResult`
- [x] `PQconsumeInput`
- [x] `PQisBusy`
- [x] `PQsetnonblocking`
- [x] `PQisnonblocking`
- [x] `PQflush` (not tested)

### Pipeline Mode
- [x] `PQpipelineStatus`
- [x] `PQenterPipelineMode`
- [x] `PQexitPipelineMode`
- [x] `PQpipelineSync`
- [x] `PQsendFlushRequest`

### Retrieving Query Result Row-by-Row
- [x] `PQsetSingleRowMode`

### Canceling Queries in Progress
- [x] `PQgetCancel`
- [x] `PQfreeCancel`
- [x] `PQcancel`
- <s>`PQrequestCancel`</s>

### Asynchronous Notification 
- [x] PQnotifies

### Functions Associated with the COPY Command
- [x] `PQputCopyData` (not tested)
- [x] `PQputCopyEnd` (not tested)
- [x] `PQgetCopyData` (not tested)
- <s>`PQgetline`</s>
- <s>`PQgetlineAsync`</s>
- <s>`PQputline`</s>
- <s>`PQputnbytes`</s>
- <s>`PQendcopy`</s>

### Control Functions
- [x] `PQclientEncoding`
- [x] `PQsetclientEncoding`
- [x] `PQsetErrorVerbosity`
- [x] `PQsetErrorContextVisibility`
- [ ] `PQtrace`
- [ ] `PQsettraceFlags`
- [ ] `PQuntrace`

### Miscellaneous Functions
- [x] `PQfreemem`
- [x] `PQconninfoFree`
- [x] `PQencryptPasswordConn`
- <s>`PQenctyptPassword`</s>
- [x] `PQmakeEmptyPGresult`
- [ ] `PQfireResultCreateEvents`
- [x] `PQcopyResult`
- [ ] `PQsetResultAttrs`
- [ ] `PQsetvalue`
- [ ] `PQresultAlloc`
- [ ] `PQresultMemorySize`
- [x] `PQlibVersion`

### Event System
- [ ] `PQregisterEventProc`
- [ ] `PQsetInstanceData`
- [ ] `PQresultSetInstanceData`
- [ ] `PQresultInstanceData`

### SSL Support
- [ ] `PQinitOpenSSL`
- [ ] `PQinitSSL`

### Behavior in Threaded Programs
- [x] `PQisthreadsafe`
