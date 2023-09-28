---
title: Quickstart
author: Amasaki Shinobu
date: 2023-09-28
---

# Quickstart

## Contents

- Introduction
- Dependencies
- Building
- Try it
- `program demo`
- Appendix

## Introduction

Libpq-Fortran utilizes the interoperability features of Modern Fortran to provide access to the PostgreSQL official client library, libpq.
At present, the documentation of this is in progress, so for detailed usage of the functions, please refer to [the PostgreSQL official documentation of libpq](https://www.postgreSQL.org/docs/current/libpq.html).  

In this article, we will explore the initial usage of this library.

## Dependencies

Here, we will discuss the software required for building Libpq-Fortran.

First, you will need the *libpq* installed on your operating system on the local machine. 

- On Windows, it comes as an option when you install PostgreSQL using the wizard.
- On Ubuntu Linux, you can simply run the command `sudo apt install libpq-dev`. 

Next, you will need a Fortran compiler. Currently, the following compilers are supported:

- GNU Compiler Collection: `gfortran`
- Intel oneAPI Fortran Compiler `ifx`, Fortran Compiler Classic `ifort`

Furthermore, Libpq-Fortran is managed through the Fortran Package Manager (`fpm`), so you will need this as well.

Additionally, it has a dependency on another library developed by the author, [`uint-fortran`](https://github.com/ShinobuAmasaki/uint-fortran), but `fpm` automatically handles this dependency.

Finally, you will need a so-called sandbox database server on your localhost or local network. 
*It is advisable to use a setup where data loss is acceptable for testing purposes. Consider this a warning.*

## Building

To get started with Libpq-Fortran, use the following commands: 

```shell
$ git clone https://github.com/ShinobuAmasaki/libpq-fortran
$ cd libpq-fortran
$ fpm build
```

When executing `fpm build`, you may need to specify the directory containing the `"libpq-fe.h"` C header  with `-I` flag in the environment variable `FPM_CFLAGS` or using `--c-flag`.

For example, on Ubuntu system, the location may be `/usr/include/postgreslq`, and then the command will be the following:

```shell
$ fpm build --c-flag "-I/usr/include/postgresql"
```

## Try it

The 'example' directory in the repository contains a program that interactively accesses a PostgreSQL server to retrieve a list of databases on that server.

To run this, execute the following command with `fpm run`:

```shell
$ fpm run demo --example
demo.f90                               done.
demo                                   done.
[100%] Project compiled successfully.
=== INPUT Database Information ===
=== type "q" for quit          ===
Hostname:
```

Success is indicated by the appearance of a prompt requesting input for accessing the server.

Upon entering the information as follows, the connection is established, and the results are returned:

```shell
=== INPUT Database Information ===
=== type "q" for quit          ===
Hostname: localhost
dbname: postgres
user: shinobu
password: xxx
=== END INPUT ===
=== Query Result===
tuples, fields: 4  1
=== Available database names ===
postgres
template1
template0
```

Please modify the hostname, dbname, username, and password to suit your own environment. 

Thus, you were able to access a PostgreSQL database and output results using code written in Fortran. In the next section, we will delve into the details of the main program used. 

## `program demo`

### Declaration statements

In this section, let's take closer look at the main program used in the above demonstration. 
The entire program can be found in the Appendix of this article or on [GitHub](https://github.com/ShinobuAmasaki/libpq-fortran/blob/main/example/demo.f90).

The declaration statements are as follows:

```fortran
program demo
   use :: libpq
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only:stdout=>output_unit, stdin=>input_unit

   type(c_ptr) :: conn, res
   character(:, kind=c_char), allocatable :: command, conninfo
   integer :: i, port
   character(256) :: str, host, dbname, user, password
```

Here, we start with the declaration `use :: libpq`, which plays a central role in this software.

Additionally, please note the declaration `use, intrinsic :: iso_c_binding`.

This is because the software interfaces directly with C libraries, requiring a C-style programming approach, 
involving passing and relaying pointers to objects as return values from functions.

`type(c_ptr) ::conn, res` are variables used to store C pointers received when programming in this style.

### Executable statements

Next, let's examine the execution statements. 

```fortran
   print *, '=== INPUT Database Information ==='
   print *, '=== type "q" for quit          ==='
   write (stdout, '(a)', advance='no') "Hostname: "
   read  (stdin, *) host
   if (host == 'q') stop

   port = 5432

   write (stdout, '(a)', advance='no') "dbname: "
   read  (stdin, *) dbname
   if (dbname == 'q') stop

   write (stdout, '(a)', advance='no') "user: "
   read  (stdin, *) user
   if (user == 'q') stop

   write (stdout, '(a)', advance='no') "password: "
   read  (stdin, *) password
   if (password == 'q') stop
   print *, "=== END INPUT ==="

   write(str, '(a, i0, a)') &
      "host="//trim(adjustl(host))// &
      " port=", port, &
      " dbname="//trim(adjustl(dbname))// &
      " user="//trim(adjustl(user))// &
      " password="//trim(adjustl(password))
```

This section is responsible for receiving input for connection information.

Here user input is collected into various fixed-length string variables, processed, and finally copied into the connection information string `conninfo`.

Furthermore, the next set of statements executes the connection to the database.

```fortran
   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if
```

The function `PQconnectdb` is called with the argument `conninfo`, and the result is assigned to the C pointer variable `conn`.

This pointer serves as an identifier for the connection and is used by the user  without needing to concerned about its internal structure.
Following this, the subsequent `if` block serves as error handling.

As we move forward, the part where command statements are executed becomes apparent.

```fortran
   query = "select datname from pg_database;"
   res = PQexec(conn, query)
   if (PQstatus(conn) /= 0 ) then
      print *, PQerrorMessage(conn)
   end if
```

Here, we first write the query into the string variable `query`.

Then, we call the `PQexec` function to execute the `query` on the `conn` connection and assign the result to the C pointer variable `res`.

The meaning of `select datname from pg_database;` is to retrieve a list of (in the sense of collection of tables) databases on the PostgreSQL server.

Following this, the subsequent `if` block serves as error handling. 

And finally, there is the process of extracting the actual data from the `res` pointer. 

```fortran
    print *, "=== Query Result==="
    print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 
 
    print *, "=== Available database names ==="
    do i = 0, PQntuples(res)-1
       print *, PQgetvalue(res, i, 0)
    end do
```

Here, we use the `PQntuples` and `PQnfields` functions to display the number of tuples (rows) and fields (columns) held by the `res` object.

Then, we iterate through the tuples, extracting the value of the 0th column of the `i`th row as a string.

It's worth noting that we are using 0-based indexing, following the conventions of C. 

The program written in this manner returns results such as the following: 

```shell
=== Query Result===
tuples, fields: 4  1
=== Available database names ===
postgres
template1
template0
```

 Success is indicated if it includes at least three of the following: postgres, template1, template0. 

## Appendix

The complete code for the program demonstrated above.

```fortran
program demo
   use :: libpq
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only:stdout=>output_unit, stdin=>input_unit

   type(c_ptr) :: conn, res
   character(:, kind=c_char), allocatable :: query, conninfo
   integer :: i, port
   character(256) :: str, host, dbname, user, password

   print *, '=== INPUT Database Information ==='
   print *, '=== type "q" for quit          ==='
   
   write (stdout, '(a)', advance='no') "Hostname: "
   read  (stdin, *) host
   if (host == 'q') stop

   port = 5432

   write (stdout, '(a)', advance='no') "dbname: "
   read  (stdin, *) dbname
   if (dbname == 'q') stop

   write (stdout, '(a)', advance='no') "user: "
   read  (stdin, *) user
   if (user == 'q') stop

   write (stdout, '(a)', advance='no') "password: "
   read  (stdin, *) password
   if (password == 'q') stop
   print *, "=== END INPUT ==="

   write(str, '(a, i0, a)') &
      "host="//trim(adjustl(host))// &
      " port=", port, &
      " dbname="//trim(adjustl(dbname))// &
      " user="//trim(adjustl(user))// &
      " password="//trim(adjustl(password))
     
   conninfo = str

   conn = PQconnectdb(conninfo)
   if (PQstatus(conn) /= 0) then
      print *, PQerrorMessage(conn)
      error stop
   end if

   ! The query to retrieve the names of databases within a database cluster is: 
   query = "select datname from pg_database;"
   res = PQexec(conn, query)
   if (PQstatus(conn) /= 0 ) then
      print *, PQerrorMessage(conn)
   end if

   print *, "=== Query Result==="
   print '(a, i0, 2x, i0)', 'tuples, fields: ', PQntuples(res), PQnfields(res) 

   print *, "=== Available database names ==="
   do i = 0, PQntuples(res)-1
      print *, PQgetvalue(res, i, 0)
   end do

   call PQclear(res)
   call PQfinish(conn)


end program demo
```

