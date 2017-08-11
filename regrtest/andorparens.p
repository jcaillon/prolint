/*------------------------------------------------------------------------
    File        : andorparens.p
    Purpose     : 

    Syntax      :

    Description : unit-test cases

    Author(s)   : jurjen
    Created     : Sat Apr 12 19:12:16 CEST 2008
    Notes       :
  ----------------------------------------------------------------------*/

FIND FIRST customer WHERE customer.city="" OR customer.city="boston".

FIND FIRST customer WHERE customer.city="" OR customer.city="boston" OR customer.creditlimit<500.

FIND FIRST customer WHERE customer.city="" OR customer.city="boston" AND customer.name BEGINS "john".

FIND FIRST customer WHERE customer.name BEGINS "john" AND customer.city="" OR customer.city="boston".

FIND FIRST customer WHERE customer.name BEGINS "john" AND customer.city="" OR customer.city="boston" AND customer.creditlimit > 10000.

FIND FIRST customer WHERE customer.name BEGINS "john" AND customer.creditlimit > 10000 AND customer.city="" OR customer.city="boston".

FIND FIRST customer WHERE (customer.city="" OR customer.city="boston") AND customer.name BEGINS "john".

FIND FIRST customer WHERE customer.name BEGINS "john" AND (customer.city="" OR customer.city="boston").

FIND FIRST customer WHERE (customer.name BEGINS "john" AND customer.city="") OR customer.city="boston".

FIND FIRST customer WHERE (customer.name BEGINS "john" AND customer.city="") OR customer.city="boston" AND customer.creditlimit > 10000.

FIND FIRST customer WHERE (customer.name BEGINS "john" AND customer.creditlimit > 10000) AND (customer.city="" OR customer.city="boston").


