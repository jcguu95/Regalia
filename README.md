Regalia is an implementation of the Common Lisp **array** class hierarchy.

### TODO

1. Provide entries for the extrinsic versions.

   + [X] Implement MAKE-ARRAY-CONTENTS (extrinsic), and add note for the intrinsic version.
   + [X] System Class ARRAY 
   + [X] Type SIMPLE-ARRAY
   + [X] System Class VECTOR
   + [X] Type SIMPLE-VECTOR 
   + [X] System Class BIT-VECTOR
   + [X] Type SIMPLE-BIT-VECTOR 
   + [X] Function MAKE-ARRAY
   + [ ] Function ADJUST-ARRAY (NOTE: SPEC is long)
   + [X] Function ADJUSTABLE-ARRAY-P
   + [X] Accessor AREF
   + [X] Function ARRAY-DIMENSION
   + [X] Function ARRAY-DIMENSIONS
   + [X] Function ARRAY-ELEMENT-TYPE
   + [X] Function ARRAY-HAS-FILL-POINTER-P
   + [X] Function ARRAY-DISPLACEMENT
   + [X] Function ARRAY-IN-BOUNDS-P
   + [X] Function ARRAY-RANK
   + [X] Function ARRAY-ROW-MAJOR-INDEX
   + [X] Function ARRAY-TOTAL-SIZE
   + [X] Function ARRAYP
   + [X] Accessor FILL-POINTER
   + [X] Accessor ROW-MAJOR-AREF
   + [X] Function UPGRADED-ARRAY-ELEMENT-TYPE 
   + [X] Constant Variable ARRAY-DIMENSION-LIMIT
   + [X] Constant Variable ARRAY-RANK-LIMIT
   + [X] Constant Variable ARRAY-TOTAL-SIZE-LIMIT
   + [X] Function SIMPLE-VECTOR-P
   + [X] Accessor SVREF 
   + [X] Function VECTOR (TODO: Find out what constitutes an object of type VECTOR.)
   + [X] Function VECTOR-POP
   + [X] Function VECTOR-PUSH, VECTOR-PUSH-EXTEND
     + [X] Function VECTOR-PUSH
     + [X] Function VECTOR-PUSH-EXTEND [WAITING FOR: ADJUST-ARRAY]
   + [X] Function VECTORP
   + [X] Accessor BIT, SBIT
   + [X] Function BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR
   + [X] Function BIT-VECTOR-P
   + [X] Function SIMPLE-BIT-VECTOR-P

2. Test them using ANSI-suite mimicking
   [Consecution](https://github.com/s-expressionists/Consecution/).

3. Document, Polish and Clean Up

   + [ ] Condition types and condition reporters
   + [ ] Documentation strings 
   + [ ] Packages and ASDF systems for intrinsic/extrinsic use
   + [ ] Possibly some compiler macros 

4. Implement the intrinsic version.

### Notes

+ Add docstrings and reference links to Lispworks's CLHS. 
+ Quote contents from dpANS or [NovaSpec](https://novaspec.org/cl/).
+ "Notes" in CLHS cannot always be trusted.
+ In the extrinsic version, regalia must provide everything that the ARRAY
  module needs. In the intrinsic version relies on some operators provide by
  the host. For example, in the intrinsic version, the definition of
  MAKE-ARRAY-CONTENTS is provided by the client. But in the extrinsic version,
  regalia defines MAKE-ARRAY-CONTENTS using whatever the host provides.

### Reference

+ [NovaSpec](https://novaspec.org/cl/)
+ [CLHS: Chapter 15. Arrays](https://www.lispworks.com/documentation/lw60/CLHS/Body/15_.htm)
+ [robert-strandh/Constrictor: A library that implements the functionality of the Conses dictionary of the Common Lisp standard](https://github.com/robert-strandh/Constrictor)
