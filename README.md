Regalia is an implementation of the Common Lisp **array** class hierarchy.

### TODO

1. Provide code for the following.

   + [ ] System Class ARRAY 
   + [ ] System Class VECTOR
   + [ ] System Class BIT-VECTOR
   + [ ] Other specialized array classes.
   + [X] ARRAY-ROW-MAJOR-INDEX
   + [X] Implement MAKE-ARRAY-CONTENTS (extrinsic), and add note for the intrinsic version.
   + [ ] ROW-MAJOR-AREF (extrinsic: in terms of the returned object from MAKE-ARRAY-CONTENTS)
   + [ ] AREF (in terms of ROW-MAJOR-AREF).
   + [X] ARRAY-DIMENSIONS
   + [X] ARRAY-DIMENSION
   + [X] ARRAY-ELEMENT-TYPE 
   + [ ] ARRAY-IN-BOUNDS-P
   + [X] ARRAY-RANK
   + [ ] ARRAY-TOTAL-SIZE
   + [ ] FILL-POINTER
   + [ ] VECTOR-PUSH
   + [ ] VECTOR-PUSH-EXTEND using ADJUST-ARRAY (not sure though if Regalia can implement ADJUST-ARRAY)
   + [ ] Condition types and condition reporters
   + [ ] Documentation strings 
   + [ ] Packages and ASDF systems for intrinsic/extrinsic use
   + [ ] Possibly some compiler macros 

2. Document all of them.

### Notes

+ Add docstrings and reference links to Lispworks's CLHS. 
+ Quote contents from dpANS or [NovaSpec](https://novaspec.org/cl/).
+ For tests, see what [Consecution](https://github.com/s-expressionists/Consecution/) did.
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
