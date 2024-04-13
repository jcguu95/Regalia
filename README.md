Regalia is an implementation of the Common Lisp **array** class hierarchy.

### TODO

1. Provide code for the following.

   + [ ] System Class ARRAY (NOTE: SPEC is long)
   + [X] System Class VECTOR
   + [X] System Class BIT-VECTOR
   + [ ] Other specialized array classes.
   + [X] ARRAY-ROW-MAJOR-INDEX
   + [X] Implement MAKE-ARRAY-CONTENTS (extrinsic), and add note for the intrinsic version.
   + [X] ROW-MAJOR-AREF (extrinsic: in terms of the returned object from MAKE-ARRAY-CONTENTS)
   + [X] AREF (in terms of ROW-MAJOR-AREF).
   + [X] ARRAY-DIMENSIONS
   + [X] ARRAY-DIMENSION
   + [X] ARRAY-ELEMENT-TYPE 
   + [X] ARRAY-IN-BOUNDS-P
   + [X] ARRAY-RANK
   + [X] ARRAY-TOTAL-SIZE
   + [X] FILL-POINTER
   + [ ] ADJUST-ARRAY (NOTE: SPEC is long)
   + [X] VECTOR-PUSH
   + [ ] VECTOR-PUSH-EXTEND (use ADJUST-ARRAY)
   
2. Document, Polish and Clean Up

   + [ ] Condition types and condition reporters
   + [ ] Documentation strings 
   + [ ] Packages and ASDF systems for intrinsic/extrinsic use
   + [ ] Possibly some compiler macros 

3. Test them using ANSI-suite mimicking [Consecution](https://github.com/s-expressionists/Consecution/).

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
