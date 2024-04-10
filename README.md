Regalia is an implementation of the Common Lisp **array** class hierarchy.

### TODO

1. Provide code for the following.

   + [ ] System Class ARRAY 
   + [ ] System Class VECTOR
   + [ ] System Class BIT-VECTOR
   + [ ] Other specialized array classes.
   + [X] ARRAY-ROW-MAJOR-INDEX
   + [ ] AREF, in terms of ARRAY-ROW-MAJOR-INDEX and ROW-MAJOR-AREF (provided by the host).
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

### Reference

+ [NovaSpec](https://novaspec.org/cl/)
+ [CLHS: Chapter 15. Arrays](https://www.lispworks.com/documentation/lw60/CLHS/Body/15_.htm)
+ [robert-strandh/Constrictor: A library that implements the functionality of the Conses dictionary of the Common Lisp standard](https://github.com/robert-strandh/Constrictor)
