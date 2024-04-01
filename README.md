Regalia is an implementation of the Common Lisp **array** class hierarchy.

### TODO

+ [ ] System Classes ARRAY, VECTOR, BIT-VECTOR, and other specialized array classes.
+ [ ] AREF should be implemented in terms of ROW-MAJOR-AREF.
+ [ ] ARRAY-DIMENSIONS
+ [ ] ARRAY-DIMENSION
+ [ ] ARRAY-ELEMENT-TYPE 
+ [ ] ARRAY-IN-BOUNDS-P
+ [ ] ARRAY-RANK
+ [ ] ARRAY-TOTAL-SIZE
+ [ ] FILL-POINTER
+ [ ] VECTOR-PUSH
+ [ ] VECTOR-PUSH-EXTEND using ADJUST-ARRAY (not sure though if Regalia can implement ADJUST-ARRAY)
+ [ ] Condition types and condition reporters
+ [ ] Documentation strings 
+ [ ] Packages and ASDF systems for intrinsic/extrinsic use
+ [ ] Possibly some compiler macros 

### Reference

+ [CLHS: Section The Arrays Dictionary](http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/Body/sec_the_arrays_dictionary.html)
+ [robert-strandh/Constrictor: A library that implements the functionality of the Conses dictionary of the Common Lisp standard](https://github.com/robert-strandh/Constrictor)
