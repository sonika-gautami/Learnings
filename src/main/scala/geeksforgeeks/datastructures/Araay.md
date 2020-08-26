###Arrays:
    - Stored at contiguous memory locations
    - This makes it easier to calculate the position of each element by simply adding an offset to a base value,
        i.e. the memory location of the first element of the array (generally denoted by the name of the array)
    - Primitive values -> stored at > continuous memory locations
    - Object -> stored at -> heap memory (only their references are stored at continuous memory loctaions)     

#####Advantages of using arrays:
    - Arrays allow random access of elements. This makes accessing elements by position faster.
    - Arrays have better cache locality that can make a pretty big difference in performance.
    
>Note:<br/>
>In Java: 
> - All objects are dynamically allocated on Heap.<br/>
> - When we only declare a variable of a class type, only a reference is created (memory is not allocated for the object).
> - To allocate memory to an object, we must use `new`. So the object is always allocated **memory on heap**



In computer science,
locality of reference, also known as the principle of locality,
 is the tendency of a processor to access the same set of memory locations repetitively
  over a short period of time.
 There are two basic types of reference locality
  – temporal and spatial locality. 
 Temporal locality refers to the reuse of specific data, and/or resources,
  within a relatively small time duration. 
 Spatial locality refers to the use of data elements within relatively close storage locations.
  Sequential locality, a special case of spatial locality,
   occurs when data elements are arranged and accessed linearly,
   such as, traversing the elements in a one-dimensional array.

Locality is a type of predictable behavior that occurs in computer systems.
Systems that exhibit strong locality of reference are 
great candidates for performance optimization 
through the use of techniques such as
 the caching,
 prefetching for memory and
  advanced branch predictors at the pipelining stage of a processor core.    