# FlexibleArrays

[![Build Status](https://travis-ci.org/eschnett/FlexibleArrays.jl.svg?branch=master)](https://travis-ci.org/eschnett/FlexibleArrays.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/2i24ij8n7ong4lxk/branch/master?svg=true)](https://ci.appveyor.com/project/eschnett/flexiblearrays-jl/branch/master)
[![codecov.io](https://codecov.io/github/eschnett/FlexibleArrays.jl/coverage.svg?branch=master)](https://codecov.io/github/eschnett/FlexibleArrays.jl?branch=master)

Flexible multi-dimensional arrays, with arbitrary lower and upper bounds that can be fixed at compile time to improve efficiency

## Background

Sometimes you really want arrays where the lower index bound is different from 1. This is not a question of performance, but of convenience -- for example, if you have quantum numbers ranging from `0` to `k`, then adding `1` every time you index an array looks tedious. This really should be part of the type declaration.

And sometimes, you know the size of a particular array dimension ahead of time. This is now a question of efficiency -- indexing into a multi-dimensional array is significantly more efficient if the sizes of the dimensions are known ahead of time.

This is just what this package `FlexibleArrays` provides: A way to define multi-dimensional array types where both lower and upper index bounds can be chosen freely, and which generates more efficient code if these bounds are known ahead of time.

Here is an example:
```Julia
using FlexibleArrays

# A (10x10) fixed-size array
typealias Arr2d_10x10 FlexArray(1:10, 1:10)
a2 = Arr2d_10x10{Float64}(:,:)

# A 3d array with lower index bounds 0
typealias Arr3d_lb0 FlexArray(0, 0, 0)
a3 = Arr3d_lb0{Float64}(9, 9, 9)

# A generic array, all bounds determined at creation time
typealias Arr4d_generic FlexArray(:, :, :, :)
a4 = Arr4d_generic{Float64}(1:10, 0:10, -1:10, 15:15)

# These can be mixed: A (2x10) array
FlexArray(0:1, 1){Float64}(:, 10)

# Arrays can also be empty:
FlexArray(4:13, 10:9)
FlexArray(:){Int}(5:0)

# The trivial 0d array type, always holding one scalar value:
FlexArray(){Int}
```

Flexible arrays are accessed like regular arrays, using the `[]` notation or the `getindex` and `setindex!` functions.

You will have noticed the slightly unusual notation for flexible arrays. Implementation-wise, the set of all bounds that are kept fixed determine the (parameterized) type of the array; different choices for fixed array bounds correspond to different types. `FlexArray` is a function that returns the respective type, creating this type if necessary.

Currently, flexible arrays do not yet support resizing, reshaping, or subarrays; adding this would be straightforward.

I designed FlexibleArrays for the two reasons above -- I needed to model quantum numbers that have a range of `0:k`, and I noticed that the C++ version of the respecived code became significantly faster when setting array sizes at compile time. In Julia, I confirmed that the generated machine code is also much simpler in this case. Of course, this provides a benefit only if array accessing is actually a bottleneck in the code.

## Manual

Each array dimension has a lower and an upper bound. Both can either be fixed or flexible. Fixed bounds are the same for all objects of this type, flexible bounds have to be chosen when the array is allocated. Julia's `Array` types correspond to `FlexArray` types where all lower bounds are fixed to `1`, and all upper bounds are flexible.

Internally, the fixed bounds are represented as a tuple of either nothing or integers: `DimSpec = NTuple{2, Union{Void, Int}}`.

For each dimension, the fixed bounds are set via:
- A range `lb:ub` to define both bounds
- An integer `lb` to define the lower bound, leaving the upper bound flexible
- A colon `:` to indicate that both lower and upper bounds are flexible
Instead of the above, you can also define the fixed bounds via a tuple of type `DimSpec`.

When an array is allocated, the flexible bounds are set conversely:
- A colon `:` indicates no flexible bounds (if both bounds are fixed)
- An integer `ub` defines the upper bound (if the lower bound is fixed)
- A range `lb:ub` defines both lower and upper bounds
- A one-element integer tuple `(lb,)` defines the lower bound (if the upper bound is fixed)

Each flexible array type is subtype of `AbstractFlexArray{T,N}`, where `T` is the element type and `N` is the rank. This abstract is a subtype of `DenseArray{T,N}`.

### Available array functions:

- Define a flexible array type:

  ```Julia
  FlexArray(<dimspec>*)
  ```

  Example:

  ```Julia
  typealias MyArrayType = FlexArray(1:2, 1, :)
  ```

- Allocate an array:

  ```Julia
  FlexArray(<dimspec>*){<type>}(<flexible bounds>*)
  ```

  Example:
  Create an array with bounds `(1:2, 1:10, 1:10)`:

  ```Julia
  myarray = MyArrayType{Float64}(:, 10, 1:10)
  ```

- Element type:

  ```Julia
  eltype{T}(arr::FlexArray(...){T})
  eltype{T}(::Type{FlexArray(...){T}})
  ```

  Example:

  ```Julia
  eltype(myarray)
  eltype(MyArrayType)
  ```

- Rank (number of dimension):

  ```Julia
  ndims{T}(arr::FlexArray(...){T})
  ndims{T}(::Type{FlexArray(...){T}})
  ```

  Example:

  ```Julia
  ndims(myarray)
  ndims(MyArrayType)
  ```

- Array length:

  ```Julia
  length{T}(arr::FlexArray(...){T})
  ```

  If all bounds are fixed, then the array length can also be obtained from the type:

  ```Julia
  length{T}(::Type{FlexArray(...){T}})
  ```

  Example:

  ```Julia
  length(myarray)
  ```

- Array bounds and sizes:

  ```Julia
  lbnd{T}(arr::FlexArray(...){T}, n::Int)
  ubnd{T}(arr::FlexArray(...){T}, n::Int)
  size{T}(arr::FlexArray(...){T}, n::Int)
  lbnd{T}(arr::FlexArray(...){T})
  ubnd{T}(arr::FlexArray(...){T})
  size{T}(arr::FlexArray(...){T})
  ```

  Fixed bounds and sizes can also be obtained from the type:

  ```Julia
  lbnd{T,n}(::Type{FlexArray(...){T}}, ::Val{n})
  ubnd{T,n}(::Type{FlexArray(...){T}}, ::Val{n})
  size{T,n}(::Type{FlexArray(...){T}}, ::Val{n})
  lbnd{T}(::Type{FlexArray(...){T}})
  ubnd{T}(::Type{FlexArray(...){T}})
  size{T}(::Type{FlexArray(...){T}})
  ```

  Example:

  ```Julia
  lbnd(myarray, 3)
  ubnd(myarray, Val{1})
  size(myarray, 2)
  lbnd(myarray)
  ubnd(myarray)
  size(myarray)
  ```

- Access array elements:

  ```Julia
  getindex{T}(arr::FlexArray(...){T}, i::Int, j::Int, ...)
  setindex!{T}(arr::FlexArray(...){T}, val, i::Int, j::Int, ...)
  ```

  Example:

  ```Julia
  myarray[1,2,3]
  myarray[2,3,4] = 42
  ```
