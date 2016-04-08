using FlexibleArrays
using Base.Test

FlexArray()

FlexArray(1:10)
FlexArray(1)
FlexArray(:)

FlexArray(1:10,1:10)
FlexArray(1:10,1)
FlexArray(1:10,:)
FlexArray(1,1:10)
FlexArray(1,1)
FlexArray(1,:)
FlexArray(:,1:10)
FlexArray(:,1)
FlexArray(:,:)

FlexArray(){Int}(nothing)
FlexArray(){Int}()

FlexArray(1:10){Int}(nothing)
FlexArray(1){Int}(nothing, 10)
FlexArray(:){Int}(nothing, 1, 10)
FlexArray(1:10){Int}(:)
FlexArray(1){Int}(10)
FlexArray(:){Int}(1:10)

FlexArray(1:10,1:10){Int}(:,:)
FlexArray(1:10,1){Int}(:,10)
FlexArray(1:10,:){Int}(:,1:10)
FlexArray(1,1:10){Int}(10,:)
FlexArray(1,1){Int}(10,10)
FlexArray(1,:){Int}(10,1:10)
FlexArray(:,1:10){Int}(1:10,:)
FlexArray(:,1){Int}(1:10,10)
FlexArray(:,:){Int}(1:10,1:10)

# 0D

Arr0 = FlexArray(){Float64}
Arr0b = FlexArray(){Int}
@test eltype(Arr0) === Float64
@test ndims(Arr0) === 0
@test length(Arr0) === 1
@test lbnd(Arr0) === ()
@test ubnd(Arr0) === ()
@test size(Arr0) === ()

arr0 = Arr0()
@test eltype(arr0) === Float64
@test ndims(arr0) === 0
@test length(arr0) === 1
@test lbnd(arr0) === ()
@test ubnd(arr0) === ()
@test size(arr0) === ()

arr0[] = 42
@test arr0[] === 42.0
@test string(arr0) == "42.0 "

# 1D

Arr1_fix = FlexArray(1:10){Float64}
@test eltype(Arr1_fix) === Float64
@test length(Arr1_fix) === 10
@test lbnd(Arr1_fix, 1) === 1
@test ubnd(Arr1_fix, 1) === 10
@test size(Arr1_fix, 1) === 10
arr1_fix = Arr1_fix(:)
arr1_fix_b = Arr1_fix(nothing )
@test eltype(arr1_fix) === Float64
@test length(arr1_fix) === 10
@test lbnd(arr1_fix, 1) === 1
@test ubnd(arr1_fix, 1) === 10
@test size(arr1_fix, 1) === 10

for i in 1:10
    arr1_fix[i] = 42+i
end
for i in 1:10
    @test arr1_fix[i] === 42.0+i
end
@test string(arr1_fix) ==
    "[43.0 44.0 45.0 46.0 47.0 48.0 49.0 50.0 51.0 52.0 ]\n"

Arr1_lb = FlexArray(1){Float64}
@test eltype(Arr1_lb) === Float64
@test lbnd(Arr1_fix, 1) === 1
arr1_lb = Arr1_lb(10)
@test eltype(arr1_lb) === Float64
@test length(arr1_lb) === 10
@test lbnd(arr1_lb, 1) === 1
@test ubnd(arr1_lb, 1) === 10
@test size(arr1_lb, 1) === 10

Arr1_gen = FlexArray(:){Float64}
@test eltype(Arr1_gen) === Float64
arr1_gen = Arr1_gen(1:10)
@test eltype(arr1_gen) === Float64
@test length(arr1_gen) === 10
@test lbnd(arr1_gen, 1) === 1
@test ubnd(arr1_gen, 1) === 10
@test size(arr1_gen, 1) === 10

# 2D

na = nothing
for bnds1 in [(1,10), (na,10), (1,na), (na,na)],
    bnds2 in [(0,11), (na,11), (0,na), (na,na)]

    Arr2 = FlexArray(bnds1, bnds2){Float64}
    @test eltype(Arr2) === Float64
    if bnds1[1]!==na && bnds1[2]!==na && bnds2[1]!==na && bnds2[2]!==na
        @test length(Arr2) === 120
    end
    bnds1[1]!==na && @test lbnd(Arr2, 1) === 1
    bnds2[1]!==na && @test lbnd(Arr2, 2) === 0
    bnds1[2]!==na && @test ubnd(Arr2, 1) === 10
    bnds2[2]!==na && @test ubnd(Arr2, 2) === 11
    bnds1[1]!==na && bnds1[2]!==na && @test size(Arr2, 1) === 10
    bnds2[1]!==na && bnds2[2]!==na && @test size(Arr2, 2) === 12
    sizes = []
    bnds1[1]===na && bnds1[2]===na && push!(sizes, 1:10)
    bnds1[1]===na && bnds1[2]!==na && push!(sizes, (1,))
    bnds1[1]!==na && bnds1[2]===na && push!(sizes, 10)
    bnds1[1]!==na && bnds1[2]!==na && push!(sizes, :)
    bnds2[1]===na && bnds2[2]===na && push!(sizes, 0:11)
    bnds2[1]===na && bnds2[2]!==na && push!(sizes, (0,))
    bnds2[1]!==na && bnds2[2]===na && push!(sizes, 11)
    bnds2[1]!==na && bnds2[2]!==na && push!(sizes, :)
    arr2 = Arr2(sizes...)
    @test eltype(arr2) === Float64
    @test length(arr2) === 120
    @test lbnd(arr2, 1) === 1
    @test lbnd(arr2, 2) === 0
    @test ubnd(arr2, 1) === 10
    @test ubnd(arr2, 2) === 11
    @test size(arr2, 1) === 10
    @test size(arr2, 2) === 12
end

Arr3 = FlexArray(0:3, 0, 0){Int}
arr3 = Arr3(:,4,5)

# Test the examples given in the README

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

# The trivial 0d array, always holding one scalar value:
FlexArray(){Int}

# Cartesian iteration
for i in eachindex(a3)
    a3[i] = 1
end
@test a3[0,0,0] == 1
@test a3[1,2,3] == 1
@test a3[9,9,9] == 1
s = zero(eltype(a3))
for k in 0:9, j in 0:9, i in 0:9
    s += a3[i,j,k]
end
@test s == 1000

# Linear iteration
for i in 1:length(a3)
    a3[LinearIndex(i)] = 2
end
@test a3[0,0,0] == 2
@test a3[1,2,3] == 2
@test a3[9,9,9] == 2
s = zero(eltype(a3))
for k in 0:9, j in 0:9, i in 0:9
    s += a3[i,j,k]
end
@test s == 2000

@test collect(arr0) == [42]
@test collect(arr1_fix) == [43, 44, 45, 46, 47, 48, 49, 50, 51, 52]
@test length(collect(arr1_gen)) == 10
@test length(collect(arr3)) == length(arr3)
@test length(collect(a4)) == length(a4)

# 10D arrays
typealias BinArr10d FlexArray(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
a10b = BinArr10d{Complex128}(:,:,:,:,:,:,:,:,:,:)
@test length(a10b) == 2^10
for i in eachindex(a10b)
    a10b[i] = 1
end
@test sum(a10b) == 2^10

typealias Arr10d FlexArray(:,:,:,:,:,:,:,:,:,:)
a10 = Arr10d{Complex128}(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
@test length(a10) == 2^10
for i in eachindex(a10b)
    a10[i] = 1
end
@test sum(a10) == 2^10

# Element-wise iteration
s = zero(eltype(a3))
for a in a3
    s += a
end
@test s == 2000

# Bounds checking
@test_throws BoundsError a3[-1,0,0]
@test_throws BoundsError a3[0,-1,0]
@test_throws BoundsError a3[0,0,-1]
@test_throws BoundsError a3[10,0,0]
@test_throws BoundsError a3[0,10,0]
@test_throws BoundsError a3[0,0,10]

# Immutable arrays

typealias I0 ImmutableArray()
i0 = I0{Int}()
@test sizeof(i0) == sizeof(Int)*2^0
for i in eachindex(i0)
    i0 = setindex(i0, 1, i)
end
@test sum(i0) == 2^0

typealias I1 ImmutableArray(0:1)
i1 = I1{Int}(:)
@test sizeof(i1) == sizeof(Int)*2^1
for i in eachindex(i1)
    i1 = setindex(i1, 1, i)
end
@test sum(i1) == 2^1

typealias I2 ImmutableArray(0:1, 0:1)
i2 = I2{Int}(:,:)
@test sizeof(i2) == sizeof(Int)*2^2
for i in eachindex(i2)
    i2 = setindex(i2, 1, i)
end
@test sum(i2) == 2^2

typealias I10 ImmutableArray(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
i10 = I10{Int}(:,:,:,:,:,:,:,:,:,:)
@test sizeof(i10) == sizeof(Int)*2^10

@test collect(i0) == [1]
@test collect(i1) == [1, 1]
@test collect(i2) == [1, 1, 1, 1]
@test length(collect(i10)) == length(i10)

# Functor, Applicative Functor

r2 = map(x->1, a2)
@test r2[1,1] === r2[10,10] === 1
q2 = map((x,y) -> y+1.0, a2, r2)
@test q2[1,1] === q2[10,10] === 2.0

ri2 = map(x->1, i2)
@test ri2[0,0] === ri2[1,1] === 1
qi2 = map((x,y) -> y+1.0, i2, ri2)
@test qi2[0,0] === qi2[1,1] === 2.0

# Foldable

@test reduce(+, 0, r2) === 100
@test mapreduce((x,y) -> x+y, +, 0.0, r2, q2) === 300.0

@test reduce(+, 0, ri2) === 4
@test mapreduce((x,y) -> x+y, +, 0.0, ri2, qi2) === 12.0

# A real-world example

typealias Box FlexArray(0:9, 0){Int}

function init()
    b = Box(:, 19)
    @inbounds for j in 0:ubnd(b,2)
        @simd for i in 0:9
            b[i,j] = 9-i
        end
    end
    b
end

function process(oldb::Box)
    b = Box(:, ubnd(oldb,2))
    @inbounds for j in 0:ubnd(b,2)
        @simd for i in 0:9
            b[i,j] = oldb[oldb[i,j],j]
        end
    end
    b
end

b = init()
b2 = process(b)
@test [b2[i,12] for i in 0:9] == collect(0:9)
