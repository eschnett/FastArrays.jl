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

FlexArray(){Int}()

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

# 1D

Arr1_fix = FlexArray(1:10){Float64}
@test eltype(Arr1_fix) === Float64
@test length(Arr1_fix) === 10
@test lbnd(Arr1_fix, 1) === 1
@test ubnd(Arr1_fix, 1) === 10
@test size(Arr1_fix, 1) === 10
arr1_fix = Arr1_fix(:)
arr1_fix_b = Arr1_fix(nothing, nothing)
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

ds(::Colon) = Nullable{Int}()
ds(i::Integer) = Nullable{Int}(i)
ds(t::Tuple) = map(ds, t)

for bnds1 in [(1,10), (:,10), (1,:), (:,:)],
    bnds2 in [(0,11), (:,11), (0,:), (:,:)]

    ds1 = ds(bnds1)
    ds2 = ds(bnds2)

    Arr2 = FlexArray(Val{(ds1,ds2)}){Float64}
    @test eltype(Arr2) === Float64
    if bnds1[1] !== (:) && bnds1[2] !== (:) && bnds2[1] !== (:) &&
            bnds2[2] !== (:)
        @test length(Arr2) === 120
    end
    bnds1[1] !== (:) && @test lbnd(Arr2, 1) === 1
    bnds2[1] !== (:) && @test lbnd(Arr2, 2) === 0
    bnds1[2] !== (:) && @test ubnd(Arr2, 1) === 10
    bnds2[2] !== (:) && @test ubnd(Arr2, 2) === 11
    bnds1[1] !== (:) && bnds1[2] !== (:) && @test size(Arr2, 1) === 10
    bnds2[1] !== (:) && bnds2[2] !== (:) && @test size(Arr2, 2) === 12
    sizes = []
    push!(sizes, bnds1[1] === (:) ? 1 : nothing)
    push!(sizes, bnds1[2] === (:) ? 10 : nothing)
    push!(sizes, bnds2[1] === (:) ? 0 : nothing)
    push!(sizes, bnds2[2] === (:) ? 11 : nothing)
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
