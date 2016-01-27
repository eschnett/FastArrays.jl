using FlexibleArrays
using Base.Test

# 0D

Arr0 = FlexArray(Val{()}){Float64}
Arr0b = FlexArray(Val{()}){Int}
@test eltype(Arr0) === Float64
@test length(Arr0) === 1

arr0 = Arr0()
@test eltype(arr0) === Float64
@test length(arr0) === 1

arr0[] = 42
@test arr0[] === 42.0

# 1D

Arr1_fix = FlexArray(Val{((Nullable{Int}(1), Nullable{Int}(10)),)}){Float64}
@test eltype(Arr1_fix) === Float64
@test length(Arr1_fix) === 10
@test lbnd(Arr1_fix, 1) === 1
@test ubnd(Arr1_fix, 1) === 10
@test size(Arr1_fix, 1) === 10
arr1_fix = Arr1_fix()
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

Arr1_lb = FlexArray(Val{((Nullable{Int}(1), Nullable{Int}()),)}){Float64}
@test eltype(Arr1_lb) === Float64
@test lbnd(Arr1_fix, 1) === 1
arr1_lb = Arr1_lb(10)
@test eltype(arr1_lb) === Float64
@test length(arr1_lb) === 10
@test lbnd(arr1_lb, 1) === 1
@test ubnd(arr1_lb, 1) === 10
@test size(arr1_lb, 1) === 10

Arr1_gen = FlexArray(Val{((Nullable{Int}(), Nullable{Int}()),)}){Float64}
@test eltype(Arr1_gen) === Float64
arr1_gen = Arr1_gen(1,10)
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
    bnds1[1] === (:) && push!(sizes, 1)
    bnds1[2] === (:) && push!(sizes, 10)
    bnds2[1] === (:) && push!(sizes, 0)
    bnds2[2] === (:) && push!(sizes, 11)
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

Arr3 = FlexArray(Val{(ds((0,3)), ds((0,:)), ds((0,:)))}){Int}
arr3 = Arr3(4,5)
@code_native arr3[1,2,3]
