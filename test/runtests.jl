using FastArrays
using Base.Test

typealias tycon0 FastArray()
FastArray(())
typealias ty0 tycon0{Float64}
a0_f64 = ty0()

@test ndims(a0_f64) === 0
@test indices(a0_f64) === ()
@test size(a0_f64) === ()
@test length(a0_f64) === 1
@test strides(a0_f64) === ()

a0_f64[] = 1
@test a0_f64[] === 1.0
@test_throws BoundsError a0_f64[1] = 2.0
@test_throws BoundsError a0_f64[1,2] = 2.0
@test_throws BoundsError a0_f64[1]
@test_throws BoundsError a0_f64[1,2]



typealias tycon1ff FastArray(0:3)
typealias tycon1ff FastArray(0:3)
typealias ty1ff tycon1ff{Float64}
a1ff_f64 = ty1ff(:)

@test ndims(a1ff_f64) === 1
@test indices(a1ff_f64) === (0:3,)
@test size(a1ff_f64) === (4,)
@test length(a1ff_f64) === 4
@test strides(a1ff_f64) === (1,)

a1ff_f64[0] = 1
@test a1ff_f64[0] === 1.0
a1ff_f64[3] = 2
@test a1ff_f64[3] === 2.0
@test_throws BoundsError a1ff_f64[] = 3
@test_throws BoundsError a1ff_f64[]
@test_throws BoundsError a1ff_f64[-1] = 3
@test_throws BoundsError a1ff_f64[-1]
@test_throws BoundsError a1ff_f64[4] = 3
@test_throws BoundsError a1ff_f64[4]
@test_throws BoundsError a1ff_f64[1,2] = 3
@test_throws BoundsError a1ff_f64[1,2]

typealias tycon1fd FastArray(0)
typealias ty1fd tycon1fd{Float64}
@test FastArray((0, nothing)) == tycon1fd
a1fd_f64 = ty1fd(3)

@test ndims(a1fd_f64) === 1
@test indices(a1fd_f64) === (0:3,)
@test size(a1fd_f64) === (4,)
@test length(a1fd_f64) === 4
@test strides(a1fd_f64) === (1,)

a1fd_f64[0] = 1
@test a1fd_f64[0] === 1.0
a1fd_f64[3] = 2
@test a1fd_f64[3] === 2.0
@test_throws BoundsError a1fd_f64[] = 3
@test_throws BoundsError a1fd_f64[]
@test_throws BoundsError a1fd_f64[-1] = 3
@test_throws BoundsError a1fd_f64[-1]
@test_throws BoundsError a1fd_f64[4] = 3
@test_throws BoundsError a1fd_f64[4]
@test_throws BoundsError a1fd_f64[1,2] = 3
@test_throws BoundsError a1fd_f64[1,2]

typealias tycon1df FastArray((nothing, 3))
typealias ty1df tycon1df{Float64}
a1df_f64 = ty1df((0, nothing))

@test ndims(a1df_f64) === 1
@test indices(a1df_f64) === (0:3,)
@test size(a1df_f64) === (4,)
@test length(a1df_f64) === 4
@test strides(a1df_f64) === (1,)

a1df_f64[0] = 1
@test a1df_f64[0] === 1.0
a1df_f64[3] = 2
@test a1df_f64[3] === 2.0
@test_throws BoundsError a1df_f64[] = 3
@test_throws BoundsError a1df_f64[]
@test_throws BoundsError a1df_f64[-1] = 3
@test_throws BoundsError a1df_f64[-1]
@test_throws BoundsError a1df_f64[4] = 3
@test_throws BoundsError a1df_f64[4]
@test_throws BoundsError a1df_f64[1,2] = 3
@test_throws BoundsError a1df_f64[1,2]

typealias tycon1dd FastArray(:)
typealias ty1dd tycon1dd{Float64}
@test FastArray((nothing, nothing)) == tycon1dd
a1dd_f64 = ty1dd(0:3)

@test ndims(a1dd_f64) === 1
@test indices(a1dd_f64) === (0:3,)
@test size(a1dd_f64) === (4,)
@test length(a1dd_f64) === 4
@test strides(a1dd_f64) === (1,)

a1dd_f64[0] = 1
@test a1dd_f64[0] === 1.0
a1dd_f64[3] = 2
@test a1dd_f64[3] === 2.0
@test_throws BoundsError a1dd_f64[] = 3
@test_throws BoundsError a1dd_f64[]
@test_throws BoundsError a1dd_f64[-1] = 3
@test_throws BoundsError a1dd_f64[-1]
@test_throws BoundsError a1dd_f64[4] = 3
@test_throws BoundsError a1dd_f64[4]
@test_throws BoundsError a1dd_f64[1,2] = 3
@test_throws BoundsError a1dd_f64[1,2]



typealias tycon2ffff FastArray(0:3, 0:4)
typealias ty2ffff tycon2ffff{Float64}
@test FastArray((0, 3), (0, 4)) == tycon2ffff
a2ffff_f64 = ty2ffff(:, :)

@test ndims(a2ffff_f64) === 2
@test indices(a2ffff_f64) === (0:3, 0:4)
@test size(a2ffff_f64) === (4, 5)
@test length(a2ffff_f64) === 4 * 5
@test strides(a2ffff_f64) === (1, 4)
@test stride(a2ffff_f64, 1) === 1
@test stride(a2ffff_f64, 2) === 4

a2ffff_f64[0,0] = 1
@test a2ffff_f64[0,0] === 1.0
a2ffff_f64[3,4] = 2
@test a2ffff_f64[3,4] === 2.0
@test_throws BoundsError a2ffff_f64[] = 3
@test_throws BoundsError a2ffff_f64[]
@test_throws BoundsError a2ffff_f64[1] = 3
@test_throws BoundsError a2ffff_f64[1]
@test_throws BoundsError a2ffff_f64[-1,2] = 3
@test_throws BoundsError a2ffff_f64[-1,2]
@test_throws BoundsError a2ffff_f64[4,2] = 3
@test_throws BoundsError a2ffff_f64[4,2]
@test_throws BoundsError a2ffff_f64[0,-1] = 3
@test_throws BoundsError a2ffff_f64[0,-1]
@test_throws BoundsError a2ffff_f64[0,5] = 3
@test_throws BoundsError a2ffff_f64[0,5]
@test_throws BoundsError a2ffff_f64[1,2,3] = 3
@test_throws BoundsError a2ffff_f64[1,2,3]

typealias tycon2fffd FastArray(0:3, 0)
typealias ty2fffd tycon2fffd{Float64}
a2fffd_f64 = ty2fffd(:, 4)

@test ndims(a2fffd_f64) === 2
@test indices(a2fffd_f64) === (0:3, 0:4)
@test size(a2fffd_f64) === (4, 5)
@test length(a2fffd_f64) === 4 * 5
@test strides(a2fffd_f64) === (1, 4)

a2fffd_f64[0,0] = 1
@test a2fffd_f64[0,0] === 1.0
a2fffd_f64[3,4] = 2
@test a2fffd_f64[3,4] === 2.0
@test_throws BoundsError a2fffd_f64[] = 3
@test_throws BoundsError a2fffd_f64[]
@test_throws BoundsError a2fffd_f64[1] = 3
@test_throws BoundsError a2fffd_f64[1]
@test_throws BoundsError a2fffd_f64[-1,2] = 3
@test_throws BoundsError a2fffd_f64[-1,2]
@test_throws BoundsError a2fffd_f64[4,2] = 3
@test_throws BoundsError a2fffd_f64[4,2]
@test_throws BoundsError a2fffd_f64[0,-1] = 3
@test_throws BoundsError a2fffd_f64[0,-1]
@test_throws BoundsError a2fffd_f64[0,5] = 3
@test_throws BoundsError a2fffd_f64[0,5]
@test_throws BoundsError a2fffd_f64[1,2,3] = 3
@test_throws BoundsError a2fffd_f64[1,2,3]



typealias tycon4fffffdfd FastArray(0:3, 0:4, 1, 1)
typealias ty4fffffdfd tycon4fffffdfd{Float64}
a4fffffdfd_f64 = ty4fffffdfd(:, :, 5, 6)

@test ndims(a4fffffdfd_f64) === 4
@test indices(a4fffffdfd_f64) === (0:3, 0:4, 1:5, 1:6)
@test size(a4fffffdfd_f64) === (4, 5, 5, 6)
@test length(a4fffffdfd_f64) === 4 * 5 * 5 * 6
@test strides(a4fffffdfd_f64) === (1, 4, 4 * 5, 4 * 5 * 5)

a4fffffdfd_f64[0,0,1,1] = 1
@test a4fffffdfd_f64[0,0,1,1] === 1.0
a4fffffdfd_f64[3,4,5,6] = 2
@test a4fffffdfd_f64[3,4,5,6] === 2.0
@test_throws BoundsError a4fffffdfd_f64[] = 3
@test_throws BoundsError a4fffffdfd_f64[]
@test_throws BoundsError a4fffffdfd_f64[1] = 3
@test_throws BoundsError a4fffffdfd_f64[1]
@test_throws BoundsError a4fffffdfd_f64[1,2] = 3
@test_throws BoundsError a4fffffdfd_f64[1,2]
@test_throws BoundsError a4fffffdfd_f64[1,2,3] = 3
@test_throws BoundsError a4fffffdfd_f64[1,2,3]
@test_throws BoundsError a4fffffdfd_f64[1,2,0,0] = 3
@test_throws BoundsError a4fffffdfd_f64[1,2,0,0]
@test_throws BoundsError a4fffffdfd_f64[1,2,3,4,5] = 3
@test_throws BoundsError a4fffffdfd_f64[1,2,3,4,5]



typealias tycon10 FastArray(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
typealias ty10 tycon10{Bool}
a10_b = ty10(:,:,:,:,:,:,:,:,:,:)

@test ndims(a10_b) === 10
@test indices(a10_b) === ntuple(i->0:1, 10)
@test size(a10_b) === ntuple(i->2, 10)
@test length(a10_b) === 2^10
@test strides(a10_b) === ((2^i for i in 0:10-1)...)

a10_b[0,0,0,0,0,0,0,0,0,0] = true
@test a10_b[0,0,0,0,0,0,0,0,0,0] === true
a10_b[1,1,1,1,1,1,1,1,1,1] = false
@test a10_b[1,1,1,1,1,1,1,1,1,1] === false



@test Base.linearindexing(a2ffff_f64) == Base.LinearFast()

for i in CartesianRange(indices(a0_f64))
    a0_f64[i] = 2 * +(0, i.I...) + 1
end
@test vec(a0_f64) == [1]
@test collect(a0_f64)[] == 1

for i in CartesianRange(indices(a1ff_f64))
    a1ff_f64[i] = 2 * +(0, i.I...) + 1
end
@test vec(a1ff_f64) == [1, 3, 5, 7]
@test collect(a1ff_f64) == [1, 3, 5, 7]

for i in CartesianRange(indices(a2ffff_f64))
    a2ffff_f64[i] = 2 * +(0, i.I...) + 1
end
@test vec(a2ffff_f64) ==
    [1, 3, 5, 7, 3, 5, 7, 9, 5, 7, 9, 11, 7, 9, 11, 13, 9, 11, 13, 15]
@test collect(a2ffff_f64) == [1 3 5 7 9; 3 5 7 9 11; 5 7 9 11 13; 7 9 11 13 15]

v2ffff_f64 = Float64[]
st = start(a2ffff_f64)
while !done(a2ffff_f64, st)
    val, st = next(a2ffff_f64, st)
    push!(v2ffff_f64, val)
end
@test v2ffff_f64 == vec(a2ffff_f64)
