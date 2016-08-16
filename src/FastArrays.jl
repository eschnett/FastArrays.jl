module FastArrays

export AbstractFastArray
export AbstractImmutableFastArray, AbstractMutableFastArray
abstract AbstractFastArray{T, N} <: DenseArray{T, N}
abstract AbstractImmutableFastArray{T, N} <: AbstractFastArray{T, N}
abstract AbstractMutableFastArray{T, N} <: AbstractFastArray{T, N}



typealias BndSpec Union{UnitRange{Int}, Int, Colon, NTuple{2, Union{Int, Void}}}

immutable MutableFastArrayImpl{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff,
                               T} <:
                                   AbstractMutableFastArray{T, N}
    dynbnds::DynBnds            # bounds (lower, upper)
    dynstrs::DynStrs            # strides
    dynlen::DynLen              # length
    dynoff::DynOff              # offset
    data::Vector{T}

    function MutableFastArrayImpl(dynbnds::DynBnds)
        check_invariant(Val{N}, Val{FixedBnds}, DynBnds, DynStrs, DynLen,
                        DynOff)
        dynbnds, dynstrs, dynlen, dynoff, length =
            calc_details(Val{FixedBnds}, DynBnds, DynStrs, DynLen, DynOff,
                         dynbnds)
        data = Vector{T}(length)
        new(dynbnds, dynstrs, dynlen, dynoff, data)
    end

    @generated function MutableFastArrayImpl(bnds::BndSpec...)
        @assert nfields(bnds) == N
        dynbnds = []
        for i in 1:N
            bnd = getfield(bnds, i)
            if bnd === UnitRange{Int}
                push!(dynbnds, :((bnds[$i].start, bnds[$i].stop)))
            elseif bnd === Int
                push!(dynbnds, :((nothing, bnds[$i])))
            elseif bnd === Colon
                push!(dynbnds, :((nothing, nothing)))
            elseif bnd <: NTuple{2, Union{Int, Void}}
                push!(dynbnds, :((bnds[$i][1], bnds[$i][2])))
            else
                @assert false
            end
        end
        dynbnds = :(tuple($(dynbnds...)))
        quote
            MutableFastArrayImpl{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff,
                                 T
                                 }($dynbnds)
        end
    end
end

@generated function check_invariant{N, FixedBnds, DynBnds, DynStrs, DynLen,
                                    DynOff
                                    }(::Type{Val{N}}, ::Type{Val{FixedBnds}},
                                      ::Type{DynBnds}, ::Type{DynStrs},
                                      ::Type{DynLen}, ::Type{DynOff})
    inv = invariant(N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff)
    quote
        @assert $inv
    end
end

function invariant(N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff)
    isa(N, Int) || (error(); return false)
    N >= 0 || (error(); return false)
    isa(FixedBnds, NTuple{N, NTuple{2, Union{Int, Void}}}) ||
        (error(); return false)
    DynBnds <: Tuple || (error(); return false)
    nfields(DynBnds) == N || (error(); return false)
    for i in 1:N
        DynBnd = fieldtype(DynBnds, i)
        DynBnd <: Tuple || (error(); return false)
        nfields(DynBnd) == 2 || (error(); return false)
        for f in 1:2
            if FixedBnds[i][f] !== nothing
                fieldtype(DynBnd, f) === Void || (error(); return false)
            else
                fieldtype(DynBnd, f) === Int || (error(); return false)
            end
        end
    end
    for i in 1:N
        if FixedBnds[i][1] !== nothing && FixedBnds[i][2] !== nothing
            FixedBnds[i][2] >= FixedBnds[i][1] - 1 || (error(); return false)
        end
    end
    DynStrs <: Tuple || (error(); return false)
    hasfixedoffset = true
    hasfixedlength = true
    for i in 1:N
        hasfixedoffset = hasfixedlength && FixedBnds[i][1] !== nothing
        if hasfixedlength
            fieldtype(DynStrs, i) === Void || (error(); return false)
        else
            fieldtype(DynStrs, i) === Int || (error(); return false)
        end
        hasfixedlength &=
            FixedBnds[i][1] !== nothing && FixedBnds[i][2] !== nothing
    end
    if hasfixedlength
        DynLen === Void || (error(); return false)
    else
        DynLen === Int || (error(); return false)
    end
    if hasfixedoffset
        DynOff === Void || (error(); return false)
    else
        DynOff === Int || (error(); return false)
    end
    return true
end

@generated function calc_details{FixedBnds, DynBnds, DynStrs, DynLen, DynOff
                                 }(::Type{Val{FixedBnds}}, ::Type{DynBnds},
                                   ::Type{DynStrs}, ::Type{DynLen},
                                   ::Type{DynOff}, dynbnds)
    N = length(FixedBnds)
    lbndexprs = []
    for i in 1:N
        if FixedBnds[i][1] !== nothing
            push!(lbndexprs, FixedBnds[i][1])
        else
            push!(lbndexprs, :(dynbnds[$i][1]))
        end
    end
    ubndexprs = []
    for i in 1:N
        if FixedBnds[i][2] !== nothing
            push!(ubndexprs, FixedBnds[i][2])
        else
            push!(ubndexprs, :(dynbnds[$i][2]))
        end
    end
    quote
        lbnds = tuple($(lbndexprs...))
        ubnds = tuple($(ubndexprs...))
        ubnds = ntuple(i->max(lbnds[i] - 1, ubnds[i]), $N)
        length = 1
        strs = ntuple($N) do i
            oldlength = length
            length *= ubnds[i] - lbnds[i] + 1
            oldlength
        end
        offset = +(1, $((:(- lbnds[$i] * strs[$i]) for i in 1:N)...))
        dynlbnds = tuple($((fieldtype(fieldtype(DynBnds, i), 1) === Int ?
                            :(lbnds[$i]) : :nothing
                            for i in 1:N)...))
        dynubnds = tuple($((fieldtype(fieldtype(DynBnds, i), 2) === Int ?
                            :(ubnds[$i]) : :nothing
                            for i in 1:N)...))
        dynbnds = tuple($((:(dynlbnds[$i], dynubnds[$i]) for i in 1:N)...))
        dynstrs = tuple($((fieldtype(DynStrs, i) === Int ?
                           :(strs[$i]) : :nothing
                           for i in 1:N)...))
        dynlen = $(DynLen === Int ? :length : :nothing)
        dynoff = $(DynOff === Int ? :offset : :nothing)
        dynbnds, dynstrs, dynlen, dynoff, length
    end
end

@generated function lbnd{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff, D
                         }(a::MutableFastArrayImpl{N, FixedBnds, DynBnds,
                                                   DynStrs, DynLen, DynOff},
                           ::Type{Val{D}})
    if fieldtype(fieldtype(DynBnds, D), 1) === Void
        FixedBnds[D][1]
    else
        :(a.dynbnds[D][1])
    end
end

@generated function ubnd{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff, D
                         }(a::MutableFastArrayImpl{N, FixedBnds, DynBnds,
                                                   DynStrs, DynLen, DynOff},
                           ::Type{Val{D}})
    if fieldtype(fieldtype(DynBnds, D), 2) === Void
        FixedBnds[D][2]
    else
        :(a.dynbnds[D][2])
    end
end

@generated function str{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff, D
                        }(a::MutableFastArrayImpl{N, FixedBnds, DynBnds,
                                                  DynStrs, DynLen, DynOff},
                          ::Type{Val{D}})
    if fieldtype(DynStrs, D) === Void
        *(1, (FixedBnds[i][2] - FixedBnds[i][1] + 1 for i in 1:D-1)...)
    else
        :(a.dynstrs[D])
    end
end

@generated function len{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff
                        }(a::MutableFastArrayImpl{N, FixedBnds, DynBnds,
                                                  DynStrs, DynLen, DynOff})
    if DynLen === Void
        *(1, (FixedBnds[i][2] - FixedBnds[i][1] + 1 for i in 1:N)...)
    else
        :(a.dynlen)
    end
end

@generated function off{N, FixedBnds, DynBnds, DynStrs, DynLen, DynOff
                        }(a::MutableFastArrayImpl{N, FixedBnds, DynBnds,
                                                  DynStrs, DynLen, DynOff})
    if DynOff === Void
        strs = Int[1]
        for i in 2:N
            push!(strs, strs[i-1] * (FixedBnds[i-1][2] - FixedBnds[i-1][1] + 1))
        end
        +(1, (- strs[i] * FixedBnds[i][1] for i in 1:N)...)
    else
        :(a.dynoff)
    end
end



export FastArray
@generated function FastArray{N}(bnds::NTuple{N, BndSpec})
    isfixed = NTuple{2, Bool}[]
    fixedbnds = []
    for i in 1:N
        bnd = fieldtype(bnds, i)
        if bnd === UnitRange{Int}
            push!(isfixed, (true, true))
            push!(fixedbnds,
                  :((bnds[$i].start, max(bnds[$i].start - 1, bnds[$i].stop))))
        elseif bnd === Int
            push!(isfixed, (true, false))
            push!(fixedbnds, :((bnds[$i], nothing)))
        elseif bnd === Colon
            push!(isfixed, (false, false))
            push!(fixedbnds, :((nothing, nothing)))
        elseif bnd === Tuple{Int, Int}
            push!(isfixed, (true, true))
            push!(fixedbnds,
                  :((bnds[$i][1], max(bnds[$i][1] - 1, bnds[$i][2]))))
        elseif bnd === Tuple{Int, Void}
            push!(isfixed, (true, false))
            push!(fixedbnds, :((bnds[$i][1], nothing)))
        elseif bnd === Tuple{Void, Int}
            push!(isfixed, (false, true))
            push!(fixedbnds, :((nothing, bnds[$i][2])))
        elseif bnd === Tuple{Void, Void}
            push!(isfixed, (false, false))
            push!(fixedbnds, :((nothing, nothing)))
        else
            @assert false
        end
    end
    dynbnds = []
    for i in 1:N
        lbnd = isfixed[i][1] ? :Void : :Int
        ubnd = isfixed[i][2] ? :Void : :Int
        push!(dynbnds, :(Tuple{$lbnd, $ubnd}))
    end
    dynstrs = []
    hasfixedoffset = true
    hasfixedlength = true
    for i in 1:N
        hasfixedoffset = hasfixedlength && isfixed[i][1]
        push!(dynstrs, hasfixedlength ? :Void : :Int)
        hasfixedlength &= isfixed[i][1] && isfixed[i][2]
    end
    dynlen = hasfixedlength ? :Void : :Int
    dynoff = hasfixedoffset ? :Void : :Int
    fixedbnds = :(tuple($(fixedbnds...)))
    dynbnds = :(Tuple{$(dynbnds...)})
    dynstrs = :(Tuple{$(dynstrs...)})
    quote
        MutableFastArrayImpl{$N, $fixedbnds, $dynbnds, $dynstrs, $dynlen,
                             $dynoff}
    end
end

@generated function FastArray(bnds::BndSpec...)
    quote
        FastArray(bnds)
    end
end



import Base: indices
@generated function indices{N}(a::MutableFastArrayImpl{N})
    quote
        $(Expr(:meta, :inline))
        # TODO: Return Base.OneTo for respective fixed lower bounds
        tuple($((:(lbnd(a, Val{$i}) : ubnd(a, Val{$i})) for i in 1:N)...))
    end
end

import Base: size
@generated function size{N}(a::MutableFastArrayImpl{N})
    quote
        inds = indices(a)
        tuple($((:(inds[$i].stop - inds[$i].start + 1) for i in 1:N)...))
    end
end
# size(a, d...) is provided by Base

import Base: strides
@generated function strides{N}(a::MutableFastArrayImpl{N})
    quote
        tuple($((:(str(a, Val{$i})) for i in 1:N)...))
    end
end
import Base: stride
function stride{N}(a::MutableFastArrayImpl{N}, d::Int)
    strides(a)[d]
end

import Base: length
function length(a::MutableFastArrayImpl)
    len(a)
end



import Base: linearindexing
linearindexing(::AbstractFastArray) = Base.LinearFast()

export LinearIndex
immutable LinearIndex
    ind::Int
end

export linearindex
function linearindex{N}(a::MutableFastArrayImpl{N}, idx::CartesianIndex{N})
    Base.@_propagate_inbounds_meta()
    @boundscheck checkbounds(a, idx)
    str = strides(a)
    lind = off(a)
    for i in 1:N
        lind += str[i] * idx[i]
    end
    LinearIndex(lind)
end
function linearindex{N}(a::MutableFastArrayImpl{N}, idx::NTuple{N, Int})
    Base.@_propagate_inbounds_meta()
    linearindex(a, CartesianIndex(idx))
end

import Base: getindex
function getindex(a::MutableFastArrayImpl, idx::LinearIndex)
    Base.@_propagate_inbounds_meta()
    getindex(a.data, idx.ind)
end
function getindex(a::MutableFastArrayImpl, idx::Union{Tuple, CartesianIndex})
    throw(BoundsError(a, idx))
end
function getindex{N}(a::MutableFastArrayImpl{N},
                     idx::Union{NTuple{N, Int}, CartesianIndex{N}})
    Base.@_propagate_inbounds_meta()
    lidx = linearindex(a, idx)
    @inbounds val = getindex(a, lidx)
    val
end
function getindex{N}(a::MutableFastArrayImpl{N}, ids::Int...)
    Base.@_propagate_inbounds_meta()
    getindex(a, ids)
end

import Base: setindex!
function setindex!(a::MutableFastArrayImpl, val, idx::LinearIndex)
    Base.@_propagate_inbounds_meta()
    setindex!(a.data, val, idx.ind)
end
function setindex!(a::MutableFastArrayImpl, val,
                   idx::Union{Tuple, CartesianIndex})
    throw(BoundsError(a, idx))
end
function setindex!{N}(a::MutableFastArrayImpl{N},
                      val, idx::Union{NTuple{N, Int}, CartesianIndex{N}})
    Base.@_propagate_inbounds_meta()
    lidx = linearindex(a, idx)
    @inbounds val = setindex!(a, val, lidx)
    val
end
function setindex!{N}(a::MutableFastArrayImpl{N}, val, ids::Int...)
    Base.@_propagate_inbounds_meta()
    setindex!(a, val, ids)
end



import Base: start, done, next
function start(a::MutableFastArrayImpl)
    inds = indices(a)
    linearindex(a, ntuple(i -> inds[i].start, length(inds)))
end
function done(a::MutableFastArrayImpl, state)
    inds = indices(a)
    state.ind > linearindex(a, ntuple(i -> inds[i].stop, length(inds))).ind
end
function next(a::MutableFastArrayImpl, state)
    a[state], LinearIndex(state.ind + 1)
end

import Base: vec
function vec(a::MutableFastArrayImpl)
    copy(a.data)
end

import Base: collect
function collect(a::MutableFastArrayImpl)
    reshape(vec(a), size(a))
end

end
