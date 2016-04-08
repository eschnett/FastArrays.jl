module FlexibleArrays

# using Base.Cartesian

export AbstractFlexArray
export AbstractImmutableFlexArray, AbstractMutableFlexArray
abstract AbstractFlexArray{T,N} <: DenseArray{T,N}
abstract AbstractImmutableFlexArray{T,N} <: AbstractFlexArray{T,N}
abstract AbstractMutableFlexArray{T,N} <: AbstractFlexArray{T,N}

# eltype, ndims are provided by DenseArray

export lbnd, ubnd
import Base: done, eachindex, next, similar, size, start

size{n}(arr::AbstractFlexArray, ::Type{Val{n}}) =
    max(0, ubnd(arr, Val{n}) - lbnd(arr, Val{n}) + 1)
size{T <: AbstractFlexArray, n}(arr::Type{T}, ::Type{Val{n}}) =
    max(0, ubnd(T, Val{n}) - lbnd(T, Val{n}) + 1)

lbnd(arr::AbstractFlexArray, n::Int) = lbnd(arr, Val{n})
ubnd(arr::AbstractFlexArray, n::Int) = ubnd(arr, Val{n})
size(arr::AbstractFlexArray, n::Int) = size(arr, Val{n})

lbnd{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = lbnd(T, Val{n})
ubnd{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = ubnd(T, Val{n})
size{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = size(T, Val{n})

# Note: Use ntuple instead of generated functions once the closure in
# ntuple is efficient
# lbnd{T,N}(arr::AbstractFlexArray{T,N}) = ntuple(n->lbnd(arr, Val{n}), N)
@generated function lbnd(arr::AbstractFlexArray)
    :(tuple($([:(lbnd(arr, Val{$n})) for n in 1:ndims(arr)]...)))
end
# The Base.Cartesian macros expect literals, not parameters
# function lbnd{T,N}(arr::AbstractFlexArray{T,N})
#     @ntuple N n->lbnd(arr, Val{n})
# end
@generated function ubnd(arr::AbstractFlexArray)
    :(tuple($([:(ubnd(arr, Val{$n})) for n in 1:ndims(arr)]...)))
end
@generated function size(arr::AbstractFlexArray)
    :(tuple($([:(size(arr, Val{$n})) for n in 1:ndims(arr)]...)))
end

@generated function lbnd{T <: AbstractFlexArray}(::Type{T})
    :(tuple($([:(lbnd(T, Val{$n})) for n in 1:ndims(T)]...)))
end
@generated function ubnd{T <: AbstractFlexArray}(::Type{T})
    :(tuple($([:(ubnd(T, Val{$n})) for n in 1:ndims(T)]...)))
end
@generated function size{T <: AbstractFlexArray}(::Type{T})
    :(tuple($([:(size(T, Val{$n})) for n in 1:ndims(T)]...)))
end

# TODO: beginof, endof

export LinearIndex
immutable LinearIndex
    i::Int
end

eachindex(arr::AbstractFlexArray) =
    CartesianRange(CartesianIndex(lbnd(arr)), CartesianIndex(ubnd(arr)))

start(arr::AbstractFlexArray) = start(eachindex(arr))
done(arr::AbstractFlexArray, i) = done(eachindex(arr), i)
function next(arr::AbstractFlexArray, i)
    ind, inew = next(eachindex(arr), i)
    arr[ind], inew
end

similar{T,N}(arr::AbstractFlexArray{T,N}, dims::NTuple{N,Int}) =
    similar(arr, T, dims)



import Base: show
@generated function show(io::IO, arr::AbstractFlexArray)
    inds = [symbol(:i,n) for n in 1:ndims(arr)]
    stmt = :(print(io, arr[$(inds...)], " "))
    for n in ndims(arr):-1:1
        stmt = quote
            print(io, "[")
            for $(symbol(:i,n)) in lbnd(arr,$n):ubnd(arr,$n)
                $stmt
            end
            println(io, "]")
        end
    end
    stmt
end
# function show{T,N}(io::IO, arr::AbstractFlexArray{T,N})
#     bnds(n) = lbnd(arr,n):ubnd(arr,n)
#     pre(n) = print(io, "[")
#     post(n) = print(io, "[")
#     body(n) =
#     @nloops N i bnds pre post begin
#         print(io, (@nref N arr i), " ")
#     end
# end

# Base.print_matrix assumes that each dimension has a range 1:size.
# Obviously, this is not going to work, so we replace this output
# function.
if VERSION < v"0.5.0-"
    # Julia v0.4
    function Base.print_matrix(io::IO, X::AbstractFlexArray,
                               sz::Tuple{Integer, Integer} = (s = tty_size(); (s[1]-4, s[2])),
                               pre::AbstractString = " ",
                               sep::AbstractString = "  ",
                               post::AbstractString = "",
                               hdots::AbstractString = "  \u2026  ",
                               vdots::AbstractString = "\u22ee",
                               ddots::AbstractString = "  \u22f1  ",
                               hmod::Integer = 5, vmod::Integer = 5)
        print(io, X)
    end
else
    # Julia v0.5
    function Base.print_matrix(io::IO, X::AbstractFlexArray,
                               pre::AbstractString = " ", # pre-matrix string
                               sep::AbstractString = "  ", # separator between elements
                               post::AbstractString = "", # post-matrix string
                               hdots::AbstractString = "  \u2026  ",
                               vdots::AbstractString = "\u22ee",
                               ddots::AbstractString = "  \u22f1  ",
                               hmod::Integer = 5, vmod::Integer = 5)
        print(io, X)
    end
end



@generated function mktuple{N,T}(::Type{Val{N}}, val::T)
    :(tuple($([:val for i in 1:N]...)))
end

@generated function setindex{N,T,I}(t::NTuple{N,T}, val, ::Type{Val{I}})
    :(tuple($([i==I ? :(T(val)) : :(t[$i]) for i in 1:N]...)))
end

@generated function setindex{N,T}(t::NTuple{N,T}, val, i::Int)
    N==0 && return :(NTuple{N,T}())
    quote
        $([:(i==$j && return setindex(t, val, Val{$j})) for j in 1:N]...)
        @assert false
    end
end
setindex{N,T}(t::NTuple{N,T}, val, i::Integer) = setindex(t, val, Int(i))



import Base: call, checkbounds, getindex, length, setindex!
# TODO: implement ind2sub, sub2ind
# TODO: tell Base that we are LinearFast?
export linearindex, setindex



typealias BndSpec NTuple{2, Bool}

@generated function genFlexArray{I,T}(::Type{Val{I}}, ::Type{Val{T}})
    @assert isa(T, Tuple)
    N = length(T)
    @assert isa(T, NTuple{N, BndSpec})

    isimmutable = I
    rank = N
    bndspecs = T

    fixed_lbnd = Bool[bndspecs[n][1] for n in 1:rank]
    fixed_ubnd = Bool[bndspecs[n][2] for n in 1:rank]
    fixed_stride = Vector{Bool}(rank+1)
    fixed_stride[1] = true
    for n in 2:rank+1
        fixed_stride[n] =
            fixed_stride[n-1] && fixed_lbnd[n-1] && fixed_ubnd[n-1]
    end
    fixed_length = fixed_stride[rank+1]
    fixed_offset = rank == 0 || fixed_stride[rank] && fixed_lbnd[rank]

    if isimmutable
        @assert (all(fixed_lbnd) && all(fixed_ubnd) && all(fixed_stride) &&
                 fixed_offset && fixed_length)
    end

    lbnd = [symbol(:lbnd,n) for n in 1:rank]
    ubnd = [symbol(:ubnd,n) for n in 1:rank]
    stride = [symbol(:stride,n) for n in 1:rank+1]

    decls = []

    # Type declaration

    # typename = gensym(:FlexArray)
    typename = let
        names = ["FlexArrayImpl_"]
        push!(names, isimmutable ? "I" : "T")
        for n in 1:rank
            push!(names, string(Int(fixed_lbnd[n])))
            push!(names, string(Int(fixed_ubnd[n])))
        end
        symbol(names...)
    end

    # Sometimes, e.g. when running tests with "coverage=true",
    # generated functions are generated multiple times. Catch this
    # early to avoid defining the implementation type multiple times.
    isdefined(FlexibleArrays, typename) && return typename

    typeparams = []
    for n in 1:rank
        fixed_lbnd[n] && push!(typeparams, lbnd[n])
        fixed_ubnd[n] && push!(typeparams, ubnd[n])
    end
    if isimmutable
        push!(typeparams, :L)   # length
    end
    push!(typeparams, :T)

    # Type name with parameters
    typenameparams = :($typename{$(typeparams...)})

    let
        body = []
        for n in 1:rank
            !fixed_lbnd[n] && push!(body, :($(lbnd[n])::Int))
            !fixed_ubnd[n] && push!(body, :($(ubnd[n])::Int))
            !fixed_stride[n] && push!(body, :($(stride[n])::Int))
        end
        !fixed_length && push!(body, :(length::Int))
        !fixed_offset && push!(body, :(offset::Int))
        if isimmutable
            push!(body, :(data::NTuple{L,T}))
        else
            # TODO: use a more low-level array representation
            push!(body, :(data::Vector{T}))
        end
        let
            args = []
            # Add a dummy Void argument to ensure that this
            # constructor is not called accidentally
            push!(args, :(::Void))
            for n in 1:rank
                !fixed_lbnd[n] && push!(args, :($(lbnd[n])::Int))
                !fixed_ubnd[n] && push!(args, :($(ubnd[n])::Int))
            end
            stmts = []
            push!(stmts, :($(stride[1]) = 1))
            for n in 2:rank+1
                push!(stmts,
                      :($(stride[n]) =
                        $(stride[n-1]) *
                        max(0, $(ubnd[n-1]) - $(lbnd[n-1]) + 1)))
            end
            push!(stmts, :(length = $(stride[rank+1])))
            if !fixed_offset
                push!(stmts,
                      :(offset =
                        +(0, $([:($(stride[n]) * $(lbnd[n]))
                                for n in 1:rank]...))))
            end
            let
                newargs = []
                for n in 1:rank
                    !fixed_lbnd[n] && push!(newargs, lbnd[n])
                    !fixed_ubnd[n] && push!(newargs, ubnd[n])
                    !fixed_stride[n] && push!(newargs, stride[n])
                end
                !fixed_length && push!(newargs, :length)
                !fixed_offset && push!(newargs, :offset)
                if isimmutable
                    push!(newargs, :(mktuple(Val{L}, zero(T))))
                else
                    push!(newargs, :(Vector{T}(length)))
                end
                push!(stmts, :(new($(newargs...))))
            end
            push!(body,
                  :(function $typename($(args...))
                      $(stmts...)
                    end))
        end
        if isimmutable
            push!(body,
                  :(function $typename(::Void, data::NTuple{L,T})
                      new(data)
                    end))
        end
        let
            if isimmutable
                stype = :AbstractImmutableFlexArray
            else
                stype = :AbstractMutableFlexArray
            end
            push!(decls,
                :(type $typenameparams <: $stype{T,$rank}
                    $(body...)
                  end))
        end
    end

    # Outer constructor

    let
        args = []
        callargs = []
        for n in 1:rank
            if fixed_lbnd[n]
                if fixed_ubnd[n]
                    push!(args, :(::Colon))
                else
                    push!(args, :($(ubnd[n])::Int))
                    push!(callargs, ubnd[n])
                end
            else
                if fixed_ubnd[n]
                    push!(args, :($(lbnd[n])::Tuple{Integer}))
                    push!(callargs, :($(lbnd[n])[1]))
                else
                    push!(args, :($(symbol(:bnds,n))::UnitRange{Int}))
                    push!(callargs, :($(symbol(:bnds,n)).start))
                    push!(callargs, :($(symbol(:bnds,n)).stop))
                end
            end
        end
        if VERSION < v"0.5.0-"
            # Julia v0.4
            push!(decls,
                  :(function call{$(typeparams...)}(::Type{$typenameparams},
                                                    $(args...))
                      $typenameparams(nothing, $(callargs...))
                    end))
        else
            # Julia v0.5
            push!(decls,
                  :(function (::Type{$typenameparams}){$(typeparams...)}($(args...))
                      $typenameparams(nothing, $(callargs...))
                    end))
        end
    end

    let
        bndschecks = []
        for n in 1:rank
            if fixed_ubnd[n]
                push!(bndschecks, :(newsize[$n] == size(arr, $n)))
            end
        end
        callargs = []
        for n in 1:rank
            if !fixed_lbnd[n]
                push!(callargs, :(lbnd(arr, $n)))
            end
            if !fixed_ubnd[n]
                push!(callargs, :(newsize[$n] - lbnd(arr, $n) - 1))
            end
        end
        push!(decls,
              :(function similar{$(typeparams...)}(arr::$typenameparams,
                                                   newtype::Type = T,
                                                   newsize::NTuple{$rank,Int} =
                                                       size(arr))
                  if !(&)(true, $(bndschecks...))
                      Base.throw_boundserror(arr, newdims)
                  end
                  $typename{$(typeparams[1:end-1]...), newtype}(nothing,
                                                                $(callargs...))
                end))
    end

    # Lower bound, upper bound, stride, length, offset

    for n in 1:rank
        if fixed_lbnd[n]
            push!(decls,
                  :(function lbnd{$(typeparams...)}(::$typenameparams,
                                                    ::Type{Val{$n}})
                      $(lbnd[n])
                    end))
            push!(decls,
                  :(function lbnd{$(typeparams...)}(::Type{$typenameparams},
                                                    ::Type{Val{$n}})
                      $(lbnd[n])
                    end))
        else
            push!(decls,
                  :(function lbnd{$(typeparams...)}(arr::$typenameparams,
                                                    ::Type{Val{$n}})
                      arr.$(lbnd[n])
                    end))
        end
    end

    for n in 1:rank
        if fixed_ubnd[n]
            push!(decls,
                  :(function ubnd{$(typeparams...)}(::$typenameparams,
                                                    ::Type{Val{$n}})
                      $(ubnd[n])
                    end))
            push!(decls,
                  :(function ubnd{$(typeparams...)}(::Type{$typenameparams},
                                                    ::Type{Val{$n}})
                      $(ubnd[n])
                    end))
        else
            push!(decls,
                  :(function ubnd{$(typeparams...)}(arr::$typenameparams,
                                                    ::Type{Val{$n}})
                      arr.$(ubnd[n])
                    end))
        end
    end

    for n in 1:rank
        if fixed_stride[n]
            push!(decls,
                  :(function stride{$(typeparams...)}(::$typenameparams,
                                                      ::Type{Val{$n}})
                      $(Expr(:meta, :inline))
                      *(1, $([:(max(0, $(ubnd[m]) - $(lbnd[m]) + 1))
                              for m in 1:(n-1)]...))
                    end))
            push!(decls,
                  :(function stride{$(typeparams...)}(::Type{$typenameparams},
                                                      ::Type{Val{$n}})
                      $(Expr(:meta, :inline))
                      *(1, $([:(max(0, $(ubnd[m]) - $(lbnd[m]) + 1))
                              for m in 1:(n-1)]...))
                    end))
        else
            push!(decls,
                  :(function stride{$(typeparams...)}(arr::$typenameparams,
                                                      ::Type{Val{$n}})
                      arr.$(stride[n])
                    end))
        end
    end

    if fixed_length
        push!(decls,
              :(function length{$(typeparams...)}(::$typenameparams)
                  $(Expr(:meta, :inline))
                  *(1, $([:(max(0, $(ubnd[n]) - $(lbnd[n]) + 1))
                          for n in 1:rank]...))
                end))
        push!(decls,
              :(function length{$(typeparams...)}(::Type{$typenameparams})
                  $(Expr(:meta, :inline))
                  *(1, $([:(max(0, $(ubnd[n]) - $(lbnd[n]) + 1))
                          for n in 1:rank]...))
                end))
    else
        push!(decls,
              :(function length{$(typeparams...)}(arr::$typenameparams)
                  arr.length
                end))
    end

    if fixed_offset
        push!(decls,
              :(function offset{$(typeparams...)}(::$typenameparams)
                  $(Expr(:meta, :inline))
                  $([:($(stride[n]) =
                       $(n == 1 ?
                         1 :
                         :($(stride[n-1]) *
                           max(0, $(ubnd[n-1]) - $(lbnd[n-1]) + 1))))
                     for n in 1:rank]...)
                  +(0, $([:($(stride[n]) * $(lbnd[n])) for n in 1:rank]...))
                end))
        push!(decls,
              :(function offset{$(typeparams...)}(::Type{$typenameparams})
                  $(Expr(:meta, :inline))
                  $([:($(stride[n]) =
                       $(n == 1 ?
                         1 :
                         :($(stride[n-1]) *
                           max(0, $(ubnd[n-1]) - $(lbnd[n-1]) + 1))))
                     for n in 1:rank]...)
                  +(0, $([:($(stride[n]) * $(lbnd[n])) for n in 1:rank]...))
                end))
    else
        push!(decls,
              :(function offset{$(typeparams...)}(arr::$typenameparams)
                  arr.offset
                end))
    end

    # Array indexing

    # TODO: Define these as generated function once variable-length
    # argument lists are handled efficiently

    push!(decls,
          :(function isinbounds{$(typeparams...)}(arr::$typenameparams,
                                                  $([:($(symbol(:ind,n))::Int)
                                                     for n in 1:rank]...))
              $(Expr(:meta, :inline))
              (&)(true, $([:(lbnd(arr, Val{$n}) <= $(symbol(:ind,n)) <=
                             ubnd(arr, Val{$n}))
                           for n in 1:rank]...))
            end))

    push!(decls,
          :(function checkbounds{$(typeparams...)}(arr::$typenameparams,
                                                  $([:($(symbol(:ind,n))::Int)
                                                     for n in 1:rank]...))
              if !isinbounds(arr, $([symbol(:ind,n) for n in 1:rank]...))
                  Base.throw_boundserror(arr, tuple($([symbol(:ind,n)
                                                       for n in 1:rank]...)))
              end
            end))

    push!(decls,
          :(function checkbounds{$(typeparams...)}(arr::$typenameparams,
                                                   idx::LinearIndex)
              if !(1 <= idx.i <= length(arr))
                  Base.throw_boundserror(arr, tuple(ind))
              end
            end))

    push!(decls,
          :(function linearindex{$(typeparams...)}(arr::$typenameparams,
                                                   $([:($(symbol(:ind,n))::Int)
                                                      for n in 1:rank]...))
              $(Expr(:meta, :inline, :propagate_inbounds))
              # The @boundscheck macro does not exist in Julia 0.4
              $(Expr(:boundscheck, true))
              checkbounds(arr, $([symbol(:ind,n) for n in 1:rank]...))
              $(Expr(:boundscheck, :pop))
              LinearIndex(+(0, $([:($(symbol(:ind,n)) * stride(arr, Val{$n}))
                                  for n in 1:rank]...))
                          - offset(arr) + 1)
            end))

    push!(decls,
          :(function getindex{$(typeparams...)}(arr::$typenameparams,
                                                idx::LinearIndex)
              $(Expr(:meta, :inline, :propagate_inbounds))
              # The @boundscheck macro does not exist in Julia 0.4
              $(Expr(:boundscheck, true))
              checkbounds(arr, idx)
              $(Expr(:boundscheck, :pop))
              @inbounds val = arr.data[idx.i]
              val
            end))

    if isimmutable
        push!(decls,
              :(function setindex{$(typeparams...)}(arr::$typenameparams, val,
                                                    idx::LinearIndex)
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  $(Expr(:boundscheck, true))
                  checkbounds(arr, idx)
                  $(Expr(:boundscheck, :pop))
                  $typenameparams(nothing, setindex(arr.data, val, idx.i))
                end))
    else
        push!(decls,
              :(function setindex!{$(typeparams...)}(arr::$typenameparams, val,
                                                     idx::LinearIndex)
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  $(Expr(:boundscheck, true))
                  checkbounds(arr, idx)
                  $(Expr(:boundscheck, :pop))
                  @inbounds arr.data[idx.i] = val
                  val
                end))
    end

    push!(decls,
          :(function getindex{$(typeparams...)}(arr::$typenameparams,
                                                $([:($(symbol(:ind,n))::Int)
                                                   for n in 1:rank]...))
              $(Expr(:meta, :inline, :propagate_inbounds))
              idx = linearindex(arr, $([symbol(:ind,n) for n in 1:rank]...))
              @inbounds val = arr.data[idx.i]
              val
            end))

    if isimmutable
        push!(decls,
              :(function setindex{$(typeparams...)}(arr::$typenameparams,
                                                    val,
                                                    $([:($(symbol(:ind,n))::Int)
                                                       for n in 1:rank]...))
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  idx = linearindex(arr, $([symbol(:ind,n) for n in 1:rank]...))
                  $typenameparams(nothing, setindex(arr.data, val, idx.i))
                end))
    else
        push!(decls,
              :(function setindex!{$(typeparams...)}(arr::$typenameparams,
                                                     val,
                                                     $([:($(symbol(:ind,n))::Int)
                                                        for n in 1:rank]...))
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  idx = linearindex(arr, $([symbol(:ind,n) for n in 1:rank]...))
                  @inbounds arr.data[idx.i] = val
                  val
                end))
    end

    push!(decls,
          :(function getindex{$(typeparams...)}(arr::$typenameparams,
                                                inds::CartesianIndex{$rank})
              $(Expr(:meta, :inline, :propagate_inbounds))
              arr[$([:(inds[$i]) for i in 1:rank]...)]
            end))

    if isimmutable
        push!(decls,
              :(function setindex{$(typeparams...)}(arr::$typenameparams, val,
                                                    inds::CartesianIndex{$rank})
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  setindex(arr, val, $([:(inds[$i]) for i in 1:rank]...))
                end))
    else
        push!(decls,
              :(function setindex!{$(typeparams...)}(arr::$typenameparams, val,
                                                     inds::CartesianIndex{$rank})
                  $(Expr(:meta, :inline, :propagate_inbounds))
                  arr[$([:(inds[$i]) for i in 1:rank]...)] = val
                end))
    end

    eval(quote $(decls...) end)

    typename
end



@generated function genFlexArray{I}(::Type{Val{I}}, dimspecs...)
    @assert isa(I, Bool)
    @assert isa(dimspecs, Tuple)
    isimmutable = I
    rank = length(dimspecs)
    bndspec = ntuple(rank) do n
        dimspec = dimspecs[n]
        if dimspec <: Union{Colon, Tuple{Void, Void}}
            (false, false)
        elseif dimspec <: Union{Integer, Tuple{Integer, Void}}
            (true, false)
        elseif dimspec <: Union{UnitRange, Tuple{Integer, Integer}}
            (true, true)
        elseif dimspec <: Tuple{Void, Integer}
            (false, true)
        else
            @assert false
        end
    end
    if isimmutable
        if !all(bs -> bs === (true,true), bndspec)
            throw(BoundsError("All dimentions of an immutable array must have fixed lower and upper bounds"))
        end
    end
    :(genFlexArray(Val{I}, Val{$bndspec}))
end



export FlexArray
@generated function FlexArray(dimspecs...)
    @assert isa(dimspecs, Tuple)
    rank = length(dimspecs)
    dims = []
    for n in 1:rank
        dimspec = dimspecs[n]
        if dimspec <: Colon || dimspec <: Tuple{Void, Void}
            # do nothing
        elseif dimspec <: Integer
            push!(dims, :(Int(dimspecs[$n])))
        elseif dimspec <: Tuple{Integer, Void}
            push!(dims, :(Int(dimspecs[$n][1])))
        elseif dimspec <: UnitRange
            push!(dims, :(Int(dimspecs[$n].start)))
            push!(dims, :(Int(dimspecs[$n].stop)))
        elseif dimspec <: Tuple{Integer, Integer}
            push!(dims, :(Int(dimspecs[$n][1])))
            push!(dims, :(Int(dimspecs[$n][2])))
        elseif dimspec <: Tuple{Void, Integer}
            push!(dims, :(Int(dimspecs[$n][2])))
        else
            @assert false
        end
    end
    quote
        $(Expr(:meta, :inline))
        arrtype = genFlexArray(Val{false}, dimspecs...)
        arrtype{$(dims...)}
    end
end

export ImmutableArray
@generated function ImmutableArray(dimspecs...)
    @assert isa(dimspecs, Tuple)
    rank = length(dimspecs)
    dims = []
    sz = []
    for n in 1:rank
        dimspec = dimspecs[n]
        if dimspec <: Colon || dimspec <: Tuple{Void, Void}
            # do nothing
        elseif dimspec <: Integer
            push!(dims, :(Int(dimspecs[$n])))
        elseif dimspec <: Tuple{Integer, Void}
            push!(dims, :(Int(dimspecs[$n][1])))
        elseif dimspec <: UnitRange
            push!(dims, :(Int(dimspecs[$n].start)))
            push!(dims, :(Int(dimspecs[$n].stop)))
            push!(sz, :(max(0, (Int(dimspecs[$n].stop) -
                                Int(dimspecs[$n].start) + 1))))
        elseif dimspec <: Tuple{Integer, Integer}
            push!(dims, :(Int(dimspecs[$n][1])))
            push!(dims, :(Int(dimspecs[$n][2])))
            push!(sz, :(max(0, (Int(dimspecs[$n][2]) -
                                Int(dimspecs[$n][1]) + 1))))
        elseif dimspec <: Tuple{Void, Integer}
            push!(dims, :(Int(dimspecs[$n][2])))
        else
            @assert false
        end
    end
    quote
        $(Expr(:meta, :inline))
        arrtype = genFlexArray(Val{true}, dimspecs...)
        # TODO: allow flexible lower bounds as well, if the total size
        # is fixed
        len = *(1, $(sz...))
        arrtype{$(dims...), len}
    end
end



# Functor, Applicative Functor

import Base: map

@noinline function map_checkbounds(arr, other)
    lbnd_arr = lbnd(arr)
    ubnd_arr = ubnd(arr)
    if lbnd(other) != lbnd_arr
        Base.throw_boundserror(other, lbnd_arr)
    end
    if ubnd(other) != ubnd_arr
        Base.throw_boundserror(other, ubnd_arr)
    end
end

@generated function map_kernel!{R,T,N}(f, res::AbstractImmutableFlexArray{R,N},
                                       arr::AbstractImmutableFlexArray{T,N},
                                       others...)
    nothers = length(others)
    quote
        for i in eachindex(res)
            res = setindex(res, f(arr[i], $([:(others[$n][i])
                                             for n in 1:nothers]...)), i)
        end
        res
    end
end

@generated function map_kernel!{R,T,N}(f, res::AbstractMutableFlexArray{R,N},
                                       arr::AbstractMutableFlexArray{T,N},
                                       others...)
    nothers = length(others)
    quote
        for i in eachindex(res)
            res[i] = f(arr[i], $([:(others[$n][i]) for n in 1:nothers]...))
        end
        res
    end
end

@generated function map(f, arr::AbstractFlexArray, others...)
    nothers = length(others)
    quote
        $(Expr(:boundscheck, true))
        $([:(map_checkbounds(arr, others[$n])) for n in 1:nothers]...)
        $(Expr(:boundscheck, :pop))
        if isempty(arr)
            return similar(arr)
        end
        i0 = CartesianIndex(lbnd(arr))
        r0 = f(arr[i0], $([:(others[$n][i0]) for n in 1:nothers]...))
        R = typeof(r0)
        res = similar(arr, R)
        map_kernel!(f, res, arr, $([:(others[$n]) for n in 1:nothers]...))
    end
end



# Foldable

import Base: mapreduce, reduce

@generated function mapreduce_kernel(f, op, v0, arr::AbstractFlexArray,
                                     others...)
    nothers = length(others)
    quote
        res = v0
        for i in eachindex(arr)
            res = op(res, f(arr[i], $([:(others[$n][i])
                                       for n in 1:nothers]...)))
        end
        res
    end
end

@generated function mapreduce(f, op, v0, arr::AbstractFlexArray, others...)
    nothers = length(others)
    quote
        $(Expr(:boundscheck, true))
        $([:(map_checkbounds(arr, others[$n])) for n in 1:nothers]...)
        $(Expr(:boundscheck, :pop))
        mapreduce_kernel(f, op, v0, arr,
                         $([:(others[$n]) for n in 1:nothers]...))
    end
end

@generated function reduce(op, v0, arr::AbstractFlexArray, others...)
    nothers = length(others)
    quote
        mapreduce(identity, op, v0, arr,
                  $([:(others[$n]) for n in 1:nothers]...))
    end
end

end
