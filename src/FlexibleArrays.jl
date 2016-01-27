module FlexibleArrays

export AbstractFlexArray
abstract AbstractFlexArray{T,N} <: DenseArray{T,N}

# eltype, ndims are provided by DenseArray

export lbnd, ubnd
import Base: size

lbnd(arr::AbstractFlexArray, n::Int) = lbnd(arr, Val{n})
ubnd(arr::AbstractFlexArray, n::Int) = ubnd(arr, Val{n})
size(arr::AbstractFlexArray, n::Int) = size(arr, Val{n})

lbnd{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = lbnd(T, Val{n})
ubnd{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = ubnd(T, Val{n})
size{T <: AbstractFlexArray}(arr::Type{T}, n::Int) = size(T, Val{n})

# Note: Use ntuple instead of generated functions once the closure in ntuple is
# efficient
@generated function lbnd{T,N}(arr::AbstractFlexArray{T,N})
    Expr(:tuple, [:(lbnd(arr, Val{$n})) for n in 1:N]...)
end
@generated function ubnd{T,N}(arr::AbstractFlexArray{T,N})
    Expr(:tuple, [:(ubnd(arr, Val{$n})) for n in 1:N]...)
end
@generated function size{T,N}(arr::AbstractFlexArray{T,N})
    Expr(:tuple, [:(size(arr, Val{$n})) for n in 1:N]...)
end

@generated function lbnd{T <: AbstractFlexArray}(::Type{T})
    Expr(:tuple, [:(lbnd(T, Val{$n})) for n in 1:ndims(T)]...)
end
@generated function ubnd{T <: AbstractFlexArray}(::Type{T})
    Expr(:tuple, [:(ubnd(T, Val{$n})) for n in 1:ndims(T)]...)
end
@generated function size{T <: AbstractFlexArray}(::Type{T})
    Expr(:tuple, [:(size(T, Val{$n})) for n in 1:ndims(T)]...)
end



import Base: convert, getindex, length, setindex!

typealias DimSpec NTuple{2, Nullable{Int}}

have_lb(ds::DimSpec) = !isnull(ds[1])
have_ub(ds::DimSpec) = !isnull(ds[2])
get_lb(ds::DimSpec) = get(ds[1])
get_ub(ds::DimSpec) = get(ds[2])

export FlexArray
@generated function FlexArray{T}(::Type{Val{T}})
    @assert isa(T, Tuple)
    N = length(T)
    @assert isa(T, NTuple{N, DimSpec})
    rank = N
    dimspecs = T

    have_all_lb = true
    for n in 1:rank
        have_all_lb &= have_lb(dimspecs[n])
    end
    have_all_ub = true
    for n in 1:rank
        have_all_ub &= have_ub(dimspecs[n])
    end
    have_all_bnd = have_all_lb && have_all_ub
    if have_all_bnd
        size = 1
        for n in 1:rank
            size *= max(0, get_ub(dimspecs[n]) - get_lb(dimspecs[n]) + 1)
        end
    else
        size = nothing
    end

    have_str = Vector{Bool}(rank+1)
    get_str = Vector{Any}(rank+1)
    have_str[1] = true
    get_str[1] = 1
    for n in 2:rank+1
        have_str[n] =
            have_str[n-1] && have_lb(dimspecs[n-1]) && have_ub(dimspecs[n-1])
        if have_str[n]
            get_str[n] =
                get_str[n-1] *
                max(0, (get_ub(dimspecs[n-1]) - get_lb(dimspecs[n-1]) + 1))
        else
            get_str[n] = nothing
        end
    end

    have_offset = rank == 0 || have_str[rank] && have_lb(dimspecs[rank])
    if have_offset
        get_offset =
            - sum(Int[get_str[n] * get_lb(dimspecs[n]) for n in 1:rank])
    else
        get_offset = nothing
    end

    decls = []

    # Type declaration

    typename = gensym(:FlexArrayImpl)
    # @show typename
    block = []
    for n in 1:rank
        # @show dimspecs
        if have_lb(dimspecs[n])
            push!(block, :(const $(symbol(:lbnd,n)) = $(get_lb(dimspecs[n]))))
        else
            push!(block, :($(symbol(:lbnd,n))::Int))
        end
        if have_ub(dimspecs[n])
            push!(block, :(const $(symbol(:ubnd,n)) = $(get_ub(dimspecs[n]))))
        else
            push!(block, :($(symbol(:ubnd,n))::Int))
        end
        if have_str[n]
            push!(block, :(const $(symbol(:str,n)) = $(get_str[n])))
        else
            push!(block, :($(symbol(:str,n))::Int))
        end
    end
    if have_str[rank+1]
        push!(block, :(const size = $(get_str[rank+1])))
    else
        push!(block, :(size::Int))
    end
    if have_offset
        push!(block, :(const offset = $(get_offset)))
    else
        push!(block, :(offset::Int))
    end
    # TODO: use a more low-level array representation
    push!(block, :(data::Vector{T}))

    # Constructor
    funcargs = []
    for n in 1:rank
        if !have_lb(dimspecs[n])
            push!(funcargs, :($(symbol(:lbnd,n))::Int))
        else
            push!(funcargs, :(::Void))
        end
        if !have_ub(dimspecs[n])
            push!(funcargs, :($(symbol(:ubnd,n))::Int))
        else
            push!(funcargs, :(::Void))
        end
    end

    body = []
    for n in 2:rank
        if !have_str[n]
            push!(body,
                :($(symbol(:str,n)) =
                    $(symbol(:str,n-1)) *
                    max(0, $(symbol(:ubnd,n-1)) - $(symbol(:lbnd,n-1)) + 1)))
        end
    end
    if !have_str[rank+1]
        push!(body,
            :(size =
                $(symbol(:str,rank)) *
                max(0, $(symbol(:ubnd,rank)) - $(symbol(:lbnd,rank)) + 1)))
    end
    if !have_offset
        push!(body,
            let
                args = []
                for n in 1:rank
                    push!(args, :($(symbol(:str,n)) * $(symbol(:lbnd,n))))
                end
                :(offset = - $(Expr(:call, +, 0, args...)))
            end)
    end
    push!(body,
        let
            args = []
            for n in 1:rank
                if !have_lb(dimspecs[n])
                    push!(args, :($(symbol(:lbnd,n))))
                end
                if !have_ub(dimspecs[n])
                    push!(args, :($(symbol(:ubnd,n))))
                end
                if !have_str[n]
                    push!(args, :($(symbol(:str,n))))
                end
            end
            if !have_str[rank+1]
                push!(args, :size)
            end
            if !have_offset
                push!(args, :offset)
            end
            Expr(:call, :new, args..., :(Vector{T}(size)))
        end)

    push!(block,
        :($(Expr(:call, typename, funcargs...)) = $(Expr(:block, body...))))

    push!(decls,
        Expr(:type, false,
            Expr(:<:, :($typename{T}), :(AbstractFlexArray{T,$rank})),
            Expr(:block, block...)))

    # Convenience constructor with a saner syntax

    funcargs = []
    for n in 1:rank
        if have_lb(dimspecs[n])
            if have_ub(dimspecs[n])
                push!(funcargs, :(::Colon))
            else
                push!(funcargs, :($(symbol(:ubnd,n))::Int))
            end
        else
            if have_ub(dimspecs[n])
                # no good syntax for this
            else
                push!(funcargs, :($(symbol(:bnds,n))::UnitRange{Int}))
            end
        end
    end

    body = []
    push!(body,
        let
            args = []
            for n in 1:rank
                if have_lb(dimspecs[n])
                    if have_ub(dimspecs[n])
                        push!(args, :nothing)
                        push!(args, :nothing)
                    else
                        push!(args, :nothing)
                        push!(args, :($(symbol(:ubnd,n))))
                    end
                else
                    if have_ub(dimspecs[n])
                        push!(args, 1)   # see above
                        push!(args, :nothing)
                    else
                        push!(args, :($(symbol(:bnds,n)).start))
                        push!(args, :($(symbol(:bnds,n)).stop))
                    end
                end
            end
            Expr(:call, :($typename{T}), args...)
        end)

    push!(decls,
        :($(Expr(:call, :(convert{T}), :(::Type{$typename{T}}), funcargs...)) =
            $(Expr(:block, body...))))

    # Lower/upper bounds, size, length

    for n in 1:rank
        if have_lb(dimspecs[n])
            push!(decls, :(lbnd{T}(::$typename{T}, ::Type{Val{$n}}) =
                $(get_lb(dimspecs[n]))))
            push!(decls, :(lbnd{T}(::Type{$typename{T}}, ::Type{Val{$n}}) =
                $(get_lb(dimspecs[n]))))
        else
            push!(decls, :(lbnd{T}(arr::$typename{T}, ::Type{Val{$n}}) =
                arr.$(symbol(:lbnd,n))))
        end
    end

    push!(decls, :(export ubnd))
    for n in 1:rank
        if have_ub(dimspecs[n])
            push!(decls, :(ubnd{T}(::$typename{T}, ::Type{Val{$n}}) =
                $(get_ub(dimspecs[n]))))
            push!(decls, :(ubnd{T}(::Type{$typename{T}}, ::Type{Val{$n}}) =
                $(get_ub(dimspecs[n]))))
        else
            push!(decls, :(ubnd{T}(arr::$typename{T}, ::Type{Val{$n}}) =
                arr.$(symbol(:ubnd,n))))
        end
    end

    for n in 1:rank
        push!(decls, :(size{T}(arr::$typename{T}, ::Type{Val{$n}}) =
            ubnd(arr, Val{$n}) - lbnd(arr, Val{$n}) + 1))
        if have_lb(dimspecs[n]) && have_ub(dimspecs[n])
            push!(decls, :(size{T}(::Type{$typename{T}}, ::Type{Val{$n}}) =
                ubnd($typename{T}, Val{$n}) - lbnd($typename{T}, Val{$n}) + 1))
        end
    end

    if have_all_bnd
        push!(decls, :(length{T}(::$typename{T}) = $size))
        push!(decls, :(length{T}(::Type{$typename{T}}) = $size))
    else
        push!(decls, :(length{T}(arr::$typename{T}) = arr.size))
    end

    # Array indexing

    push!(decls,
        let
            funcargs = []
            for n in 1:rank
                push!(funcargs, :($(symbol(:ind,n))::Int))
            end
            body = []
            # for n in 1:rank
            #     # check indices
            #     push!(body,
            #         :(@assert lbnd(arr, Val{$n}) <= $(symbol(:ind,n)) <=
            #             ubnd(arr, Val{$n})))
            # end
            args = []
            if have_offset
                push!(args, get_offset)
            else
                push!(args, :(arr.offset))
            end
            for n in 1:rank
                if have_str[n]
                    push!(args, :($(get_str[n]) * $(symbol(:ind,n))))
                else
                    push!(args, :(arr.$(symbol(:str,n)) * $(symbol(:ind,n))))
                end
            end
            push!(body, :(arr.data[$(Expr(:call, :+, 1, args...))]))
            :($(Expr(:call, :(getindex{T}),
                :(arr::$typename{T}), funcargs...)) =
                $(Expr(:block, body...)))
        end)

    push!(decls,
        let
            funcargs = []
            for n in 1:rank
                push!(funcargs, :($(symbol(:ind,n))::Int))
            end
            body = []
            # for n in 1:rank
            #     # check indices
            #     push!(body,
            #         :(@assert lbnd(arr, Val{$n}) <= $(symbol(:ind,n)) <=
            #             ubnd(arr, Val{$n})))
            # end
            args = []
            if have_offset
                push!(args, get_offset)
            else
                push!(args, :(arr.offset))
            end
            for n in 1:rank
                if have_str[n]
                    push!(args, :($(get_str[n]) * $(symbol(:ind,n))))
                else
                    push!(args, :(arr.$(symbol(:str,n)) * $(symbol(:ind,n))))
                end
            end
            push!(body, :(arr.data[$(Expr(:call, :+, 1, args...))] = val))
            :($(Expr(:call, :(setindex!{T}),
                :(arr::$typename{T}), :val, funcargs...)) =
                $(Expr(:block, body...)))
        end)

    eval(Expr(:block, decls...))

    typename
end

# Convenience wrapper with a saner syntax
@inline function FlexArray(dimspecs...)
    @assert isa(dimspecs, Tuple)
    dimspectuple = ntuple(length(dimspecs)) do n
        dimspec = dimspecs[n]
        if isa(dimspec, Colon)
            (Nullable{Int}(), Nullable{Int}())
        elseif isa(dimspec, Integer)
            (Nullable(Int(dimspec)), Nullable{Int}())
        elseif isa(dimspec, UnitRange)
            (Nullable(Int(dimspec.start)), Nullable(Int(dimspec.stop)))
        else
            @assert false
        end
    end
    FlexArray(Val{dimspectuple})
end

end
