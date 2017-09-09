"""
Gives a reinterpreted view (of element type T) of the underlying array (of element type S).
If the size of `T` differs from the size of `S`, the array will be compressed/expanded in
the first dimension.
"""
struct ReinterpretArray{T,S,N,A<:AbstractArray{S, N}} <: AbstractArray{T, N}
    parent::A
    Base.reinterpret(::Type{T}, a::A) where {T,S,N,A<:AbstractArray{S, N}} = new{T, S, N, A}(a)
end

Base.eltype(a::ReinterpretArray{T}) where {T} = T
function Base.size(a::ReinterpretArray{T,S}) where {T,S}
    psize = size(a.parent)
    if sizeof(T) > sizeof(S)
        size1 = div(psize[1], div(sizeof(T), sizeof(S)))
    else
        size1 = psize[1] * div(sizeof(S), sizeof(T))
    end
    tuple(size1, Base.tail(psize)...)
end

@inline @propagate_inbounds function Base.getindex(a::ReinterpretArray{T,S,N}, inds::Vararg{Int, N}) where {T,S,N}
    if sizeof(T) == sizeof(S)
        return reinterpret(T, a[inds...])
    elseif sizeof(T) > sizeof(S)
        nels = div(sizeof(T), sizeof(S))
        ind_off = (inds[1]-1) * nels
        o = Ref{T}()
        optr = Base.unsafe_convert(Ref{T}, o)
        for i = 1:nels
            unsafe_store!(convert(Ptr{S}, optr)+(i-1)*sizeof(S), a.parent[ind_off + i, tail(inds)...])
        end
        return o[]
    else
        ind, sub = divrem(inds[1]-1, div(sizeof(S), sizeof(T)))
        r = Ref{S}(a.parent[1+ind, Base.tail(inds)...])
        @gc_preserve r begin
            rptr = Base.unsafe_convert(Ref{S}, r)
            ret = unsafe_load(convert(Ptr{T}, rptr) + sub*sizeof(T))
        end
        return ret
    end
end

@inline @propagate_inbounds function Base.setindex!(a::ReinterpretArray{T,S,N}, v, inds::Vararg{Int, N}) where {T,S,N}
    v = convert(T, v)::T
    if sizeof(T) == sizeof(S)
        return setindex!(a, reinterpret(S, v), inds...)
    elseif sizeof(T) > sizeof(S)
        nels = div(sizeof(T), sizeof(S))
        ind_off = (inds[1]-1) * nels
        o = Ref{T}(v)
        @gc_preserve o begin
            optr = Base.unsafe_convert(Ref{T}, o)
            for i = 1:nels
                a.parent[ind_off + i, Base.tail(inds)...] = unsafe_load(convert(Ptr{S}, optr)+(i-1)*sizeof(S))
            end
        end
    else
        ind, sub = divrem(inds[1]-1, div(sizeof(S), sizeof(T)))
        r = Ref{S}(a.parent[1+ind, Base.tail(inds)...])
        rptr = Base.unsafe_convert(Ref{S}, r)
        unsafe_store!(convert(Ptr{T}, rptr) + sub*sizeof(T), v)
        a.parent[1+ind] = r[]
    end
    return a
end
