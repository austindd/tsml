module [
    TypeCache,
    empty_cache,
    cache_lookup,
    cache_insert,
    cache_stats,
    clear_cache,
]

import SimpleComprehensiveType as Type exposing [Type]
import Ast exposing [Node]

# Cache for type inference results
TypeCache : {
    entries : List CacheEntry,
    hits : U64,
    misses : U64,
    max_size : U64,
}

CacheEntry : {
    node_hash : U64,  # Hash of the AST node
    inferred_type : Type,
    access_count : U64,
}

# Create empty cache
empty_cache : U64 -> TypeCache
empty_cache = \max_size ->
    {
        entries: [],
        hits: 0,
        misses: 0,
        max_size,
    }

# Look up a type in the cache
cache_lookup : TypeCache, Node -> Result (Type, TypeCache) [NotCached]
cache_lookup = \cache, node ->
    hash = hash_node node

    when List.find_first cache.entries \entry -> entry.node_hash == hash is
        Ok entry ->
            # Update access count
            new_entries = List.map cache.entries \e ->
                if e.node_hash == hash then
                    { e & access_count: e.access_count + 1 }
                else
                    e

            new_cache = { cache &
                entries: new_entries,
                hits: cache.hits + 1,
            }
            Ok (entry.inferred_type, new_cache)

        Err _ ->
            Err NotCached

# Insert a type into the cache
cache_insert : TypeCache, Node, Type -> TypeCache
cache_insert = \cache, node, inferred_type ->
    hash = hash_node node

    # Check if already exists
    when List.find_first cache.entries \entry -> entry.node_hash == hash is
        Ok _ ->
            # Update existing entry
            new_entries = List.map cache.entries \entry ->
                if entry.node_hash == hash then
                    { entry &
                        inferred_type,
                        access_count: entry.access_count + 1,
                    }
                else
                    entry
            { cache & entries: new_entries }

        Err _ ->
            # Add new entry
            new_entry = {
                node_hash: hash,
                inferred_type,
                access_count: 1,
            }

            # Check if we need to evict
            if List.len cache.entries >= cache.max_size then
                # Evict least recently used
                sorted = List.sort_with cache.entries \a, b ->
                    when Num.compare a.access_count b.access_count is
                        LT -> LT
                        GT -> GT
                        EQ -> EQ

                # Keep all but the first (least used)
                kept = when List.drop_first sorted 1 is
                    [] -> []
                    rest -> rest

                { cache &
                    entries: List.append kept new_entry,
                    misses: cache.misses + 1,
                }
            else
                { cache &
                    entries: List.append cache.entries new_entry,
                    misses: cache.misses + 1,
                }

# Get cache statistics
cache_stats : TypeCache -> { hits : U64, misses : U64, size : U64, hit_rate : F64 }
cache_stats = \cache ->
    total = cache.hits + cache.misses
    hit_rate = if total == 0 then
        0.0
    else
        Num.to_f64 cache.hits / Num.to_f64 total

    {
        hits: cache.hits,
        misses: cache.misses,
        size: List.len cache.entries,
        hit_rate,
    }

# Clear the cache
clear_cache : TypeCache -> TypeCache
clear_cache = \cache ->
    { cache &
        entries: [],
        hits: 0,
        misses: 0,
    }

# Simple hash function for AST nodes
hash_node : Node -> U64
hash_node = |node|
    when node is
        Identifier { name } -> hash_string(name)
        NumberLiteral { value } -> hash_string(value)
        StringLiteral { value } -> hash_string(value)
        BooleanLiteral { value } -> if value then 1 else 0
        _ -> 42  # Default hash for complex nodes

# Simple string hash
hash_string : Str -> U64
hash_string = \s ->
    Str.to_utf8 s
    |> List.walk 0 \hash, byte ->
        (hash * 31 + Num.to_u64 byte) % 1000000007
