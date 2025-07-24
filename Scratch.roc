module []

TypeA := [Nil, X { b : TypeB }]
TypeB := [Nil, X { a : TypeA }]

# TypeA := [
#     X {
#       left : TypeB,
#       right : TypeA,
#     },
#     Nil,
# ]
#
# TypeB := [
#     X TypeA,
#     Nil,
# ]
