module [MyType]

# Recursive type through List
MyType : [
    Leaf Str,
    Branch (List MyType),  # Recursive through List
]