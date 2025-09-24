module [MyType]

# Minimal recursive type that triggers the bug
MyType : [
    Leaf Str,
    Branch MyType,  # Recursive reference
]