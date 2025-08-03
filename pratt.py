class Token:
    def __init__(self, type, value, lbp=0):
        self.type = type
        self.value = value
        self.lbp = lbp  # left binding power
    
    def __repr__(self):
        return f"Token({self.type}, {self.value}, {self.lbp})"

class PrattParser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current_token = self.tokens[0] if tokens else None
    
    def advance(self):
        """Move to the next token"""
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current_token = self.tokens[self.pos]
        else:
            self.current_token = None
    
    def parse_expression(self, min_precedence=0):
        """Main parsing function"""
        if not self.current_token:
            raise SyntaxError("Unexpected end of input")
        
        # Get the left side using nud (null denotation)
        left = self.nud()
        
        # Process operators with led (left denotation)
        while (self.current_token and 
               self.current_token.lbp > min_precedence):
            left = self.led(left)
        
        return left
    
    def nud(self):
        """Null denotation - handles tokens at start of expressions"""
        token = self.current_token
        self.advance()
        
        if token.type == "NUMBER":
            return {"type": "literal", "value": token.value}
        
        elif token.type == "IDENTIFIER":
            return {"type": "identifier", "value": token.value}
        
        elif token.type == "LPAREN":
            # Parse parenthesized expression
            expr = self.parse_expression()
            if not self.current_token or self.current_token.type != "RPAREN":
                raise SyntaxError("Expected closing parenthesis")
            self.advance()
            return expr
        
        elif token.type in ["PLUS", "MINUS"]:
            # Unary operators
            return {
                "type": "unary",
                "operator": token.value,
                "operand": self.parse_expression(50)  # High precedence for unary
            }
        
        else:
            raise SyntaxError(f"Unexpected token: {token}")
    
    def led(self, left):
        """Left denotation - handles binary operators"""
        token = self.current_token
        self.advance()
        
        if token.type in ["PLUS", "MINUS", "MULTIPLY", "DIVIDE"]:
            # For left-associative operators, use token.lbp
            # For right-associative, use token.lbp - 1
            right = self.parse_expression(token.lbp)
            return {
                "type": "binary",
                "operator": token.value,
                "left": left,
                "right": right
            }
        
        elif token.type == "LPAREN":
            # Function call
            args = []
            if self.current_token and self.current_token.type != "RPAREN":
                args.append(self.parse_expression())
                while self.current_token and self.current_token.type == "COMMA":
                    self.advance()  # consume comma
                    args.append(self.parse_expression())
            
            if not self.current_token or self.current_token.type != "RPAREN":
                raise SyntaxError("Expected closing parenthesis")
            self.advance()
            
            return {
                "type": "call",
                "function": left,
                "arguments": args
            }
        
        else:
            raise SyntaxError(f"Unexpected token in led: {token}")

def tokenize(text):
    """Simple tokenizer for demonstration"""
    tokens = []
    i = 0
    
    while i < len(text):
        char = text[i]
        
        if char.isspace():
            i += 1
            continue
        
        elif char.isdigit():
            num = ""
            while i < len(text) and (text[i].isdigit() or text[i] == '.'):
                num += text[i]
                i += 1
            tokens.append(Token("NUMBER", float(num) if '.' in num else int(num)))
        
        elif char.isalpha():
            ident = ""
            while i < len(text) and (text[i].isalnum() or text[i] == '_'):
                ident += text[i]
                i += 1
            tokens.append(Token("IDENTIFIER", ident))
        
        elif char == '+':
            tokens.append(Token("PLUS", char, lbp=10))
            i += 1
        elif char == '-':
            tokens.append(Token("MINUS", char, lbp=10))
            i += 1
        elif char == '*':
            tokens.append(Token("MULTIPLY", char, lbp=20))
            i += 1
        elif char == '/':
            tokens.append(Token("DIVIDE", char, lbp=20))
            i += 1
        elif char == '(':
            tokens.append(Token("LPAREN", char, lbp=80))  # High for function calls
            i += 1
        elif char == ')':
            tokens.append(Token("RPAREN", char))
            i += 1
        elif char == ',':
            tokens.append(Token("COMMA", char))
            i += 1
        else:
            raise SyntaxError(f"Unexpected character: {char}")
    
    return tokens

# Example usage
def parse_and_print(expression):
    print(f"\nParsing: {expression}")
    try:
        tokens = tokenize(expression)
        print(f"Tokens: {tokens}")
        
        parser = PrattParser(tokens)
        ast = parser.parse_expression()
        
        import json
        print(f"AST: {json.dumps(ast, indent=2)}")
    except Exception as e:
        print(f"Error: {e}")

# Test examples
if __name__ == "__main__":
    parse_and_print("3 + 4 * 5")      # Should respect precedence
    parse_and_print("(3 + 4) * 5")    # Parentheses override precedence
    parse_and_print("-3 + 4")         # Unary minus
    parse_and_print("func(1, 2 + 3)") # Function call with expression args

# self.precedence = {
#             "EOF": 0,
#
#             # Assignment operators (right associative)
#             "ASSIGN": 1,         # =
#             "PLUS_ASSIGN": 1,    # +=
#             "MINUS_ASSIGN": 1,   # -=
#             "MULTIPLY_ASSIGN": 1, # *=
#             "DIVIDE_ASSIGN": 1,  # /=
#
#             # Ternary conditional (right associative)
#             "QUESTION": 2,       # condition ? true : false
#
#             # Logical OR
#             "LOGICAL_OR": 3,     # ||
#
#             # Logical AND  
#             "LOGICAL_AND": 4,    # &&
#
#             # Equality
#             "EQUALITY": 5,       # ==, !=, ===, !==
#
#             # Relational
#             "RELATIONAL": 6,     # <, >, <=, >=
#
#             # Additive
#             "ADDITIVE": 7,       # +, -
#
#             # Multiplicative
#             "MULTIPLICATIVE": 8, # *, /, %
#
#             # Exponentiation (right associative)
#             "EXPONENTIATION": 9, # **
#
#             # Unary (handled in nud, not led)
#             "UNARY": 10,         # +, -, !, ~, typeof
#
#             # Postfix
#             "POSTFIX": 11,       # ++, --
#
#             # Call and member access
#             "CALL": 12,          # func(), obj.prop, obj[prop]
#         }
