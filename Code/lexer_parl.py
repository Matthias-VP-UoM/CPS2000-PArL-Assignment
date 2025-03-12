from enum import Enum

class TokenType(Enum):
    identifier = 1
    integerType = 2
    boolean = 3
    floatType = 4
    colour = 5
    whitespace = 6
    equals = 7
    operator = 8
    relOperator = 9
    decimalPoint = 10
    semicolon = 11    
    if_keyword = 12
    else_keyword = 13
    let_keyword = 14
    fun_keyword = 15
    ret_keyword = 16
    as_keyword = 17
    and_keyword = 18
    or_keyword = 19
    for_keyword = 20
    while_keyword = 21
    print_keyword = 22
    write_keyword = 23
    write_box_keyword = 24
    read_keyword = 25
    delay_keyword = 26
    clear_keyword = 27
    random_keyword = 28
    width_keyword = 29
    height_keyword = 30
    datatype = 31
    variableDecl = 32
    variableDeclColon = 33
    normalBracBegin = 34
    normalBracEnd = 35
    curlyBracBegin = 36
    curlyBracEnd = 37
    sqBracBegin = 38
    sqBracEnd = 39
    comma = 40
    arrow = 41
    arrayLength = 42
    commentBegin = 43
    commentEnd = 44
    void = 45
    end = 46

class Token:
    def __init__(self, t, l):
        self.type = t
        self.lexeme = l        

#Lexer class wrapper for code above
class Lexer:
    def __init__(self):
        self.max_states = 30
        self.lexeme_list = ["_", "letter", "digit", "boolVar", "colourVar", "ws", "eq", "sc", "cma", "addOp", "mulOp", "dp", "rop", "vardecl", "norBrac1", "norBrac2", "curBrac1", "curBrac2", "sqBrac1", "sqBrac2", "comment1", "comment2", "retArrow", "array", "print", "write", "read", "delay", "width", "height", "other"]
        self.states_list = [n for n in range(self.max_states+1)]
        self.states_accp = [n for n in range(1, self.max_states+1)]
        self.keywords = {
            "if": TokenType.if_keyword,
            "else": TokenType.else_keyword,
            "let": TokenType.let_keyword,
            "return": TokenType.ret_keyword,
            "fun": TokenType.fun_keyword,
            "as": TokenType.as_keyword,
            "and": TokenType.and_keyword,
            "or": TokenType.or_keyword,
            "for": TokenType.for_keyword,
            "while": TokenType.while_keyword,
            "__width": TokenType.width_keyword,
            "__height": TokenType.height_keyword
        }

        self.padKeywords = {
            "__print": TokenType.print_keyword,
            "__write": TokenType.write_keyword,
            "__write_box": TokenType.write_box_keyword,
            "__read": TokenType.read_keyword,
            "__delay": TokenType.delay_keyword,
            "__clear": TokenType.clear_keyword,
            "__random_int": TokenType.random_keyword
        }

        self.returnArrow = {
            "->": TokenType.arrow
        }

        self.boolVals = {
            "true": TokenType.boolean,
            "false": TokenType.boolean
        }

        self.dataTypes = {
            "int": TokenType.datatype,
            "float": TokenType.datatype,
            "bool": TokenType.datatype,
            "colour": TokenType.datatype
        }

        self.supportedComments = {
            "//": TokenType.commentBegin,
            "/*": TokenType.commentBegin,
            "*/": TokenType.commentEnd
        }

        self.rows = len(self.states_list)
        self.cols = len(self.lexeme_list)

        # Let's take integer -1 to represent the error state for this DFA
        self.Tx = [[-1 for j in range(self.cols)] for i in range(self.rows)]
        self.InitialiseTxTable();     

    def InitialiseTxTable(self):
        # Update Tx to represent the state transition function of the DFA
        # Variables and keywords
        self.Tx[0][self.lexeme_list.index("letter")] = 1
        self.Tx[0][self.lexeme_list.index("_")] = 1
        self.Tx[1][self.lexeme_list.index("_")] = 1
        self.Tx[1][self.lexeme_list.index("letter")] = 1
        self.Tx[1][self.lexeme_list.index("digit")] = 1

        #White Space
        self.Tx[0][self.lexeme_list.index("ws")] = 2
        self.Tx[2][self.lexeme_list.index("ws")] = 2

        #Eq sign (=)
        self.Tx[0][self.lexeme_list.index("eq")] = 3        

        # Comma (,)
        self.Tx[0][self.lexeme_list.index("cma")] = 4

        #Integers
        self.Tx[0][self.lexeme_list.index("digit")] = 5        
        self.Tx[5][self.lexeme_list.index("digit")] = 5
        self.Tx[7][self.lexeme_list.index("addOp")] = 5   

        # Negative numbers
        self.Tx[7][self.lexeme_list.index("digit")] = 11
        self.Tx[11][self.lexeme_list.index("digit")] = 11    

        #Semicolon sign (;)
        self.Tx[0][self.lexeme_list.index("sc")] = 6

        # Addition, Subtration, Multiplication and Division operators (+, -, *, /)
        self.Tx[0][self.lexeme_list.index("addOp")] = 7
        self.Tx[7][self.lexeme_list.index("addOp")] = 7
        self.Tx[0][self.lexeme_list.index("mulOp")] = 7
        self.Tx[7][self.lexeme_list.index("mulOp")] = 7

        # Decimal point (.)
        self.Tx[0][self.lexeme_list.index("dp")] = 8
        self.Tx[8][self.lexeme_list.index("dp")] = 8
        self.Tx[5][self.lexeme_list.index("dp")] = 8

        # Relational operators (<, >, <=, >=, !=, ==)
        self.Tx[0][self.lexeme_list.index("rop")] = 9
        self.Tx[3][self.lexeme_list.index("eq")] = 9  # Transition for == operator
        self.Tx[9][self.lexeme_list.index("eq")] = 9  # Transition for == operator
        self.Tx[7][self.lexeme_list.index("rop")] = 9  # Transition for return arrow '->'
        self.Tx[4][self.lexeme_list.index("eq")] = 9

        # Transitions for float numbers
        self.Tx[5][self.lexeme_list.index("dp")] = 8  # Transition to state 8 for decimal point
        self.Tx[5][self.lexeme_list.index("digit")] = 5  # Continue in state 5 for digits
        self.Tx[8][self.lexeme_list.index("digit")] = 10  # Transition to state 10 for digits after decimal point
        self.Tx[10][self.lexeme_list.index("digit")] = 10  # Continue in state 10 for digits
        #self.Tx[0][self.lexeme_list.index("digit")] = 10  # Transition to state 9 for digit
        #self.Tx[10][self.lexeme_list.index("digit")] = 10  # Continue in state 9 for digits
        #self.Tx[10][self.lexeme_list.index("dp")] = 11  # Transition to state 10 for decimal point
        #self.Tx[11][self.lexeme_list.index("digit")] = 11  # Continue in state 10 for digits

        # Colour variables (#.....)
        self.Tx[0][self.lexeme_list.index("colourVar")] = 12
        self.Tx[12][self.lexeme_list.index("digit")] = 12
        self.Tx[12][self.lexeme_list.index("letter")] = 12

        # Boolean variables (true, false)
        self.Tx[0][self.lexeme_list.index("boolVar")] = 13

        # Variable declarations (let ...:...)
        self.Tx[0][self.lexeme_list.index("vardecl")] = 14
        #self.Tx[1][self.lexeme_list.index("vardecl")] = 11

        # Normal brackets ()
        self.Tx[0][self.lexeme_list.index("norBrac1")] = 15
        self.Tx[0][self.lexeme_list.index("norBrac2")] = 17
        #self.Tx[13][self.lexeme_list.index("digit")] = 13
        #self.Tx[13][self.lexeme_list.index("letter")] = 13
        self.Tx[15][self.lexeme_list.index("norBrac1")] = 15
        self.Tx[17][self.lexeme_list.index("norBrac2")] = 17

        # Curly brackets {}
        self.Tx[0][self.lexeme_list.index("curBrac1")] = 16
        self.Tx[0][self.lexeme_list.index("curBrac2")] = 18
        #self.Tx[13][self.lexeme_list.index("digit")] = 13
        #self.Tx[13][self.lexeme_list.index("letter")] = 13
        self.Tx[16][self.lexeme_list.index("curBrac1")] = 16
        self.Tx[18][self.lexeme_list.index("curBrac2")] = 18

        # Print statement
        self.Tx[0][self.lexeme_list.index("print")] = 19

        # Write statement
        self.Tx[0][self.lexeme_list.index("write")] = 20

        # Read statement
        self.Tx[0][self.lexeme_list.index("read")] = 21

        # Delay statement
        self.Tx[0][self.lexeme_list.index("delay")] = 22

        # Square Brackets
        self.Tx[0][self.lexeme_list.index("sqBrac1")] = 24
        self.Tx[0][self.lexeme_list.index("sqBrac2")] = 25
        #self.Tx[23][self.lexeme_list.index("digit")] = 5
        #self.Tx[23][self.lexeme_list.index("sqBrac1")] = 23
        #self.Tx[5][self.lexeme_list.index("sqBrac2")] = 23

        # Comments (//, /*, */)
        # self.Tx[0][self.lexeme_list.index("comment1")] = 18
        # self.Tx[0][self.lexeme_list.index("comment2")] = 19
        # self.Tx[18][self.lexeme_list.index("digit")] = 18
        # self.Tx[18][self.lexeme_list.index("letter")] = 18
        # self.Tx[18][self.lexeme_list.index("ws")] = 18
        # self.Tx[19][self.lexeme_list.index("comment1")] = 19

        for row in self.Tx:
            print(row)

    def AcceptingStates(self, state):
        try:
            self.states_accp.index(state)
            return True
        except ValueError:
            return False
    
    def ValidateColour(self, color):
        # Check if the color starts with '#' and has exactly six characters
        if len(color) == 7 and color[0] == '#' and all(c in '0123456789abcdefABCDEF' for c in color[1:]):
            return True
        else:
            return False

    def GetTokenTypeByFinalState(self, state, lexeme):
        if state == 1:
            if lexeme in self.keywords:
                return Token(self.keywords[lexeme], lexeme)
            elif lexeme in self.padKeywords:
                return Token(self.padKeywords[lexeme], lexeme)
            elif lexeme in self.dataTypes:
                return Token(TokenType.datatype, lexeme)
            elif lexeme in self.boolVals:
                return Token(TokenType.boolean, lexeme)
            else:
                return Token(TokenType.identifier, lexeme)
        elif state == 2:
            return Token(TokenType.whitespace, lexeme)
        elif state == 3:
            return Token(TokenType.equals, lexeme)
        elif state == 4:
            return Token(TokenType.comma, lexeme)
        elif state == 5  or state == 11:
            return Token(TokenType.integerType, lexeme)
        elif state == 6:
            return Token(TokenType.semicolon, lexeme)
        elif state == 7:
            if lexeme in self.supportedComments:
                return Token(self.supportedComments[lexeme], lexeme)
            else:
                return Token(TokenType.operator, lexeme)
        elif state == 8:
            return Token(TokenType.decimalPoint, lexeme)
        elif state == 9:
            if lexeme in self.returnArrow:
                return Token(TokenType.arrow, lexeme)
            else:
                return Token(TokenType.relOperator, lexeme)
        elif state == 10:
            return Token(TokenType.floatType, lexeme)  # Return token type for float numbers
        elif state == 12:
            if self.ValidateColour(lexeme):
                return Token(TokenType.colour, lexeme)
            else:
                return Token(TokenType.void, "error")  # Return void token for invalid color
        elif state == 14:
            return Token(TokenType.variableDecl, lexeme)
        elif state == 15:
            return Token(TokenType.normalBracBegin, lexeme)
        elif state == 16:
            return Token(TokenType.curlyBracBegin, lexeme)
        elif state == 17:
            return Token(TokenType.normalBracEnd, lexeme)
        elif state == 18:
            return Token(TokenType.curlyBracEnd, lexeme)
        elif state == 23:
            return Token(TokenType.arrayLength, lexeme)
        elif state == 24:
            return Token(TokenType.sqBracBegin, lexeme)
        elif state == 25:
            return Token(TokenType.sqBracEnd, lexeme)
        else:
            return 'default result'


    def CatChar(self, character):
        cat = "other"
        if character.isalpha(): cat = "letter"
        if character.isdigit(): cat = "digit"
        if character == "_": cat = "_"
        if character == " ": cat = "ws"
        if character == ";": cat = "sc"
        if character == ",": cat = "cma"
        if character == "=": cat = "eq"
        if character == "+" or character == "-": cat = "addOp"
        if character == "*" or character == "/": cat = "mulOp"
        if character == ".": cat = "dp"
        if character == "<" or character == ">" or character == "!": cat = "rop"
        if character == ":": cat = "vardecl"
        if character == "#": cat = "colourVar"
        if character == "(": cat = "norBrac1"
        if character == ")": cat = "norBrac2"
        if character == "{": cat = "curBrac1"
        if character == "}": cat = "curBrac2"
        if character == "[": cat = "sqBrac1"
        if character == "]": cat = "sqBrac2"
        return cat

    def EndOfInput(self, src_program_str, src_program_idx):
        if (src_program_idx > len(src_program_str)-1):
            return True
        else:
            return False

    def NextChar(self, src_program_str, src_program_idx):
        if (not self.EndOfInput(src_program_str, src_program_idx)):
            return True, src_program_str[src_program_idx]
        else: 
            return False, "."

    def NextToken(self, src_program_str, src_program_idx):
        state = 0  #initial state is 0 - check Tx
        stack = []
        lexeme = ""
        stack.append(-2)  #insert the error state at the bottom of the stack.
        
        while (state != -1):
            if self.AcceptingStates(state): 
                stack.clear()
            stack.append(state)
            
            exists, character = self.NextChar(src_program_str, src_program_idx)
            lexeme += character
            if (not exists): 
                #print("LAST LEXEME: ", lexeme); 
                break  #Break out of loop if we're at the end of the string
            src_program_idx = src_program_idx + 1
            
            cat = self.CatChar(character)
            state = self.Tx[state][self.lexeme_list.index(cat)]
            #if state == 0 and len(lexeme) == 2:
                #if lexeme == "==":
                    #state = 8  # Relational operator state
                #elif lexeme[0] in ("<", ">", "!") and lexeme[1] == "=":
                    #state = 8  # Relational operator state
            #print("Lexeme: ", lexeme, " => NEXT STATE: ", state, "  => CAT: ", cat, "  => CHAR:", character, "  => STACK: ", stack)
        
        lexeme = lexeme[:-1] #remove the last character added which sent the lexer to state -1    

        syntax_error = False
        #rollback
        while (len(stack) > 0):
            if (stack[-1] == -2): #report a syntax error
                syntax_error = True
                break    

            #Pop this state if not an accepting state.
            if (not self.AcceptingStates(stack[-1])):
                stack.pop()
                print("POPPED => ", stack)
                lexeme = lexeme[:-1]
            
            #This is an accepting state ... return it.    
            else:
                state = stack.pop()
                break
        
        #print("Lexeme: ", lexeme, "with state: ", state)

        if syntax_error:
            return Token(TokenType.void, "error"), "error"

        if self.AcceptingStates(state):
            return self.GetTokenTypeByFinalState(state, lexeme), lexeme
        else: 
            return Token(TokenType.void, "error"), "error"


    def GenerateTokens(self, src_program_str):
        print("INPUT:: " + src_program_str)
        tokens_list = []
        src_program_idx = 0
        token, lexeme = self.NextToken(src_program_str, src_program_idx)
        tokens_list.append(token)

        while (token.type != TokenType.end):           #this loop is simulating the Parser asking for the next Token
            src_program_idx = src_program_idx + len(lexeme)
            #print ("Nxt TOKEN: ", token, " ", lexeme, "(", len(lexeme), ")  => IDX: ", src_program_idx)
            if (not self.EndOfInput(src_program_str, src_program_idx)):
                token, lexeme = self.NextToken(src_program_str, src_program_idx)
                tokens_list.append(token)
                if (token.type == TokenType.void):
                    break   #A lexical error was encountered
            else:
                token, lexeme = Token(TokenType.end, "END"), "END"   #The end of the source program

        if (token.type == TokenType.end):
            print("Encountered end of Input token!! DONE")

        return tokens_list
    
    def GetPadKeywords(self):
        return self.padKeywords


lex = Lexer()
test_string_a = "/* Hello World */ fun main(x1, x2){let x:int=24; let c1:colour = #00ff00; let c2:colour = #ffffff - c1; let cond:bool = true; if ((x!=23) and (cond == true)) x=23; else return ; float y= 10.5; int z =4;  y<=12; x==23; x >z; z!=0; a= z+3; b =y-6; c = x*5; d=y/z;}"
test_string_b = "fun main() -> int { }"
test_string_c = "m = a[15]"
toks = lex.GenerateTokens(test_string_c)

for t in toks:
    print(t.type, t.lexeme)