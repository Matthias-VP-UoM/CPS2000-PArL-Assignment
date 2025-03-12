import astnodes_parl as ast
import visitor_parl as vis
import lexer_parl as lex
import random

class Parser:
    def __init__(self, src_program_str):
        self.name = "PARSEAR"
        self.lexer = lex.Lexer()
        self.index = -1  #start at -1 so that the first token is at index 0
        self.next = -1
        self.if_count = 0
        self.let_count = 0
        self.stmt_type = []
        self.declared_vars = {}
        self.declared_var_vals = {}
        self.declared_functions = {}
        self.declared_functions_params = {}
        self.src_program = src_program_str
        self.stmt_list = self.src_program.split(";")
        self.stmt_index = 0
        self.tokens = self.lexer.GenerateTokens(self.src_program)
        #print("[Parser] Lexer generated token list ::")
        #for t in self.tokens:
        #    print(t.type, t.lexeme)
        self.crtToken = lex.Token("", lex.TokenType.void)
        self.nextToken = lex.Token("", lex.TokenType.void)
        self.ASTroot = ast.ASTProgramNode

    def GetNextTokenSkipWS(self):
        self.index += 1   #Grab the next token
        if (self.index < len(self.tokens)):
            self.crtToken = self.tokens[self.index]
        else:
            self.crtToken = lex.Token(lex.TokenType.end, "END")
    
    def PredictNextTokenSkipWS(self):
        #self.next = self.index + 1   #Grab the next token
        if (self.next < len(self.tokens)):
            self.nextToken = self.tokens[self.next]
        else:
            self.nextToken = lex.Token(lex.TokenType.end, "END")

    def GetNextToken(self):
        self.GetNextTokenSkipWS()
        while (self.crtToken.type == lex.TokenType.whitespace):
            #print("--> Skipping WS")
            self.GetNextTokenSkipWS()
    
    def PredictNextToken(self):
        self.next = self.index + 1   #Grab the next token
        self.PredictNextTokenSkipWS()
        while (self.nextToken.type == lex.TokenType.whitespace):
            #print("--> Skipping WS")
            self.PredictNextTokenSkipWS()
            self.next += 1

        #print("Next Token Set to ::: ", self.crtToken.type, self.crtToken.lexeme)

    def ParseExpression(self):
        #for now we'll assume an expression can only be an integer
        if (self.crtToken.type == lex.TokenType.integerType):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTIntegerNode(value)
        elif (self.crtToken.type == lex.TokenType.floatType):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTFloatNode(value)
        if (self.crtToken.type == lex.TokenType.boolean):
            value = self.crtToken.lexeme
            if (value == "true"):
                value = 1
            elif (value == "false"):
                value = 0
            self.GetNextToken()
            return ast.ASTBoolNode(value)
        if (self.crtToken.type == lex.TokenType.colour):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTColourNode(value)
        if (self.crtToken.type == lex.TokenType.width_keyword):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTWidthNode(value)
        if (self.crtToken.type == lex.TokenType.height_keyword):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTHeightNode(value)
        if (self.crtToken.type == lex.TokenType.operator):
            value = self.crtToken.lexeme
            self.GetNextToken()
            return ast.ASTOperatorNode(value)
    
    def ParseEndToken(self):
        if (self.crtToken.type == lex.TokenType.end):
            self.stmt_type.append(ast.ASTEndNode())
            return ast.ASTEndNode()
    
    def ParseWhileStatement(self):
        if (self.crtToken.type == lex.TokenType.while_keyword):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.normalBracBegin):
            self.GetNextToken()
        
        cond_lhs = None

        if (self.crtToken.type == lex.TokenType.identifier):
            #create AST node to store the identifier            
            cond_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        else:
            cond_lhs = self.ParseExpression()
        
        if (self.crtToken.type == lex.TokenType.relOperator):
            comp_op = ast.ASTBinaryOpNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        cond_rhs = None

        if (self.crtToken.type != lex.TokenType.identifier):
            cond_rhs = self.ParseExpression()
        else:
            cond_rhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        for_condition = ast.ASTConditionNode(cond_lhs, comp_op, cond_rhs)
        
        if (self.crtToken.type == lex.TokenType.normalBracEnd):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.curlyBracBegin):
            self.GetNextToken()
        
        while_body = self.ParseBlock()

        if (self.crtToken.type == lex.TokenType.curlyBracEnd):
            self.GetNextToken()
        
        self.stmt_type.append(ast.ASTWhileNode(for_condition, while_body))
        return ast.ASTWhileNode(for_condition, while_body)
    
    def ParseForStatement(self):
        if (self.crtToken.type == lex.TokenType.for_keyword):
            self.GetNextToken()

        if (self.crtToken.type == lex.TokenType.normalBracBegin):
            self.GetNextToken()
        
        for_left = None
        if (self.crtToken.type == lex.TokenType.identifier):
            for_left = self.ParseAssignment()
        elif (self.crtToken.type == lex.TokenType.let_keyword):
            for_left = self.ParseVarDecl()
        
        if (self.crtToken.type == lex.TokenType.semicolon):
            self.GetNextToken()
            self.stmt_index += 1
        else:
            print("Syntax Error - Missing semicolon for separating parts of for loop!")
        
        if (self.crtToken.type == lex.TokenType.normalBracBegin):
            self.GetNextToken()
        
        cond_lhs = None

        if (self.crtToken.type == lex.TokenType.identifier):
            #create AST node to store the identifier            
            cond_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        else:
            cond_lhs = self.ParseExpression()
        
        if (self.crtToken.type == lex.TokenType.relOperator):
            comp_op = ast.ASTBinaryOpNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        cond_rhs = None

        if (self.crtToken.type != lex.TokenType.identifier):
            cond_rhs = self.ParseExpression()
        else:
            cond_rhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        for_condition = ast.ASTConditionNode(cond_lhs, comp_op, cond_rhs)

        if (self.crtToken.type == lex.TokenType.semicolon):
            self.GetNextToken()
            self.stmt_index += 1
        else:
            print("Syntax Error - Missing semicolon for separating parts of for loop!")
        
        for_right = None
        
        if (self.crtToken.type == lex.TokenType.identifier):
            for_right = self.ParseAssignment()
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.normalBracEnd):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.curlyBracBegin):
            self.GetNextToken()
        
        for_body = self.ParseBlock()

        if (self.crtToken.type == lex.TokenType.curlyBracEnd):
            self.GetNextToken()
        
        self.stmt_type.append(ast.ASTForNode(for_left, for_condition, for_right, for_body))
        return ast.ASTForNode(for_left, for_condition, for_right, for_body)
    
    def ParseReturn(self):
        self.GetNextToken()

        if (self.crtToken.type != lex.TokenType.identifier):
            ret_val = self.ParseExpression()
        else:
            ret_val = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        self.stmt_type.append(ast.ASTReturnNode(ret_val))
        return ast.ASTReturnNode(ret_val)
    
    def ParseArrayElement(self):
        var_name = ast.ASTVariableNode(self.crtToken.lexeme)
        var_arr = ast.ASTArrayNode(self.declared_var_vals[var_name.lexeme], ast.ASTIntegerNode(len(self.declared_var_vals[var_name.lexeme])))
        self.GetNextToken()
        self.GetNextToken()

        if (self.crtToken.type == lex.TokenType.integerType):
            index = self.ParseExpression()
        
        if (self.crtToken.type == lex.TokenType.sqBracEnd):
            self.GetNextToken()
        
        return ast.ASTArrayElementNode(index, var_arr)
    
    def ParsePadFunction(self):
        pad_type = self.crtToken.type
        if pad_type == lex.TokenType.print_keyword:
            self.GetNextToken()
            if (self.crtToken.type == lex.TokenType.identifier):
                self.PredictNextToken()
                if (self.nextToken.type == lex.TokenType.sqBracBegin):
                    pad_val = self.ParseArrayElement()
                else:
                    pad_val = ast.ASTVariableNode(self.crtToken.lexeme)
                    self.GetNextToken()
            else:
                pad_val = self.ParseExpression()
            #self.GetNextToken()
        elif pad_type == lex.TokenType.delay_keyword:
            self.GetNextToken()
            pad_val = self.ParseExpression()
            #self.GetNextToken()
        elif pad_type == lex.TokenType.clear_keyword:
            self.GetNextToken()
            if self.crtToken.type != lex.TokenType.identifier:
                pad_val = self.ParseExpression()
            else:
                pad_val = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            #self.GetNextToken()
        elif pad_type == lex.TokenType.write_keyword:
            self.GetNextToken()
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_1 = var_name
                    self.GetNextToken()
            else:
                pad_val_1 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_2 = var_name
                    #pad_val_2 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_2 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_3 = var_name
                    #pad_val_3 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_3 = self.ParseExpression()
            #self.GetNextToken()
        elif pad_type == lex.TokenType.write_box_keyword:
            self.GetNextToken()
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_1 = var_name
                    self.GetNextToken()
            else:
                pad_val_1 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_2 = var_name
                    #pad_val_2 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_2 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_3 = var_name
                    #pad_val_3 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_3 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_4 = var_name
                    #pad_val_3 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_4 = self.ParseExpression()
            
            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.identifier:
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                if var_name.lexeme in self.declared_vars.keys():
                    pad_val_5 = var_name
                    #pad_val_3 = ast.ASTIntegerNode(self.declared_var_vals[var_name.lexeme])
                    self.GetNextToken()
            else:
                pad_val_5 = self.ParseExpression()
            #self.GetNextToken()
        else:
            have_to_decl = False
            if (self.crtToken.type == lex.TokenType.let_keyword):
                #create AST node to store the identifier            
                #assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
                have_to_decl = True
                self.GetNextToken()
                #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)
            if (self.crtToken.type == lex.TokenType.identifier):
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.variableDecl):
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.datatype):
                var_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if have_to_decl:
                self.declared_vars[var_name.lexeme] = var_type.type

            if (self.crtToken.type == lex.TokenType.equals):
                #no need to do anything ... token can be discarded
                self.GetNextToken()
                #print("EQ Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

            if (self.crtToken.type == lex.TokenType.normalBracBegin):
                self.GetNextToken()
            
            #Next sequence of tokens should make up an expression ... therefore call ParseExpression that will return the subtree representing that expression
            if self.crtToken.type == lex.TokenType.random_keyword:
                pad_type = self.crtToken.type
                self.GetNextToken()
                pad_val = self.ParseExpression()
                if have_to_decl:
                    if not (isinstance(pad_val, ast.ASTWidthNode) or isinstance(pad_val, ast.ASTHeightNode)):
                        self.declared_var_vals[var_name.lexeme] = random.randint(0, int(pad_val.value))
                    else:
                        self.declared_var_vals[var_name.lexeme] = random.randint(0, 35)


            if (self.crtToken.type == lex.TokenType.normalBracEnd):
                self.GetNextToken()

        if pad_type == lex.TokenType.print_keyword:
            self.stmt_type.append(ast.ASTPrintNode(pad_val))
            return ast.ASTPrintNode(pad_val)
        elif pad_type == lex.TokenType.delay_keyword:
            self.stmt_type.append(ast.ASTDelayNode(pad_val))
            return ast.ASTDelayNode(pad_val)
        elif pad_type == lex.TokenType.clear_keyword:
            self.stmt_type.append(ast.ASTClearNode(pad_val))
            return ast.ASTClearNode(pad_val)
        elif pad_type == lex.TokenType.write_keyword:
            self.stmt_type.append(ast.ASTWriteNode(pad_val_3, pad_val_2, pad_val_1))
            return ast.ASTWriteNode(pad_val_3, pad_val_2, pad_val_1)
        elif pad_type == lex.TokenType.write_box_keyword:
            self.stmt_type.append(ast.ASTWriteBoxNode(pad_val_5, pad_val_4, pad_val_3, pad_val_2, pad_val_1))
            return ast.ASTWriteBoxNode(pad_val_5, pad_val_4, pad_val_3, pad_val_2, pad_val_1)
        elif pad_type == lex.TokenType.random_keyword:
            self.stmt_type.append(ast.ASTVarDeclNode(var_name, var_type, ast.ASTRandomNode(pad_val)))
            return ast.ASTVarDeclNode(var_name, var_type, ast.ASTRandomNode(pad_val))

    def ParseAssignment(self):
        #Assignment is made up of two main parts; the LHS (the variable) and RHS (the expression)
        if (self.crtToken.type == lex.TokenType.identifier):
            #create AST node to store the identifier            
            assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
            #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

        if (self.crtToken.type == lex.TokenType.equals):
            self.GetNextToken()

        #Next sequence of tokens should make up an expression
        #assignment_rhs = self.ParseExpression()
        
        vals_list = []
        op = ""
        var_val_2 = ""
        while (self.crtToken.type != lex.TokenType.semicolon):
            if (self.crtToken.type == lex.TokenType.normalBracBegin):
                self.GetNextToken()
        
            #Next sequence of tokens should make up an expression
            if (self.crtToken.type != lex.TokenType.identifier):
                var_val_1 = self.ParseExpression()
            else:
                self.PredictNextToken()
                if (self.nextToken.type == lex.TokenType.sqBracBegin):
                    var_val_1 = self.ParseArrayElement()
                else:
                    var_val_1 = ast.ASTVariableNode(self.crtToken.lexeme)
                    self.GetNextToken()

            vals_list.append(var_val_1)

            if (self.crtToken.type == lex.TokenType.semicolon):
                break

            if (self.crtToken.type == lex.TokenType.operator):
                op = ast.ASTOperatorNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if (self.crtToken.type != lex.TokenType.identifier):
                var_val_2 = self.ParseExpression()
            else:
                var_val_2 = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.normalBracEnd):
                self.GetNextToken()
            
            if op != "" and vals_list != "":
                vals_list.append(var_val_2)
                vals_list.append(op)
            
            break
        
        assignment_rhs = ""

        if len(vals_list) == 1:
            assignment_rhs = vals_list[0]
        else:
            assignment_rhs = ast.ASTModExpressionNode(vals_list[0], vals_list[2], vals_list[1])
        
        if (self.crtToken.type == lex.TokenType.normalBracEnd):
            self.GetNextToken()
        
        self.stmt_type.append(ast.ASTAssignmentNode(assignment_lhs, assignment_rhs))
                
        return ast.ASTAssignmentNode(assignment_lhs, assignment_rhs)
    
    def ParseAssignmentArray(self):    
        if (self.crtToken.type == lex.TokenType.identifier):
            var_name = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()

        if (self.crtToken.type == lex.TokenType.sqBracBegin):
            self.GetNextToken()
        
        access_index = False
        if (self.crtToken.type == lex.TokenType.integerType):
            index_val = self.ParseExpression()
            access_index = True
        
        if (self.crtToken.type == lex.TokenType.sqBracEnd):
            self.GetNextToken()
        else:
            print("Syntax Error: ] not present after [")

        if (self.crtToken.type == lex.TokenType.equals):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.sqBracBegin):
            self.GetNextToken()
        
        if access_index:
            if (self.crtToken.type == lex.TokenType.integerType) or (self.crtToken.type == lex.TokenType.boolean) or (self.crtToken.type == lex.TokenType.floatType) or (self.crtToken.type == lex.TokenType.colour):
                pass
        else:
            vals_list = []
            arr_len = 0
            while (self.crtToken.type != lex.TokenType.sqBracEnd):
            
                #Next sequence of tokens should make up an expression ... therefore call ParseExpression that will return the subtree representing that expression
                if (self.crtToken.type == lex.TokenType.integerType or self.crtToken.type == lex.TokenType.floatType or self.crtToken.type == lex.TokenType.boolean or self.crtToken.type == lex.TokenType.colour or self.crtToken.type == lex.TokenType.width_keyword or self.crtToken.type == lex.TokenType.height_keyword):
                    val = self.ParseExpression()
                    arr_len += 1

                vals_list.append(val)

                if (self.crtToken.type == lex.TokenType.comma):
                    self.GetNextToken()
            
            new_arr = ast.ASTArrayNode(vals_list, ast.ASTIntegerNode(arr_len))

            if (self.crtToken.type == lex.TokenType.sqBracEnd):
                self.GetNextToken()

        self.stmt_type.append(ast.ASTAssignmentArrayNode(var_name, new_arr))

        return ast.ASTAssignmentArrayNode(var_name, new_arr)
    
    def ParseVarDecl(self):
        #Assignment is made up of two main parts; the LHS (the variable) and RHS (the expression)
        if (self.crtToken.type == lex.TokenType.let_keyword):
            #create AST node to store the identifier            
            #assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
            #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)
        
        if (self.crtToken.type == lex.TokenType.identifier):
            var_name = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.variableDecl):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.datatype):
            var_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        self.declared_vars[var_name.lexeme] = var_type.type

        if (self.crtToken.type == lex.TokenType.equals):
            #no need to do anything ... token can be discarded
            self.GetNextToken()
            #print("EQ Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)
        
        vals_list = []
        op = ""
        var_val_2 = ""
        while (self.crtToken.type != lex.TokenType.semicolon):
            if (self.crtToken.type == lex.TokenType.normalBracBegin):
                self.GetNextToken()
        
            #Next sequence of tokens should make up an expression ... therefore call ParseExpression that will return the subtree representing that expression
            if (self.crtToken.type == lex.TokenType.integerType or self.crtToken.type == lex.TokenType.floatType or self.crtToken.type == lex.TokenType.boolean or self.crtToken.type == lex.TokenType.colour or self.crtToken.type == lex.TokenType.width_keyword or self.crtToken.type == lex.TokenType.height_keyword):
                var_val_1 = self.ParseExpression()
            elif (self.crtToken.type == lex.TokenType.identifier):
                self.PredictNextToken()
                if (self.nextToken.type == lex.TokenType.sqBracBegin):
                    var_val_1 = self.ParseArrayElement()

            vals_list.append(var_val_1)

            if (self.crtToken.type == lex.TokenType.semicolon):
                break

            if (self.crtToken.type == lex.TokenType.operator):
                op = ast.ASTOperatorNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.integerType or self.crtToken.type == lex.TokenType.floatType or self.crtToken.type == lex.TokenType.width_keyword or self.crtToken.type == lex.TokenType.height_keyword):
                var_val_2 = self.ParseExpression()
            else:
                print("Semantic Error: Cannot apply additive or multiplicative operations to boolean or colour values!")
            
            if (self.crtToken.type == lex.TokenType.normalBracEnd):
                self.GetNextToken()
            
            if op != "" and vals_list != "":
                vals_list.append(var_val_2)
                vals_list.append(op)
            
            break
        
        new_var = ""

        if len(vals_list) == 1:
            if not isinstance(vals_list[0], ast.ASTArrayElementNode):
                self.declared_var_vals[var_name.lexeme] = vals_list[0].value
            else:
                self.declared_var_vals[var_name.lexeme] = vals_list[0].arr_node.values[int(vals_list[0].index.value)].value

            if var_type.type == "int":
                #vals_list[0].value = int(vals_list[0].value)
                if not isinstance(vals_list[0], ast.ASTArrayElementNode):
                    if vals_list[0].value != "__width" and vals_list[0].value != "__height":
                        new_var = ast.ASTIntegerNode(str(vals_list[0].value))
                    elif vals_list[0].value == "__width":
                        new_var = ast.ASTWidthNode(str(vals_list[0].value))
                    elif vals_list[0].value == "__height":
                        new_var = ast.ASTHeightNode(str(vals_list[0].value))
                else:
                    #new_var = ast.ASTIntegerNode(str(vals_list[0].arr_node.values[int(vals_list[0].index.value)].value))
                    new_var = ast.ASTArrayElementNode(vals_list[0].index, vals_list[0].arr_node)
            elif var_type.type == "float":
                if not isinstance(vals_list[0], ast.ASTArrayElementNode):
                    new_var = ast.ASTFloatNode(str(vals_list[0].value))
                else:
                    new_var = ast.ASTFloatNode(str(vals_list[0].arr_node.values[int(vals_list[0].index.value)].value))
            elif var_type.type == "colour":
                if not isinstance(vals_list[0], ast.ASTArrayElementNode):
                    new_var = ast.ASTColourNode(str(vals_list[0].value))
                else:
                    new_var = ast.ASTColourNode(str(vals_list[0].arr_node.values[int(vals_list[0].index.value)].value))
            elif var_type.type == "bool":
                if not isinstance(vals_list[0], ast.ASTArrayElementNode):
                    new_var = ast.ASTBoolNode(str(vals_list[0].value))
                else:
                    new_var = ast.ASTBoolNode(str(vals_list[0].arr_node.values[int(vals_list[0].index.value)].value))
        else:
            new_var = ast.ASTModExpressionNode(vals_list[0], vals_list[2], vals_list[1])

        self.stmt_type.append(ast.ASTVarDeclNode(var_name, var_type, new_var))

        return ast.ASTVarDeclNode(var_name, var_type, new_var)
    
    def ParseVarDeclArray(self):
        if (self.crtToken.type == lex.TokenType.let_keyword):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.identifier):
            var_name = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.variableDecl):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.datatype):
            var_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        self.declared_vars[var_name.lexeme] = var_type.type

        if (self.crtToken.type == lex.TokenType.sqBracBegin):
            self.GetNextToken()
        
        if (self.crtToken.type == lex.TokenType.sqBracEnd):
            self.GetNextToken()
        else:
            print("Syntax Error: ] not present after [")

        if (self.crtToken.type == lex.TokenType.equals):
            #no need to do anything ... token can be discarded
            self.GetNextToken()
            #print("EQ Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)
        
        if (self.crtToken.type == lex.TokenType.sqBracBegin):
            self.GetNextToken()

        vals_list = []
        arr_len = 0
        #op = ""
        #var_val_2 = ""
        while (self.crtToken.type != lex.TokenType.sqBracEnd):
        
            #Next sequence of tokens should make up an expression ... therefore call ParseExpression that will return the subtree representing that expression
            if (self.crtToken.type == lex.TokenType.integerType or self.crtToken.type == lex.TokenType.floatType or self.crtToken.type == lex.TokenType.boolean or self.crtToken.type == lex.TokenType.colour or self.crtToken.type == lex.TokenType.width_keyword or self.crtToken.type == lex.TokenType.height_keyword):
                val = self.ParseExpression()
                arr_len += 1

            vals_list.append(val)

            if (self.crtToken.type == lex.TokenType.comma):
                self.GetNextToken()
        
        self.declared_var_vals[var_name.lexeme] = vals_list

        new_arr = ast.ASTArrayNode(vals_list, ast.ASTIntegerNode(arr_len))

        if (self.crtToken.type == lex.TokenType.sqBracEnd):
            self.GetNextToken()

        self.stmt_type.append(ast.ASTVarDeclArrayNode(var_name, var_type, new_arr))

        return ast.ASTVarDeclArrayNode(var_name, var_type, new_arr)
    
    def ParseIfStatement(self):
        self.GetNextToken()  # Move past 'if' keyword

        if (self.crtToken.type == lex.TokenType.normalBracBegin):
            self.GetNextToken()
        
        cond_lhs = None

        if (self.crtToken.type == lex.TokenType.identifier):
            #create AST node to store the identifier            
            cond_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        else:
            cond_lhs = self.ParseExpression()
        
        if (self.crtToken.type == lex.TokenType.relOperator):
            comp_op = ast.ASTBinaryOpNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        cond_rhs = None

        if (self.crtToken.type != lex.TokenType.identifier):
            cond_rhs = self.ParseExpression()
        else:
            cond_rhs = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()

        if (self.crtToken.type == lex.TokenType.normalBracEnd):
            self.GetNextToken()
        
        condition = ast.ASTConditionNode(cond_lhs, comp_op, cond_rhs)

        if (self.crtToken.type == lex.TokenType.curlyBracBegin):
            self.GetNextToken()
        

        #self.GetNextToken()  # Move past 'then' keyword
        if_body = self.ParseBlock()
        #if (self.crtToken.type == lex.TokenType.curlyBracEnd):
        #self.GetNextToken()

        self.PredictNextToken()

        if (self.nextToken.type == lex.TokenType.else_keyword):
            if self.crtToken.type == lex.TokenType.curlyBracEnd:
                self.GetNextToken()


        #if self.crtToken.type == lex.TokenType.else_keyword:
            self.GetNextToken()  # Move past 'else' keyword
            if (self.crtToken.type == lex.TokenType.curlyBracBegin):
                self.GetNextToken()
            
            else_body = self.ParseBlock()

            #if self.crtToken.type == lex.TokenType.curlyBracEnd:
            #    self.GetNextToken()
            
            self.if_count += 1
            self.stmt_type.append(ast.ASTIfElseNode(condition, if_body, else_body))
            return ast.ASTIfElseNode(condition, if_body, else_body)
        else:
            self.if_count += 1
            self.stmt_type.append(ast.ASTIfNode(condition, if_body))
            return ast.ASTIfNode(condition, if_body)
    
    def ParseFunction(self):
        self.GetNextToken()  # Move past 'fun' keyword

        if self.crtToken.type == lex.TokenType.identifier:
            func_name = ast.ASTVariableNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        if self.crtToken.type != lex.TokenType.normalBracBegin:
            print("Syntax Error - Character \'(\' does not exist!")
        else:
            self.GetNextToken()
        
        param_list = {}
        while self.crtToken.type != lex.TokenType.normalBracEnd:
            if self.crtToken.type == lex.TokenType.identifier:
                param_name = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.variableDecl:
                self.GetNextToken()
            
            if self.crtToken.type == lex.TokenType.datatype:
                param_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            param_list[param_name] = param_type

            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
        

        if self.crtToken.type != lex.TokenType.normalBracEnd:
            print("Syntax Error - Character \')\' does not exist!")
        else:
            self.GetNextToken()
        
        if self.crtToken.type != lex.TokenType.arrow:
            print("Syntax Error - Type arrow \'->\' does not exist!")
        else:
            self.GetNextToken()
        
        if self.crtToken.type != lex.TokenType.datatype:
            print("Syntax Error - Datatype does not exist!")
        else:
            func_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
            self.GetNextToken()
        
        self.declared_functions[func_name.lexeme] = func_type.type
        self.declared_functions_params[func_name.lexeme] = param_list
        
        if param_list == {}:
            func_decl = ast.ASTFunctionDeclNode(func_name, func_type)
        else:
            func_decl = ast.ASTFunctionDeclNodeWithParams(func_name, func_type, param_list)

        if (self.crtToken.type == lex.TokenType.curlyBracBegin):
            self.GetNextToken()
        

        if (self.crtToken.type == lex.TokenType.curlyBracEnd):
            self.GetNextToken()


        func_body = self.ParseBlock()

        #self.GetNextToken()
        self.stmt_type.append(ast.ASTFunctionNode(func_decl, func_body))
        return ast.ASTFunctionNode(func_decl, func_body)
    
    def ParseFunctionCall(self):
        have_to_decl = False
        if (self.crtToken.type == lex.TokenType.let_keyword):
            #create AST node to store the identifier            
            #assignment_lhs = ast.ASTVariableNode(self.crtToken.lexeme)
            have_to_decl = True
            self.GetNextToken()
            #print("Variable Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)
        if have_to_decl:
            if (self.crtToken.type == lex.TokenType.identifier):
                var_name = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.variableDecl):
                self.GetNextToken()
            
            if (self.crtToken.type == lex.TokenType.datatype):
                var_type = ast.ASTDataTypeNode(self.crtToken.lexeme)
                self.GetNextToken()
        
        
            self.declared_vars[var_name.lexeme] = var_type.type

            if (self.crtToken.type == lex.TokenType.equals):
                #no need to do anything ... token can be discarded
                self.GetNextToken()
                #print("EQ Token Matched ::: Nxt Token is ", self.crtToken.type, self.crtToken.lexeme)

        if self.crtToken.type == lex.TokenType.identifier:
            if self.crtToken.lexeme in self.declared_functions.keys():
                func_name = ast.ASTVariableNode(self.crtToken.lexeme)
            else:
                print("Semantic Error: Function not declared!")
            self.GetNextToken()
        
        if self.crtToken.type != lex.TokenType.normalBracBegin:
            print("Syntax Error - Character \'(\' does not exist!")
        else:
            self.GetNextToken()
        
        param_list = {}
        while self.crtToken.type != lex.TokenType.normalBracEnd:
            if self.crtToken.type == lex.TokenType.identifier:
                param_name = ast.ASTVariableNode(self.crtToken.lexeme)
                self.GetNextToken()
            
            #param_list[param_name] = param_type

            if self.crtToken.type == lex.TokenType.comma:
                self.GetNextToken()
        
        if self.crtToken.type != lex.TokenType.normalBracEnd:
            print("Syntax Error - Character \')\' does not exist!")
        else:
            self.GetNextToken()
        
        self.stmt_type.append(ast.ASTFunctionCallNode(func_name))
        return ast.ASTFunctionCallNode(func_name)
    
        #self.GetNextToken()
        #self.stmt_type.append(ast.ASTFunctionNode(func_decl, func_body))
        #return ast.ASTFunctionNode(func_decl, func_body)
            
    def ParseStatement(self):
        #At the moment we only have assignment statements .... you'll need to add more for the assignment - branching depends on the token type
        if self.crtToken.type == lex.TokenType.identifier:
            if "[" not in self.stmt_list[self.stmt_index]:
                return self.ParseAssignment()
            else:
                return self.ParseAssignmentArray()
        elif (self.crtToken.type == lex.TokenType.let_keyword and "__random_int" not in self.stmt_list[self.stmt_index]) and (self.crtToken.type == lex.TokenType.let_keyword and "[]" not in self.stmt_list[self.stmt_index]) :
            return self.ParseVarDecl()
        elif (self.crtToken.type == lex.TokenType.let_keyword and [key for key in self.declared_functions.keys() if key in self.stmt_list[self.stmt_index]]):
            return self.ParseFunctionCall()
        elif (self.crtToken.type == lex.TokenType.let_keyword and "[]" in self.stmt_list[self.stmt_index]):
            return self.ParseVarDeclArray()
        elif self.crtToken.type == lex.TokenType.if_keyword:
            return self.ParseIfStatement()
        elif self.crtToken.type == lex.TokenType.fun_keyword:
            return self.ParseFunction()
        elif self.crtToken.type == lex.TokenType.print_keyword or self.crtToken.type == lex.TokenType.delay_keyword or self.crtToken.type == lex.TokenType.write_keyword or self.crtToken.type == lex.TokenType.write_box_keyword or self.crtToken.type == lex.TokenType.clear_keyword or (self.crtToken.type == lex.TokenType.let_keyword and "__random_int" in self.stmt_list[self.stmt_index]):
            return self.ParsePadFunction()
        elif self.crtToken.type == lex.TokenType.ret_keyword:
            return self.ParseReturn()
        elif self.crtToken.type == lex.TokenType.for_keyword:
            return self.ParseForStatement()
        elif self.crtToken.type == lex.TokenType.while_keyword:
            return self.ParseWhileStatement()
        else:
            print("Syntax Error: Unexpected token ", self.crtToken.type, self.crtToken.lexeme)
            return None


    def ParseBlock(self):
        #At the moment we only have assignment statements .... you'll need to add more for the assignment - branching depends on the token type

        block = ast.ASTBlockNode()
        running = True
        while (self.crtToken.type != lex.TokenType.end):# and self.if_count < self.max_if_count):
            if self.crtToken.type == lex.TokenType.curlyBracEnd or self.crtToken.type == lex.TokenType.void:
                break
        #while (self.crtToken.type != lex.TokenType.end and self.crtToken.type != lex.TokenType.curlyBracEnd):
            while (self.crtToken.type != lex.TokenType.end and self.crtToken.type != lex.TokenType.curlyBracEnd):
                #print("New Statement - Processing Initial Token:: ", self.crtToken.type, self.crtToken.lexeme)
                s = self.ParseStatement()
                block.add_statement(s)
                self.stmt_index+=1
                if (self.crtToken.type == lex.TokenType.semicolon or self.crtToken.type == lex.TokenType.curlyBracEnd):
                    self.GetNextToken()
                elif (s == None or self.crtToken.type == lex.TokenType.end):
                    break
                else:
                    print("Syntax Error - No Semicolon separating statements in block")
                    break
            
        if (self.crtToken.type == lex.TokenType.end):
            s = self.ParseEndToken()
            block.add_statement(s)

        return block

    def ParseProgram(self):                        
        self.GetNextToken()  #set crtToken to the first token (skip all WS)
        b = self.ParseBlock()        
        return b        

    def Parse(self):        
        self.ASTroot = self.ParseProgram()


#parser = Parser("x=23;")
test_string_1 = "   let  x:int=   23 ; let y : int=  100; let z:float = 2.3 ; let c1:colour = #00ff00; let cond:bool= true; if (x != z){z = 2.4;} let x_n:float = 1.1045;"
test_string_1_a = "   let  x:int=   23 ; let y : int=  100; let z:float = 2.3 ; let c1:colour = #00ff00; let cond:bool= true; __print x; __print y; __print z; __print c1; __print cond;"
test_string_2 = "let c:colour = (__random_int 16777216); let x_1:int = (__random_int 35); __print 36; let x_2:int = (__random_int __width); let y: int = (__random_int __height);  __write x_2, y, c;"
test_string_2_loop = "for (let counter:int = 0; counter < 10; counter = counter + 1){ let c:colour = (__random_int 16777216); let x_1:int = (__random_int 35); __print 36; let x_2:int = (__random_int __width); let y: int = (__random_int __height);  __write x_2, y, c; }"
test_string_2_while = "let counter:int = 0; while ( counter < 10){ let c:colour = (__random_int 16777216); let x_1:int = (__random_int 35); __print 36; let x_2:int = (__random_int __width); let y: int = (__random_int __height);  __write x_2, y, c; counter = counter + 1; }"
test_string_3 = "let c:colour = (__random_int 16777216); let cond:bool = true; if (cond == true){ __clear c;} else{ __clear 0;}"
test_string_4 = "for (let counter:int = 0; counter < 10; counter = counter + 1){ __print counter; }"
test_string_4_while = " let counter:int = 0; while (counter < 10) { __print counter; counter = counter + 1; }"
test_string_5 = "for (let x:int = 0; x < __width; x = x + 1){ __write x, 5, #0000ff; __delay 100; }"
test_string_5_a = "for (let y:int = 0; y < __height; y = y + 1){ __write 5, y, #0000ff; __delay 100; }"
test_string_5_while = "let x:int = 0; while (x < __width){ __write x, 5, #0000ff; __delay 100; x = x + 1; }"
test_string_6 = "__write_box 10, 10, 4, 3, #00ff00;"
test_string_6_a = "let w:int = 12; let h:int = 15; let y:int = 5; let x:int = 3; __write_box x, y, w, h, #ff0000;"
test_string_7 = "let a:int [] = [10, 20, 30, 40, 50, 60, 70, 80, 90]; __print a;"
test_string_7_a = "let a:int [] = [10, 20, 30, 40, 50, 60, 70, 80, 90]; __print a[3];"
test_string_7_b = "let a:int [] = [10, 20, 30, 40, 50, 60, 70, 80, 90]; let m:int = a[3]; __print m;"
test_string_8 = "let i:int = 5; let ans:int = 1; while (i > 1){ ans = ans * i; i = i - 1; } __print ans;"
parser = Parser(test_string_7)
parser.Parse()

in_sub_block = False
for i in range(len(parser.stmt_list)):
    if "{" in parser.stmt_list[i]:
        in_sub_block = True
    
    if "}" in parser.stmt_list[i]:
        in_sub_block = False

    if "let" in parser.stmt_list[i]: #and not in_sub_block:
        parser.let_count += 1

for i, stmt in enumerate(parser.stmt_type):
    if isinstance(stmt, ast.ASTVarDeclArrayNode):
        for val in stmt.expr.values:
            parser.let_count += 1
    
index_to_rm = []
for i, stmt in enumerate(parser.stmt_type):
    if isinstance(stmt, ast.ASTIfNode):
        for j, blockStmt in enumerate(stmt.if_body.stmts):
            index_to_rm.append(i-j-1)
    elif isinstance(stmt, ast.ASTIfElseNode):
        #pass
        for k, blockStmt in enumerate(stmt.else_body.stmts):
            index_to_rm.append(i-k-1)

        for j, blockStmt in enumerate(stmt.if_body.stmts):
            index_to_rm.append(i-j-2)
    elif isinstance(stmt, ast.ASTForNode):
        for j, blockStmt in enumerate(stmt.for_body.stmts):
            index_to_rm.append(i-j-1)

        for k in range(2):
            index_to_rm.append(0)
    elif isinstance(stmt, ast.ASTWhileNode):
        for j, blockStmt in enumerate(stmt.while_body.stmts):
            index_to_rm.append(i-j-1)
    elif isinstance(stmt, ast.ASTFunctionNode):
        pass

new_index_list = sorted(index_to_rm, reverse=True)
print(new_index_list)

for index in new_index_list:
    parser.stmt_type.pop(index)

print_visitor = vis.PrintNodesVisitor()
parser.ASTroot.accept(print_visitor)

semantic_checker = vis.SemanticAnalyser()
print("\nSemantic Analysis Check:")

for i in range(len(parser.stmt_type)):
    semantic_checker.check_semantics(parser.stmt_type[i])

if semantic_checker.error_count == 0:
    print("No Semantic Errors detected!")

code_generator = vis.PArIRCodeGenerator()

print("\nParIR Code from Parser:")
if parser.let_count > 0:
    code_generator.intialise_vars(parser.let_count)
for i in range(len(parser.stmt_type)):
    parser_code = code_generator.generate_code(parser.stmt_type[i])
print(parser_code)